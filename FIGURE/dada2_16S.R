
## load packages
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("dada2")
require("dada2"); packageVersion("dada2")
library(ShortRead); packageVersion("ShortRead")
library(Biostrings); packageVersion("Biostrings")
library(stringr)



## Construct Paths
work_dir <- "/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/16S_50-861646108/"
raw_dir <- file.path(work_dir,"raw_unpacked") # CHANGE ME to the directory containing the fastq files after unzipping.

##Note: Elena's files removed in this folder! only continue with the FIGURE samples. 
preFilt_dir <- file.path(work_dir,"preFilt")
primerCut5_dir <- file.path(work_dir,"primerCut5")
primerCut3_dir <- file.path(work_dir,"primerCut3")
qualFiltTrim_dir <- file.path(work_dir,"qualFiltTrim")

## load files

list.files(raw_dir)
# Forward and reverse fastq filenames have format: SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs.raw <- sort(list.files(raw_dir, pattern="_R1_001.fastq", full.names = TRUE))
fnRs.raw <- sort(list.files(raw_dir, pattern="_R2_001.fastq", full.names = TRUE))
# Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
sample.names <- sapply(strsplit(basename(fnFs.raw), "_"), `[`, 1)

## Construct needed file list

fnFs.preFilt <- file.path(preFilt_dir,basename(fnFs.raw))
fnRs.preFilt <- file.path(preFilt_dir,basename(fnRs.raw))
fnFs.primerCut5 <- file.path(primerCut5_dir,basename(fnFs.raw))
fnRs.primerCut5 <- file.path(primerCut5_dir,basename(fnRs.raw))
fnFs.primerCut3 <- file.path(primerCut3_dir,basename(fnFs.raw))
fnRs.primerCut3 <- file.path(primerCut3_dir,basename(fnRs.raw))
fnFs.qualFiltTrim <- file.path(qualFiltTrim_dir,basename(fnFs.raw))
fnRs.qualFiltTrim <- file.path(qualFiltTrim_dir,basename(fnRs.raw))

#Qulaity check

plotQualityProfile(fnFs.raw[1:4])
plotQualityProfile(fnRs.raw[1:4])


#PREFILTERING
filterAndTrim(fnFs.raw,fnFs.preFilt,fnRs.raw,fnRs.preFilt,truncQ=2,minQ=2,minLen=50,maxN=0,multithread = FALSE)


#Removing the primers

#IDENTIFY PRIMER(file location: https://docs.google.com/spreadsheets/d/1dXn0BZCGKcrjlYYcSQAqQNV8wHRGfWal/edit?usp=sharing&ouid=101362427511031839862&rtpof=true&sd=true)
FWD_PRIMER="GTGYCAGCMGCCGCGGTAA" #515F Parada
REV_PRIMER="GGACTACNVGGGTWTCTAAT" #806R Apprill

allOrients <- function(primer) {
  # Create all orientations of the input sequence
  require(Biostrings)
  dna <- DNAString(primer)  # The Biostrings works w/ DNAString objects rather than character vectors
  orients <- c(Forward = dna, Complement = complement(dna), Reverse = reverse(dna),
               RevComp = reverseComplement(dna))
  return(sapply(orients, toString))
}

FWD_PRIMER.orients <- allOrients(FWD_PRIMER)
REV_PRIMER.orients <- allOrients(REV_PRIMER)

FWD_PRIMER.orients
REV_PRIMER.orients

primerHits <- function(primer, fn) {
  # Counts number of reads in which the primer is found
  nhits <- vcountPattern(primer, sread(readFastq(fn)), fixed = FALSE)
  return(sum(nhits > 0))
}
#check one sample
rbind(FWD_PRIMER.ForwardReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnFs.preFilt[[1]]),
      FWD_PRIMER.ReverseReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnRs.preFilt[[1]]),
      REV_PRIMER.ForwardReads = sapply(REV_PRIMER.orients, primerHits, fn = fnFs.preFilt[[1]]),
      REV_PRIMER.ReverseReads = sapply(REV_PRIMER.orients, primerHits, fn = fnRs.preFilt[[1]]))


#REMOVE PRIMERS

#cutadapt available?
cutadapt <- "/Users/corahoerstmann/opt/anaconda3/bin/cutadapt" # CHANGE ME to the cutadapt path on your machine
system2(cutadapt, args = "--version")

#create output dirs
if(!dir.exists(primerCut5_dir)) dir.create(primerCut5_dir)
if(!dir.exists(primerCut3_dir)) dir.create(primerCut3_dir)

FWD_PRIMER.RC <- dada2:::rc(FWD_PRIMER)
REV_PRIMER.RC <- dada2:::rc(REV_PRIMER)

# Run Cutadapt both primer at 5' end
for(i in seq_along(fnFs.preFilt)) {
  system2(cutadapt, args = c("-g", paste("\"",FWD_PRIMER,";min_overlap=15;max_error_rate=0.15","\"",sep=""),
                             "-G", paste("\"",REV_PRIMER,";min_overlap=15;max_error_rate=0.15","\"",sep=""),
                             "--discard-untrimmed", "-o", fnFs.primerCut5[i], "-p", fnRs.primerCut5[i],
                             fnFs.preFilt[i], fnRs.preFilt[i]))
  
}

#check one sample
rbind(FWD_PRIMER.ForwardReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnFs.primerCut5[[1]]),
      FWD_PRIMER.ReverseReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnRs.primerCut5[[1]]),
      REV_PRIMER.ForwardReads = sapply(REV_PRIMER.orients, primerHits, fn = fnFs.primerCut5[[1]]),
      REV_PRIMER.ReverseReads = sapply(REV_PRIMER.orients, primerHits, fn = fnRs.primerCut5[[1]]))

# Run Cutadapt both primer at 3' end
for(i in seq_along(fnFs.primerCut5)) {
  system2(cutadapt, args = c("-a", paste("\"",REV_PRIMER.RC,";min_overlap=15;max_error_rate=0.15","\"",sep=""),
                             "-A", paste("\"",FWD_PRIMER.RC,";min_overlap=15;max_error_rate=0.15","\"",sep=""),
                             "-o", fnFs.primerCut3[i], "-p", fnRs.primerCut3[i],
                             fnFs.primerCut5[i], fnRs.primerCut5[i]))
  
}


rbind(FWD_PRIMER.ForwardReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnFs.primerCut3[[1]]),
      FWD_PRIMER.ReverseReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnRs.primerCut3[[1]]),
      REV_PRIMER.ForwardReads = sapply(REV_PRIMER.orients, primerHits, fn = fnFs.primerCut3[[1]]),
      REV_PRIMER.ReverseReads = sapply(REV_PRIMER.orients, primerHits, fn = fnRs.primerCut3[[1]]))

# Quality filter

filterOut <- filterAndTrim(fnFs.primerCut3,fnFs.qualFiltTrim,
                           fnRs.primerCut3,fnRs.qualFiltTrim, truncLen=c(200,180),
                     maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,
                     compress=TRUE, multithread=FALSE)
#summary, to see how many reads were removed by filtereing
print(filterOut)

#DE-REPLICATE and keep going only with existing files

exists <- file.exists(fnFs.qualFiltTrim)
fnFs.deRep <- derepFastq(fnFs.qualFiltTrim[exists], verbose=TRUE)
fnRs.deRep <- derepFastq(fnRs.qualFiltTrim[exists], verbose=TRUE)
names(fnFs.deRep) <- sample.names[exists]
names(fnRs.deRep) <- sample.names[exists]


#LEARN ERRORS AND PLOT

errF <- learnErrors(fnFs.deRep, multithread=TRUE,randomize=TRUE, nbases = 1e8)
errR <- learnErrors(fnRs.deRep, multithread=TRUE,randomize=TRUE, nbases = 1e8)


#Samle inferences
#### identify how many unique samples we have in each sample

dadaFs <- dada(fnFs.deRep,err=errF, multithread=TRUE)
dadaRs <- dada(fnRs.deRep, err=errR, multithread=TRUE)
dadaFs[1]
dadaRs[1]

#merge paired end reads

mergers <- mergePairs(dadaFs, fnFs.deRep, dadaRs, fnRs.deRep, verbose=TRUE)
# Inspect the merger data.frame from the first sample
head(mergers[[1]])

#CONSTRUCT SEQUENCE TABLE
seqtab <- makeSequenceTable(mergers)
dim(seqtab)
# Inspect distribution of sequence lengths
table(nchar(getSequences(seqtab)))


#REMOVE CHIMERAS AND EXPORT ASV TABLE
seqtab.nochim <- removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=TRUE)
dim(seqtab.nochim)
write.csv(t(seqtab.nochim), "/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/nifH_20230410/seqtab_all_FIGURE_16S.csv", quote=FALSE ) # CHANGE ME to output directory.
saveRDS(filterOut, file = "/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/nifH_20230410/seqtab_nochim_FIGURE_16S.RDA") # CHANGE ME to output directory.
#obtain the frequencies of chimeras in the dataset
sum(seqtab.nochim)/sum(seqtab)


#TRACK READS
getN <- function(x) sum(getUniques(x))
#bind columns with same length
track1 <- cbind(sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
rownames(filterOut) <- sample.names
#merge with columns with different length and add NA when there is no value because there were no output sequences from filtering
track_reads <-merge (filterOut, track1, by = 0, all = TRUE)
colnames(track_reads) <- c("sample", "input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
#change NAs by zeros and add rownames
track_reads[is.na(track_reads)] <- 0
rownames(track_reads) <- track_reads$sample
track_reads <- track_reads[,-1]

write.table(track_reads, "/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/nifH_20230410/tracked_reads_FIGURE_16S.txt") # CHANGE ME to output directory.


#TAXONOMY
#with silva 138.1 for DADA2 https://doi.org/10.5281/zenodo.4587946

##In case of re-doing the annotation the sequence table can be imported and modified from the import file
seqtab.nochim <- (ASV16S)
seqtab.nochim <- t(seqtab.nochim)
refsr <- readFasta("/Users/corahoerstmann/Documents/MIO_general_Mastderdocs/silva_species_assignment_v138.1.fasta")
taxa <- assignTaxonomy(seqtab.nochim, "/Users/corahoerstmann/Documents/MIO_general_Mastderdocs/silva_nr99_v138.1_wSpecies_train_set.fa" , multithread=TRUE, minBoot = 0)
taxa <- as.data.frame(taxa)
#Lets look at the taxonomy
taxa.print <- taxa # Removing sequence rownames for display only
rownames(taxa.print) <- NULL
head(taxa.print)
#write table
write.table(taxa, "/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/16S_50-861646108/taxonomy_FIGURE_16S_SILVAv138.1.csv", sep = ";", quote=F, col.names=NA)

