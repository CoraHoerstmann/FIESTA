#load packages
require("phyloseq"); packageVersion("phyloseq")
require(ggbiplot)
require(vegan)
require("wesanderson"); packageVersion("wesanderson")
require("fantaxtic"); packageVersion("fantaxtic")
require("ggords")

source("/Users/choerstm/Documents/MIO_FIGURE/scripts/RDA_function.R")

nifH_meta$Main_Bin <- paste(nifH_meta$Pysical_Classification, nifH_meta$Desired_Depth..m., sep= "_")
nifH_meta$Main_Bin <- factor(nifH_meta$Main_Bin, levels = unique(nifH_meta$Main_Bin[order(nifH_meta$Int_Order)]))

##only bigger physical structures
nifH_meta_AC <- nifH_meta%>%filter(str_detect(Pysical_Classification, "AC"))%>% 
  cbind(broad_Structure = paste0("AC"))

nifH_meta_C <- nifH_meta%>%filter(str_detect(Pysical_Classification, " C"))%>% 
  cbind(broad_Structure = paste0("C"))

nifH_meta_GS_C <- nifH_meta_C%>%filter(str_detect(Pysical_Classification, "GS/"))

nifH_meta_GS_C$broad_Structure <- "GS/C"

nifH_meta_C <- nifH_meta_C%>%filter(!str_detect(Pysical_Classification, "GS/"))

nifH_meta_GS <- nifH_meta%>%filter(str_detect(Pysical_Classification, "GS"))%>% 
  cbind(broad_Structure = paste0("GS"))

nifH_meta_GS <- nifH_meta_GS%>%filter(!str_detect(Pysical_Classification, "GS/"))

nifH_meta_OO <- nifH_meta%>%filter(str_detect(Pysical_Classification, "open"))%>% 
  cbind(broad_Structure = paste0("open_ocean"))

nifH_meta <- rbind(nifH_meta_OO, nifH_meta_AC, nifH_meta_C, nifH_meta_GS_C, nifH_meta_GS)
rm(nifH_meta_OO, nifH_meta_AC, nifH_meta_C, nifH_meta_GS_C, nifH_meta_GS)

###



#make a phyloseq object

OTU = otu_table(nifH_ASV_CLR, taxa_are_rows = TRUE)
rownames(nifH_taxonomy_CLR) <- nifH_taxonomy_CLR$X
nifH_taxonomy_CLR$X <- NULL

nifH_taxonomy_CLR$Genus[is.na(nifH_taxonomy_CLR$Genus)] <- "g__"
nifH_taxonomy_CLR$Family[is.na(nifH_taxonomy_CLR$Family)] <- "F__"



tax.matrix<- as.matrix(nifH_taxonomy_CLR)
TAX = tax_table(tax.matrix)

nifH_meta$Main_Bin <- paste(nifH_meta$Pysical_Classification, nifH_meta$Desired_Depth..m., sep= "_")
nifH_meta$Main_Bin <- factor(nifH_meta$Main_Bin, levels = unique(nifH_meta$Main_Bin[order(nifH_meta$Int_Order)]))

rownames(nifH_meta) <- nifH_meta$No
map = sample_data(nifH_meta)
rownames(map) <- map$No
phyloseq_merged = phyloseq(OTU, TAX)
all = merge_phyloseq(phyloseq_merged, map) ####merge data into phyloseq
all

###
# Agglomerate to order level

all_T6 <- all  ##phyloseq::tax_glom(all, taxrank = "Species")
taxtab <- all_T6@tax_table@.Data

miss_f <- which(taxtab[, "Genus"] == "g__")
miss_g <- which(taxtab[, "Family"] == "F__")

# Number unspecified genera
taxtab[miss_f, "Genus"] <- paste0("g__", 1:length(miss_f))
taxtab[miss_g, "Family"] <- paste0("F__", 1:length(miss_g))

# Find duplicate genera
dupl_g <- which(duplicated(taxtab[, "Genus"]) |
                  duplicated(taxtab[, "Genus"], fromLast = TRUE))

for(i in seq_along(taxtab)){
  # The next higher non-missing rank is assigned to unspecified genera
  if(i %in% miss_f && i %in% miss_g){
    taxtab[i, "Genus"] <- paste0(taxtab[i, "Genus"], "(", taxtab[i, "Order"], ")")
  } else if(i %in% miss_f){
    taxtab[i, "Genus"] <- paste0(taxtab[i, "Genus"], "(", taxtab[i, "Family"], ")")
  }
  
  # Family names are added to duplicate genera
  if(i %in% dupl_g){
    taxtab[i, "Genus"] <- paste0(taxtab[i, "Genus"], "(", taxtab[i, "Genus"], ")")
  }
}

all_T6@tax_table@.Data <- taxtab


###


pal <- wes_palette("Zissou1", 21, type = "continuous")

#east edge AC"#370eaa"
#center AC"#0f3cf7"
#west edge AC"#a50a9a" 
#east edge C "#f45a45"
#center C "#ee9a00"
#west edge C "#f9d606"
#GS"#3f560d"
#outside GS"#4af496" 


##
set.seed(66)
all@sam_data$Depth_B <- as.factor(all@sam_data$Desired_Depth..m.)
out.pcoa.logt <- ordinate(all, method = "RDA", distance = "euclidean")
evals <- out.pcoa.logt$CA$eig
p3<-plot_ordination(all, out.pcoa.logt, type = "Sample",color= "Pysical_Classification") #"
#p3$layers <- p3$layers[-1]
p3+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.9),
        axis.text.y = element_text(size=16, vjust = 0.9),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.7))+
  theme(legend.key=element_blank())+
  geom_point(aes(size = Desired_Depth..m.), stroke =2)+
  scale_color_manual(values = c("#0f3cf7","#ee9a00","#370eaa","#f45a45", "#3f560d","#58d805","#4af496","#0ac6f2",
                                "#a50a9a" , "#f9d606", "#499b6e"))



###only CTD samples
nifHM_CTD$Sample <- as.character(nifHM_CTD$No)
nifH_ASV_CLR_CTD <- nifH_ASV_CLR%>%dplyr::select(nifHM_CTD$Sample)
OTU = otu_table(nifH_ASV_CLR_CTD, taxa_are_rows = TRUE)
nifH_tax_CTD <- as.data.frame(taxtab)
tax.matrix_CTD<- nifH_tax_CTD%>%filter(rownames(nifH_tax_CTD) %in% rownames(nifH_ASV_CLR_CTD))
tax.matrix_CTD<- as.matrix(tax.matrix_CTD)
TAX = tax_table(tax.matrix_CTD)

rownames(nifHM_CTD) <- nifHM_CTD$No
nifHM_CTD <- left_join(nifHM_CTD, nifH_meta[,c(2,20:22)], by = "No")


map = sample_data(nifHM_CTD)
rownames(map) <- map$No
phyloseq_merged = phyloseq(OTU, TAX)
all_CTD = merge_phyloseq(phyloseq_merged, map) ####merge data into phyloseq
all_CTD

##
nifH_ASV_CLR_CTD$Tax <- rownames(nifH_ASV_CLR_CTD)
tax_named_CTD <- as.data.frame(taxtab)

tax_named_CTD$Tax <- rownames(tax_named_CTD)
ALL_CTD <- left_join(nifH_ASV_CLR_CTD, tax_named_CTD[,6:7], by = "Tax")
ALL_CTD$Sp <- paste0(1:282,ALL_CTD$Genus)
rownames(ALL_CTD) <- ALL_CTD$Sp
ALL_CTD$Sp <- NULL
ALL_CTD$Genus <- NULL
ALL_CTD$Tax <- NULL

## relative abundances/ for more intituive reading of the plot scale the data

ASV_nifH_clr_norm <- apply(nifH_ASV_CLR, 2, function(x) x - min(x[x < 0]))

ASV_nifH_clr_norm_P <- apply(ASV_nifH_clr_norm, 2, function(x) {x/sum(x)})

ASV_nifH_clr_norm_P <- as.matrix(ASV_nifH_clr_norm_P)

## Turn into other phyloseq object

OTU = otu_table(ASV_nifH_clr_norm_P, taxa_are_rows = TRUE)
tax.matrix<- as.matrix(nifH_taxonomy_CLR)
TAX = tax_table(tax.matrix)
rownames(nifH_meta) <- nifH_meta$No
map = sample_data(nifH_meta)
rownames(map) <- map$No
phyloseq_merged = phyloseq(OTU, TAX)
all2 = merge_phyloseq(phyloseq_merged, map) ####merge data into phyloseq
all2

all3 <- psmelt(all2)

d <- ggplot(all3, aes(x = Sample, y = Abundance, color = Order, fill = Order))+
  geom_bar(stat="identity", position="stack")+
  #scale_fill_manual(values= c("#60E27B","#99BC08", "#9324EA", "#A3610A", "#824B04","#3F2707", "grey"))+
  #scale_color_manual(values= c("#60E27B", "#99BC08","#9324EA", "#A3610A", "#824B04","#3F2707","grey"))+
  theme(axis.text.x = element_text(size=4, vjust = 0.3, angle = 90))+
  facet_wrap(~Pysical_Classification + Desired_Depth..m. ,
             scales = "free_x")
print(d)

##heatmap

##Agglomerate at a level
all_Order <- tax_glom(all2, taxrank = "Order")
p <- plot_heatmap(all_Order, "NMDS", "bray", "Pysical_Classification", "Order", sample.order = "Int_Order")
p
f <- ggplot(all3, aes(x = Sample, y = Abundance, color = Kingdom, fill = Kingdom))+
  geom_bar(stat="identity", position="stack")+
  #scale_fill_manual(values= c("#60E27B","#99BC08", "#9324EA", "#A3610A", "#824B04","#3F2707", "grey"))+
  #scale_color_manual(values= c("#60E27B", "#99BC08","#9324EA", "#A3610A", "#824B04","#3F2707","grey"))+
  theme(axis.text.x = element_text(size=4, vjust = 0.3, angle = 90))+
  facet_wrap(~Pysical_Classification + Desired_Depth..m. ,
             scales = "free_x")
print(f)

all3$taxid <- all3$OTU
all3$abundance <- all3$Abundance
## top 10 most important/ abundant 
all_TSpec <- tax_glom(all2, taxrank = "Genus")
top <- fantaxtic::top_taxa(all_TSpec, 
                           tax_level = "Genus", 
                           n_taxa = 10,
                           grouping = "Main_Bin")

top_taxa <- top$top_taxa

print(top_taxa)

library(RColorBrewer)
set.seed(2023)
n <- 36
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
colors_order <- sample(col_vector, n)

e <- ggplot(top_taxa, aes(x = Main_Bin, y = abundance, color = Order, fill = Order))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_manual(values= colors_order)+
  scale_color_manual(values= colors_order)+
  theme(axis.text.x = element_text(size=10, vjust = 0.3, angle = 90))

print(e)

###RDA

nifH_ASV_CTD <- nifH_ASV%>%dplyr::select(nifHM_CTD$Sample)
nifH_ASV_ait <- dczm(nifH_ASV_CTD,1)
nifHM_CTD <- left_join(nifHM_CTD, nifH_meta[,c(2,4,23,25,26)], by = "No")

#Identify metadata which contributes significantly

source("/Users/choerstm/Documents/MIO_FIGURE/scripts/FIGURE_ordistep.R")

ordi(ALL_CTD, nifHM_CTD[,c(1,2,17,18,19,21,30:32,34:35)])

#with N2
nifH_meta_CTD_nNA <- nifHM_CTD[,c(1,2,9,10,17,18,19,21,30:32,34:35)]
nifH_meta_CTD_nNA <- na.omit(nifH_meta_CTD_nNA)
stations_with_N2 <- as.character(nifH_meta_CTD_nNA$No)
ALL_CTD_nNA <- ALL_CTD%>%dplyr::select(stations_with_N2)

#ordi(ALL_CTD_nNA, nifH_meta_CTD_nNA)
meta_sig <- c("swell", "Actual_Depth..m.")


#PLOT
RDA_plots(nifHM_CTD[,c(1:21, 28:31)], ALL_CTD, nifH_ASV_ait, meta_sig)


##Betadispersion structure

nifH_ASV_CTD <- nifH_ASV%>%dplyr::select(nifHM_CTD$Sample)
nifH_ASV_CTD.ait <- dczm(nifH_ASV_CTD,1)
nifH_ASV_CTD.ait.t <- t(nifH_ASV_CTD.ait)


dis <- vegdist(nifH_ASV_CTD.ait.t, method = "jaccard")
mod_small <- betadisper(dis, nifHM_CTD$Sampling_method)
print(anova(mod_small))

nifH_groupDistances <- as.data.frame(mod_small$group.distances)
nifH_groupDistances$Sampling_method <- rownames(nifH_groupDistances)
nifH_CTD_VV <- nifHM_CTD%>%group_by(Sampling_method)%>%
  summarise(mean_w = mean(mean_w))
nifH_groupDistances <- left_join(nifH_groupDistances, nifH_CTD_VV, by = "Sampling_method")

cor_Dw <- cor.test(nifH_groupDistances$`mod_small$group.distances`, -(nifH_groupDistances$mean_w), method="pearson")
cor_Dw
