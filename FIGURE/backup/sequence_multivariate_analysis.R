#load packages
require("phyloseq"); packageVersion("phyloseq")
require(ggbiplot)
require(vegan)
require("wesanderson"); packageVersion("wesanderson")
require("fantaxtic"); packageVersion("fantaxtic")
#make a phyloseq object

OTU = otu_table(nifH_ASV_CLR, taxa_are_rows = TRUE)
rownames(nifH_taxonomy_CLR) <- nifH_taxonomy_CLR$X
nifH_taxonomy_CLR$X <- NULL

nifH_taxonomy_CLR$Species[is.na(nifH_taxonomy_CLR$Species)] <- "sp__"
nifH_taxonomy_CLR$Genus[is.na(nifH_taxonomy_CLR$Genus)] <- "g__"



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

miss_f <- which(taxtab[, "Species"] == "sp__")
miss_g <- which(taxtab[, "Genus"] == "g__")

# Number unspecified genera
taxtab[miss_f, "Species"] <- paste0("sp__", 1:length(miss_f))
taxtab[miss_g, "Genus"] <- paste0("g__", 1:length(miss_g))

# Find duplicate genera
dupl_g <- which(duplicated(taxtab[, "Species"]) |
                  duplicated(taxtab[, "Species"], fromLast = TRUE))

for(i in seq_along(taxtab)){
  # The next higher non-missing rank is assigned to unspecified genera
  if(i %in% miss_f && i %in% miss_g){
    taxtab[i, "Species"] <- paste0(taxtab[i, "Species"], "(", taxtab[i, "Family"], ")")
  } else if(i %in% miss_f){
    taxtab[i, "Species"] <- paste0(taxtab[i, "Species"], "(", taxtab[i, "Genus"], ")")
  }
  
  # Family names are added to duplicate genera
  if(i %in% dupl_g){
    taxtab[i, "Species"] <- paste0(taxtab[i, "Species"], "(", taxtab[i, "Species"], ")")
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
p3<-plot_ordination(all, out.pcoa.logt, type = "Sample",color= "Pysical_Classification", shape = "Depth_B") #"
#p3$layers <- p3$layers[-1]
p3+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.9),
        axis.text.y = element_text(size=16, vjust = 0.9),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.7))+
  theme(legend.key=element_blank())+
  geom_point(size=3, stroke =2)+
  scale_color_manual(values = c("#0df9f2","#34b219","#89a057", "#3f560d","#370eaa","#0f3cf7","#a50a9a", "#e0b70a","#72def7","#4af496", "#7594c6", "#f9c36e", "#915bdd"))+
  scale_shape_manual(values = c(0,1,2,3,5,6,7,8))
  #scale_color_gradientn(colours = pal)


###only CTD samples
nifHM_CTD$Sample <- as.character(nifHM_CTD$No)
nifH_ASV_CLR_CTD <- nifH_ASV_CLR%>%dplyr::select(nifHM_CTD$Sample)
OTU = otu_table(nifH_ASV_CLR_CTD, taxa_are_rows = TRUE)
nifH_tax_CTD <- as.data.frame(taxtab)
tax.matrix_CTD<- nifH_tax_CTD%>%filter(rownames(nifH_tax_CTD) %in% rownames(nifH_ASV_CLR_CTD))
tax.matrix_CTD<- as.matrix(tax.matrix_CTD)
TAX = tax_table(tax.matrix_CTD)

rownames(nifHM_CTD) <- nifHM_CTD$No
map = sample_data(nifHM_CTD)
rownames(map) <- map$No
phyloseq_merged = phyloseq(OTU, TAX)
all_CTD = merge_phyloseq(phyloseq_merged, map) ####merge data into phyloseq
all_CTD

##
nifH_ASV_CLR_CTD$Tax <- rownames(nifH_ASV_CLR_CTD)
tax_named_CTD <- as.data.frame(taxtab)

tax_named_CTD$Tax <- rownames(tax_named_CTD)
ALL_CTD <- left_join(nifH_ASV_CLR_CTD, tax_named_CTD[,7:8], by = "Tax")
ALL_CTD$Sp <- paste0(1:282,ALL_CTD$Species)
rownames(ALL_CTD) <- ALL_CTD$Sp
ALL_CTD$Sp <- NULL
ALL_CTD$Species <- NULL
ALL_CTD$Tax <- NULL
##RDA









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
all_TSpec <- tax_glom(all2, taxrank = "Species")
top <- fantaxtic::top_taxa(all_TSpec, 
                           tax_level = "Species", 
                           n_taxa = 10,
                           grouping = "Main_Bin")

top_taxa <- top$top_taxa

print(top_taxa)

e <- ggplot(top_taxa, aes(x = Main_Bin, y = abundance, color = Order, fill = Order))+
  geom_bar(stat="identity", position="stack")+
  #scale_fill_manual(values= c("#60E27B","#99BC08", "#9324EA", "#A3610A", "#824B04","#3F2707", "grey"))+
  #scale_color_manual(values= c("#60E27B", "#99BC08","#9324EA", "#A3610A", "#824B04","#3F2707","grey"))+
  theme(axis.text.x = element_text(size=10, vjust = 0.3, angle = 90))

print(e)




