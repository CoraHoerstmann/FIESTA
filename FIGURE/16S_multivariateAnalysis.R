
#load packages
require("phyloseq"); packageVersion("phyloseq")
require(ggbiplot)
require(vegan); packageVersion("vegan")
require("wesanderson"); packageVersion("wesanderson")
require("fantaxtic"); packageVersion("fantaxtic")


##CLR transform the 16S data

ASV16S_clr <- clr(ASV16S.tax,1)
taxonomy_16S_clr <- taxonomy_16S%>%dplyr::filter(rownames(taxonomy_16S) %in% rownames(ASV16S_clr))


#PCA

my_pca <- prcomp(ASV16S_clr, scale = FALSE,
                 center = TRUE, retx = T)
summary(my_pca)
dim(my_pca$x)

#make a phyloseq object

OTU = otu_table(ASV16S_clr, taxa_are_rows = TRUE)
tax.matrix<- as.matrix(taxonomy_16S_clr)
TAX = tax_table(tax.matrix)

a16S_meta$Main_Bin <- paste(a16S_meta$Pysical_Classification, a16S_meta$Desired_Depth..m., sep= "_")
a16S_meta$Main_Bin <- factor(a16S_meta$Main_Bin, levels = unique(a16S_meta$Main_Bin[order(a16S_meta$Int_Order)]))

##only bigger physical structures
a16S_AC <- a16S_meta%>%filter(str_detect(Pysical_Classification, "AC"))%>% 
  cbind(broad_Structure = paste0("AC"))

a16S_C <- a16S_meta%>%filter(str_detect(Pysical_Classification, " C"))%>% 
  cbind(broad_Structure = paste0("C"))

a16S_GS_C <- a16S_C%>%filter(str_detect(Pysical_Classification, "GS/"))

a16S_GS_C$broad_Structure <- "GS/C"

a16S_C <- a16S_C%>%filter(!str_detect(Pysical_Classification, "GS/"))

a16S_GS <- a16S_meta%>%filter(str_detect(Pysical_Classification, "GS"))%>% 
  cbind(broad_Structure = paste0("GS"))

a16S_GS <- a16S_GS%>%filter(!str_detect(Pysical_Classification, "GS/"))

a16S_OO <- a16S_meta%>%filter(str_detect(Pysical_Classification, "open"))%>% 
  cbind(broad_Structure = paste0("open_ocean"))

a16S_meta <- rbind(a16S_OO, a16S_AC, a16S_C, a16S_GS_C, a16S_GS)
rm(a16S_OO, a16S_AC, a16S_C, a16S_GS_C, a16S_GS)

rownames(a16S_meta) <- a16S_meta$No
map = sample_data(a16S_meta)
rownames(map) <- map$No
#map$kcluster3u <- as.character(map$kcluster3u)
phyloseq_merged = phyloseq(OTU, TAX) #no tax annotation yet
all = merge_phyloseq(phyloseq_merged, map) ####merge data into phyloseq
all

pal <- wes_palette("Zissou1", 21, type = "continuous")

##Color assignment
#open ocean "#0ac6f2"
#east edge AC"#370eaa"
#center AC"#0f3cf7"
#west edge AC"#a50a9a" 
#east edge C "#f45a45"
#east-middle C "#ed7000"
#center C "#ee9a00"
#west edge C "#f9d606"
#GS/ cyclone "#58d805"
#GS"#3f560d"
#west edge GS "#499b6e"
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

#scale_color_gradientn(colours = pal)
library("scatterplot3d")

colors <- c("#0f3cf7","#ee9a00","#370eaa","#f45a45", "#3f560d","#58d805","#4af496","#0ac6f2",
                     "#a50a9a" , "#f9d606", "#499b6e")
colors <- colors[as.factor(a16S_meta$Pysical_Classification)]

a16S_meta$Pysical_Classification
scatterplot3d(x=a16S_meta$Longitude, y=a16S_meta$Latitude, z=-(a16S_meta$Desired_Depth..m.), color = colors,
              pch = 16, scale.y = 0.5, angle = 65)

##PERMANOVA of water masses 
ASV_16S_ait <- dczm(ASV16S,1)
ASV_16S_ait.t <- t(ASV_16S_ait)

print(adonis2(
  formula = ASV_16S_ait.t ~ Pysical_Classification,
  data = a16S_meta,
  method = "jaccard"
))

print(adonis2(
  formula = ASV_16S_ait.t ~ broad_Structure,
  data = a16S_meta,
  method = "jaccard"
))


## Vertical vel along stations (subset of St 0-2)
ST16S <- c("St 0 C1", "St 1 C2", "St 2 C2")
a16S_meta_ST <- a16S_meta%>%filter(Sampling_method %in% ST16S)
a16S_meta_ST$No <- as.character(a16S_meta_ST$No)
ASV16S_ST <- ASV16S%>%dplyr::select(a16S_meta_ST$No)
ASV_16S_STait <- dczm(ASV16S_ST,1)
dis <- vegdist(t(ASV_16S_STait), method = "jaccard")
mod_small <- betadisper(dis, a16S_meta_ST$Sampling_method)

print(mod_small)
print(anova(mod_small))
#

a16S_groupDistances <- as.data.frame(mod_small$group.distances)
a16S_groupDistances$Sampling_method <- rownames(a16S_groupDistances)
a16S_CTD_VV <- a16S_meta_ST%>%group_by(Sampling_method)%>%
  summarise(mean_w = mean(mean_w))
a16S_groupDistances <- left_join(a16S_groupDistances, a16S_CTD_VV, by = "Sampling_method")

cor_Dw <- cor.test(a16S_groupDistances$`mod_small$group.distances`, -(a16S_groupDistances$mean_w), method="pearson")
cor_Dw
