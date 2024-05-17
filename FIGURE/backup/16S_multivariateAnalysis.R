
#load packages
require("phyloseq"); packageVersion("phyloseq")
require(ggbiplot)
require(vegan)
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

rownames(a16S_meta) <- a16S_meta$No
map = sample_data(a16S_meta)
rownames(map) <- map$No
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

scatterplot3d(x=a16S_meta$Longitude, y=a16S_meta$Latitude, z=-(a16S_meta$Desired_Depth..m.), color = colors,
              pch = 16, scale.y = 0.5, angle = 65)

