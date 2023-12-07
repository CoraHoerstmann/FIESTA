#ASV_meta_red <- ASV_meta_with_N2[complete.cases(ASV_meta_with_N2[ , 29:50]),]
ASV_meta_red <- a16S_meta[complete.cases(a16S_meta[ ,9]),]
#nifH_ASVTable_CLR_norm_P <- as.data.frame(nifH_ASVTable_CLR_norm_P)

library(TITAN2); packageVersion("TITAN2")

#data(glades.taxa)
ASV16STable_red <- ASV16S.tax%>%dplyr::select(rownames(ASV_meta_red))

ASV16STable_red <- ASV16S_clr%>%dplyr::select(rownames(ASV_meta_red))
## relative abundances/ for more intituive reading of the plot scale the data

ASV_16S_clr_norm <- apply(ASV16STable_red, 2, function(x) x - min(x[x < 0]))

ASV_16S_clr_norm_P <- apply(ASV_16S_clr_norm, 2, function(x) {x/sum(x)*100})

ASV_16S_clr_norm_P <- as.matrix(ASV_16S_clr_norm_P)
ASV_16S_clr_norm_P <- as.data.frame(ASV_16S_clr_norm_P)
#minimum occurences must be 3 sites

d1 <- decostand(ASV_16S_clr_norm_P, method = "pa")
d1$criteria <- rowSums(d1)
d2 <- d1[d1$criteria > 3,]
d3 <- d1[d1$criteria > 94,]


ASV16STable_red.1 <- ASV_16S_clr_norm_P%>%filter(rownames(ASV_16S_clr_norm_P) %in% rownames(d2))
glades.test <- t(ASV16STable_red.1)

env_temp <- ASV_meta_red[,c(1,9)]
env_temp$No <- NULL

glades.titan <- titan(env_temp, glades.test)
head(glades.titan$sppmax)
aa <- glades.titan$sppmax
aa <- as.data.frame(aa)
##purity value should be above 0 so it drops also ost of them!

ab <- aa%>%filter(filter > 0)
ad <- ab%>%filter(purity == 1)
Indicator16S <- taxonomy_16S%>%filter(rownames(taxonomy_16S) %in% rownames(ab))

ab$ASV <- rownames(ab)
Indicator16S$ASV <- rownames(Indicator16S)

Indicator16S_Tricho <- inner_join(ab, Indicator16S, by = "ASV")

###create a phyloseq object for plotting
###PHYLOSEQ WITH N2 measurements

OTU = otu_table(ASV16STable_red, taxa_are_rows = TRUE)

tax.matrix<- as.matrix(taxonomy_16S)
TAX = tax_table(tax.matrix)
map = sample_data(ASV_meta_red)
phyloseq_merged = phyloseq(OTU, TAX)
all_16S_N2 = merge_phyloseq(phyloseq_merged, map) ####merge data into phyloseq
all_16S_N2

all_16S_N2_Long <- psmelt(all_16S_N2)

##This result can then be taken into phyloseq to look at the plots

All16S_indicator <- all_16S_N2_Long %>% filter(OTU %in% rownames(ab))
All16S_indicator_Purity <- all_16S_N2_Long %>% filter(OTU %in% rownames(ad))

ggplot(All16S_indicator)+
  geom_point(aes(x=mean_N2, y = Abundance, color = Genus), size = 0.5)+
  geom_smooth(aes(x=mean_N2, y = Abundance, color = Genus), size = 0.8, se = F)

ggplot(All16S_indicator_Purity)+
  geom_point(aes(x=mean_log_Tricho, y = Abundance, color = Genus))

All16S_indicator_Purity_ID <- All16S_indicator%>%filter(!Genus == "NA")

ggplot(All16S_indicator_Purity_ID)+
  geom_point(aes(x=mean_log_Tricho, y = Abundance, color = Genus), size = 0.5)+
  geom_smooth(aes(x=mean_log_Tricho, y = Abundance, color = Genus), size = 0.8, se = F)
