
##from https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
#load additional packages
library(dendextend); packageVersion("dendextend")
library(colorspace) # get nice colors
library(stats); packageVersion("stats")
library(raster)
###16S

#input is a distance matrix - so we use Aithcinson
ASV16S_ait <- dczm(ASV16S,1)
T_ASV_16S <- as.data.frame(t(ASV16S))
T_ASV_16S$No <- rownames(T_ASV_16S)
a16S_meta$No <- as.character(a16S_meta$No)
CLUSTER_ASV_16S <- left_join(T_ASV_16S, a16S_meta[,c(1,19)], by ="No")

hc_iris <- hclust(t(ASV16S_ait), method = "complete")
iris_species <- rev(levels(CLUSTER_ASV_16S[,2632]))

species_labels <- CLUSTER_ASV_16S[,2632]

species_col <- rev(rainbow_hcl(19))[as.numeric(species_labels)]


dend <- as.dendrogram(hc_iris)

plot(dend)
# order it the closest we can to the order of the observations:
dend <- dendextend::rotate(dend, 1:41)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=5) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(19)[sort_levels_values(
    as.numeric(CLUSTER_ASV_16S[,2632])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(CLUSTER_ASV_16S[,2632])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Dendogram 16S", 
     horiz =  TRUE,  nodePar = list(cex = .007))
#legend("topleft", legend = iris_species, fill = rainbow_hcl(5))

#############

hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", 
                    "median", "centroid", "ward.D2")

iris_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
  hc_iris <- hclust(t(ASV16S_ait), method = hclust_methods[i])   
  iris_dendlist <- dendlist(iris_dendlist, as.dendrogram(hc_iris))
}

names(iris_dendlist) <- hclust_methods
iris_dendlist
iris_dendlist_cor <- cor.dendlist(iris_dendlist)
iris_dendlist_cor

corrplot::corrplot(iris_dendlist_cor, "pie", "lower")

iris_dendlist_cor_spearman <- cor.dendlist(iris_dendlist, method_coef = "spearman")
corrplot::corrplot(iris_dendlist_cor_spearman, "pie", "lower")

###Now it's interesting and where I want to get with nifH vs. 16S

# The `which` parameter allows us to pick the elements in the list to compare
iris_dendlist %>% dendlist(which = c(1,8)) %>% ladderize %>% 
  set("branches_k_color", k=4) %>% 
  untangle(method = "labels", k_seq = 3:20) %>%
  # set("clear_branches") %>% #otherwise the single lines are not black, since they retain the previous color from the branches_k_color.
  tanglegram(faster = TRUE) # (common_subtrees_color_branches = TRUE)

################################################################################
###nifH#####
################################################################################

#input is a distance matrix - so we use Aithcinson
nifH_ait <- dczm(nifH_ASV,1)
T_ASV_nifH <- as.data.frame(t(nifH_ASV))
T_ASV_nifH$No <- rownames(T_ASV_nifH)
nifH_meta$No <- as.character(nifH_meta$No)
CLUSTER_ASV_nifH <- left_join(T_ASV_nifH, nifH_meta[,c(2,28)], by ="No")

hc_nifH <- hclust(t(nifH_ait), method = "complete")
nifH_species <- rev(levels(CLUSTER_ASV_nifH[,1752]))

species_labels_nifH <- CLUSTER_ASV_nifH[,1752]

species_col_nifH <- rev(rainbow_hcl(19))[as.numeric(species_labels_nifH)]


dend_nifH <- as.dendrogram(hc_nifH)

plot(dend_nifH)
# order it the closest we can to the order of the observations:
dend_nifH <- dendextend::rotate(dend_nifH, 1:55)

# Color the branches based on the clusters:
dend_nifH <- color_branches(dend_nifH, k=5) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend_nifH) <-
  rainbow_hcl(19)[sort_levels_values(
    as.numeric(CLUSTER_ASV_nifH[,1752])[order.dendrogram(dend_nifH)]
  )]

# We shall add the flower type to the labels:
labels(dend_nifH) <- paste(as.character(CLUSTER_ASV_nifH[,1752])[order.dendrogram(dend_nifH)],
                      "(",labels(dend_nifH),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend_nifH <- hang.dendrogram(dend_nifH,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend_nifH <- set(dend_nifH, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend_nifH, 
     main = "Dendogran", 
     horiz =  TRUE,  nodePar = list(cex = .007))
#legend("topleft", legend = iris_species, fill = rainbow_hcl(5))

############################################
##nifH and 16S 
############################################
#reduce the nifH dataset to what's compatible with 16S

CLUSTER_ASV_nifH_R <- CLUSTER_ASV_nifH%>%filter(No %in% a16S_meta$No)
rownames(CLUSTER_ASV_nifH_R) <- CLUSTER_ASV_nifH_R$No

#define column names and colors before removing the metadata in the next step
#(double check: correctly assigned afterwards?)
nifH_R_species <- rev(levels(CLUSTER_ASV_nifH_R[,1752]))
species_labels_nifHR <- CLUSTER_ASV_nifH_R[,1752]
species_col_nifHR <- rev(rainbow_hcl(19))[as.numeric(species_labels_nifHR)]

##

CLUSTER_ASV_16S_R <- CLUSTER_ASV_16S%>%filter(No %in% CLUSTER_ASV_nifH_R$No)
rownames(CLUSTER_ASV_16S_R) <- CLUSTER_ASV_16S_R$No
CLUSTER_ASV_16S_R$No <- NULL
CLUSTER_ASV_16S_R$Main_Bin <- NULL
CLUSTER_ASV_nifH_R$No <- NULL
CLUSTER_ASV_nifH_R$Main_Bin <- NULL
#remove ASV with 0s
CLUSTER_ASV_nifH_RT <- t(CLUSTER_ASV_nifH_R)
CLUSTER_ASV_nifH_RT <- as.data.frame(CLUSTER_ASV_nifH_RT[rowSums(CLUSTER_ASV_nifH_RT[])>0,])

CLUSTER_ASV_16S_RT <- t(CLUSTER_ASV_16S_R)
CLUSTER_ASV_16S_RT <- as.data.frame(CLUSTER_ASV_16S_RT[rowSums(CLUSTER_ASV_16S_RT[])>0,])

#Re-calculate the aitchinson distances

ait_nifH_R <- dczm(CLUSTER_ASV_nifH_RT,1)
ait_16S_R <- dczm(CLUSTER_ASV_16S_RT,1)

#Calculate the distances
#("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")

hc_nifH_R_complete <- hclust(t(ait_nifH_R), method = "complete")
hc_nifH_R_wardD2 <- hclust(t(ait_nifH_R), method = "ward.D2")
hc_nifH_R_wardD <- hclust(t(ait_nifH_R), method = "ward.D")
hc_nifH_R_single <- hclust(t(ait_nifH_R), method = "single")
hc_nifH_R_average <- hclust(t(ait_nifH_R), method = "average")
hc_nifH_R_mcquitty <- hclust(t(ait_nifH_R), method = "mcquitty")
hc_nifH_R_median <- hclust(t(ait_nifH_R), method = "median")
hc_nifH_R_centroid <- hclust(t(ait_nifH_R), method = "centroid")

dend_nifH_R_complete <- as.dendrogram(hc_nifH_R_complete)
dend_nifH_R_wardD2 <- as.dendrogram(hc_nifH_R_wardD2)
dend_nifH_R_wardD <- as.dendrogram(hc_nifH_R_wardD)
dend_nifH_R_single <- as.dendrogram(hc_nifH_R_single)
dend_nifH_R_average <- as.dendrogram(hc_nifH_R_average)
dend_nifH_R_mcquitty <- as.dendrogram(hc_nifH_R_mcquitty)
dend_nifH_R_median <- as.dendrogram(hc_nifH_R_median)
dend_nifH_R_centroid <- as.dendrogram(hc_nifH_R_centroid)
#plot(dend_nifH_R)

hc_16S_R_complete <- hclust(t(ait_16S_R), method = "complete")
hc_16S_R_wardD2 <- hclust(t(ait_16S_R), method = "ward.D2")
hc_16S_R_wardD <- hclust(t(ait_16S_R), method = "ward.D")
hc_16S_R_single <- hclust(t(ait_16S_R), method = "single")
hc_16S_R_average <- hclust(t(ait_16S_R), method = "average")
hc_16S_R_mcquitty <- hclust(t(ait_16S_R), method = "mcquitty")
hc_16S_R_median <- hclust(t(ait_16S_R), method = "median")
hc_16S_R_centroid <- hclust(t(ait_16S_R), method = "centroid")

dend_16S_R_complete <- as.dendrogram(hc_16S_R_complete)
dend_16S_R_wardD2 <- as.dendrogram(hc_16S_R_wardD2)
dend_16S_R_wardD <- as.dendrogram(hc_16S_R_wardD)
dend_16S_R_single <- as.dendrogram(hc_16S_R_single)
dend_16S_R_average <- as.dendrogram(hc_16S_R_average)
dend_16S_R_mcquitty <- as.dendrogram(hc_16S_R_mcquitty)
dend_16S_R_median <- as.dendrogram(hc_16S_R_median)
dend_16S_R_centroid <- as.dendrogram(hc_16S_R_centroid)

#plot(dend_16S_R)

########## 

dend_names <- c("nifH_complete", "nifH_wardD2","nifH_wardD","nifH_single","nifH_average","nifH_mcquitty",
                "nifH_median","nifH_centroid",
                "16S_complete", "16S_wardD2","16S_wardD","16S_single","16S_average","16S_mcquitty",
                "16S_median","16S_centroid")

COM_dendlist <- dendlist(dend_nifH_R_complete, dend_nifH_R_wardD2, dend_nifH_R_wardD, dend_nifH_R_single,
                         dend_nifH_R_average, dend_nifH_R_mcquitty, dend_nifH_R_median, dend_nifH_R_centroid,
                         dend_16S_R_complete, dend_16S_R_wardD2, dend_16S_R_wardD, dend_16S_R_single,
                         dend_16S_R_average, dend_16S_R_mcquitty, dend_16S_R_median, dend_16S_R_centroid)
names(COM_dendlist) <- dend_names
COM_dendlist

COM_dendlist_cor <- cor.dendlist(COM_dendlist)
COM_dendlist_cor

corrplot::corrplot(COM_dendlist_cor, "pie", "lower")

COM_dendlist_cor_spearman <- cor.dendlist(COM_dendlist, method_coef = "spearman")
corrplot::corrplot(COM_dendlist_cor_spearman, "pie", "lower")
##wardD2 seems to be the best match

###Now it's interesting and where I want to get with nifH vs. 16S

# The `which` parameter allows us to pick the elements in the list to compare
#change the "which" option to the two methods compared. here wardD2
print(COM_dendlist %>% dendlist(which = c(2,10)) %>% ladderize %>% 
  set("branches_k_color", k=4) %>% 
  untangle(method = "labels", k_seq = 3:20) %>%
  # set("clear_branches") %>% #otherwise the single lines are not black, since they retain the previous color from the branches_k_color.
  tanglegram(faster = TRUE)) # (common_subtrees_color_branches = TRUE)

