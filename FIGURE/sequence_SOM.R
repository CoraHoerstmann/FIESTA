library(kohonen)
library(phyloseq)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggpubr)
library(vegan)

library(funrar)

###################################################################################################################
###SOM community segmentation and dimension reduction

str(iris)
# Kohonen needs a numeric matrix
edge.norm <- as.matrix(t(ASV16S_clr))

#maybe rel. abundance and not CLR?
  
#edge.norm <- funrar::make_relative(t(ASV16S))

## Define a grid.  The bigger the better, but you want many fewer units in the grid
## than samples.  1:5 is a good ballpark, here we are minimal.
som.grid <- somgrid(xdim = 3, ydim=2, topo="hexagonal")


set.seed(2023)

## Now build the ESOM!  It is worth playing with the parameters, though in
## most cases you will want the circular neighborhood and toroidal map structure.
som.model.edges <- som(edge.norm, 
                       grid = som.grid, 
                       #rlen = 100,
                       #alpha = c(0.05,0.01),
                       #keep.data = TRUE,
)

plot(som.model.edges, type = 'mapping', pch=16, col=as.factor(map$Pysical_Classification))
#add.cluster.boundaries(som.model.edges, som.cluster.edges$cluster)


plot(som.model.edges, type = 'counts', pch = 19, palette.name = topo.colors)


som.model.edges$codes=as.data.frame(som.model.edges$codes)
wss.edges <- (nrow(som.model.edges$codes)-1)*sum(apply(som.model.edges$codes,2,var)) 
for (i in 2:15) {
  wss.edges[i] <- sum(kmeans(som.model.edges$codes, centers=i)$withinss)
}

plot(wss.edges,
     pch = 19,
     ylab = 'Within-clusters sum of squares',
     xlab = 'K')



###Select cluster number and run kmeans 

#Three clusters

k <- 5
som.cluster.edges <- kmeans(som.model.edges$codes, centers = k)



plot(som.model.edges,
     main = '',
     type = "property",
     property = som.cluster.edges$cluster,
     palette.name = topo.colors)
add.cluster.boundaries(som.model.edges, som.cluster.edges$cluster)


som.cluster.edges$cluster[som.model.edges$unit.classif]

# get vector with cluster value for each original data sample
cluster_assignment <- som.cluster.edges$cluster[som.model.edges$unit.classif]
# for each of analysis, add the assignment as a column in the original data:

map$kcluster3u = cluster_assignment


k <- 6
som.cluster.edges <- kmeans(som.model.edges$codes, centers = k)



plot(som.model.edges,
     main = '',
     type = "property",
     property = som.cluster.edges$cluster,
     palette.name = topo.colors)
add.cluster.boundaries(som.model.edges, som.cluster.edges$cluster)


som.cluster.edges$cluster[som.model.edges$unit.classif]

# get vector with cluster value for each original data sample
cluster_assignment <- som.cluster.edges$cluster[som.model.edges$unit.classif]
# for each of analysis, add the assignment as a column in the original data:

SWmeta$kcluster4u = cluster_assignment


#Five clusters

k <- 5
som.cluster.edges <- kmeans(som.model.edges$codes, centers = k)



plot(som.model.edges,
     main = '',
     type = "property",
     property = som.cluster.edges$cluster,
     palette.name = topo.colors)
add.cluster.boundaries(som.model.edges, som.cluster.edges$cluster)


som.cluster.edges$cluster[som.model.edges$unit.classif]

# get vector with cluster value for each original data sample

cluster_assignment <- som.cluster.edges$cluster[som.model.edges$unit.classif]

# for each of analysis, add the assignment as a column in the original data:

SWmeta$kcluster5u = cluster_assignment

write.csv(SWmeta, "~/Desktop/SWmeta_somu.csv")