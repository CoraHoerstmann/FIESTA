##Plot

p <- N2_data_all_CTD%>%
  ggplot()+
  geom_point(aes(y = N2, x = -(Desired_Depth..m.), color = Station))+
  geom_smooth(aes(y = N2, x = -(Desired_Depth..m.), group=Station, color = Station), method = "loess", level=0.50)+
  scale_color_manual(values = c("#0f3cf7","#f45a45","#ee9a00","#f9d606","#58d805", "#3f560d","#4af496"))+
  coord_flip()+
  theme_bw() +
  labs(x = "Depth", y = "fixation rate [nmol L-1 day-1]")

print(p)

#p <- N2_data_all_CTD%>%
#  ggplot()+
#  geom_point(aes(x = Longitude, y = N2, fill = -(Desired_Depth..m.)), shape = 21, size = 1.8)+
#  scale_fill_viridis() + 
#  theme_bw() +
#  labs(x = "Longitude", y = "fixation rate [nmol L-1 day-1]")

#print(p)


##or merge the two datasets

common_N2_data <- c("Name", "Latitude", "Longitude", "N2", "C_N", "Desired_Depth..m.")
N2_UW$Desired_Depth..m. <- 5


all_N2 <- rbind(N2_data_all_CTD[,common_N2_data], N2_UW[,common_N2_data])


####################

# Load libraries
library(plot3D)
library(rgl)
library(plot3Drgl)

# Define coordinates
x <- sep.l <- all_N2$Longitude
y <- pet.l <- all_N2$Latitude
z <- sep.w <- -(all_N2$Desired_Depth..m.)
u <- all_N2$N2
colvar <- all_N2$N2
# Add small dots on basal plane and on the depth plane
scatter3D_fancy <- function(x, y, z,..., colvar = u)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 1, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 1, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}

# Define like color
scatter3D_fancy(x, y, z, pch = 16,
                ticktype = "detailed", theta = -30, d = 2,
                main = "Iris data",  clab = c("Petal", "Width (cm)"))


###https://www.andreaperlato.com/graphpost/how-to-create-3d-and-4d-plot/
#####################
#c = all_N2$N2
#c = cut(c, breaks=77)
#colors2 <- rainbow(77)[as.numeric(c)]
#scatterplot3d(x=all_N2$Longitude, y=all_N2$Latitude, z=-(all_N2$Desired_Depth..m.), color = colors2,
#              pch = 16, scale.y = 0.5, angle = 65)
#color.bar(rainbow(77), min(c), max(c), title='C', ticks=round(seq(min(c), max(c), len=11), 2))

#all_data <- right_join(qPCR_all, all_N2, by = c("Latitude", "Longitude", "Desired_Depth..m."))
#write.csv(all_data, "FIGURE_all_data.csv")
##This file is made in R so needs to be manually updated with the column "biodepth"
##save files with 10x3.6 format
#all_data <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/FIGURE_all_data.csv")
#all_data_SFC <- all_data%>%filter(biodepth == "SFC")



#f <- ggplot()+
#  geom_point(data = all_data_SFC, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
#  ylim(0,60)+
#  xlim(-73.7,-68)

#print(f)


#all_data_aDCM <- all_data%>%filter(biodepth == "aDCM")


#f <- ggplot()+
#  geom_point(data = all_data_aDCM, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
#  ylim(0,60)+
#  xlim(-73.7,-68)

#print(f)

#all_data_DCM <- all_data%>%filter(biodepth == "DCM")



#f <- ggplot()+
#  geom_point(data = all_data_DCM, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
#  ylim(0,60)+
#  xlim(-73.7,-68)

#print(f)

#all_data_deep <- all_data%>%filter(biodepth == "bDCM")



#print(p)

#f <- ggplot()+
#  geom_point(data = all_data_deep, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
#  ylim(0,60)+
#  xlim(-73.7,-68)

#print(f)
#safe as 500 x 200
#or for pdf as 10x4


#surface samples (the interpolation only makes sense for these samples as we have enough data through the underway)

#N2_surf <- all_N2%>%filter(Desired_Depth..m. == 5)

#write.csv(N2_surf, "FIGURE_N2_SFC.csv")

#pop <- N2_surf[,1:3]

#pop$x = pop$Longitude
#pop$y = pop$Latitude
#pop$prop = pop$N2

#pop <- pop[,4:6]

#cfun <- function(x, bias=2) {
#  x <- (x-min(x))/(max(x)-min(x))
#  xcol <- colorRamp(c("lightyellow", "orange", "red"), bias=bias)(x)
#  rgb(xcol, maxColorValue=255)
#}

#plot(pop$x, pop$y, col=cfun(pop$prop), cex=4, pch=20,
#     xlab="Lon.", ylab="Lat.", main="N2 fixations")

#e <- extent(pop[,1:2])

# this simple method of finding the correct number of rows and
# columns by counting the number of unique coordinate values in each
# dimension works in this case because there are no 'islands'
# (or if you wish, just one big 'island'), and the points are already
# regularly spaced.

#nun <- function(x) { length(unique(x))}

#r <- raster(e, ncol=nun(pop$x), nrow=nun(pop$y))

#x <- rasterize(pop[, 1:2], r, pop[,3], fun=sum)
#as.matrix(x)

#cpal <- colorRampPalette(c("lightyellow", "orange", "red"), bias=2)

#plot(x, col=cpal(200),
#     xlab="Lon.", ylab="Lat.", main="N2 fixation")

# interpolation
#pop.int <- interp(pop$x, pop$y,  pop$prop, duplicate = 'mean')

#surface_plot <- filled.contour(pop.int$x, pop.int$y, pop.int$z,
#               color.palette=cpal,
#               xlab="Longitude", ylab="Latitude",
#               main="N2 fixation",
#               key.title = title(main="N2 fixation nmol N L-1 d-1", cex.main=0.8))
#print(surface_plot)


#rm(pop, e, pop.int, x, r, cpal)



