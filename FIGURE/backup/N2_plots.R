##Plot

p <- N2_data_all_CTD%>%
  ggplot()+
  geom_point(aes(x = Station, y = N2, color = -(Desired_Depth..m.)))+
  scale_color_viridis() + 
  theme_bw() +
  labs(x = "Station", y = "fixation rate [nmol L-1 day-1]")

print(p)

p <- N2_data_all_CTD%>%
  ggplot()+
  geom_point(aes(x = Longitude, y = N2, fill = -(Desired_Depth..m.)), shape = 21, size = 1.8)+
  scale_fill_viridis() + 
  theme_bw() +
  labs(x = "Longitude", y = "fixation rate [nmol L-1 day-1]")

print(p)


#UW

u <- N2_UW%>%
  ggplot()+
  geom_point(aes(x = Longitude, y = N2), fill = "#FCE51E", shape = 21, size = 1.8)+
  theme_bw() +
  labs(x = "Longitude", y = "fixation rate [nmol L-1 day-1]")

print(u)

##or merge the two datasets

common_N2_data <- c("Latitude", "Longitude", "N2", "C_N", "Desired_Depth..m.")
N2_UW$Desired_Depth..m. <- 5

all_N2 <- rbind(N2_data_all_CTD[,common_N2_data], N2_UW[,common_N2_data])

p <- all_N2%>%
  ggplot()+
  geom_point(aes(x = Longitude, y = N2, fill = -(Desired_Depth..m.)), shape = 21, size = 1.8)+
  scale_fill_viridis() + 
  theme_bw() +
  labs(x = "Longitude", y = "fixation rate [nmol L-1 day-1]")

print(p)

p <- all_N2%>%
  ggplot()+
  geom_point(aes(x = Longitude, y = -(Desired_Depth..m.), fill = N2, size = N2), shape = 21, alpha = 0.8)+
  scale_fill_viridis() + 
  theme_bw() +
  labs(x = "Longitude", y = "depth")

print(p)


#all_data <- right_join(qPCR_all, all_N2, by = c("Latitude", "Longitude", "Desired_Depth..m."))
#write.csv(all_data, "FIGURE_all_data.csv")
##This file is made in R so needs to be manually updated with the column "biodepth"
##save files with 10x3.6 format
#all_data <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/FIGURE_all_data.csv")
all_data_SFC <- all_data%>%filter(biodepth == "SFC")

p <- ggplot()+
  geom_point(data = all_data_SFC, aes(x=Longitude, y=log_Tricho), fill="#F93F21", size=3, shape=21, alpha=0.8)+
  geom_point(data = all_data_SFC, aes(x=Longitude, y=log_UCYN_A1), fill="#EDB106",size=3, shape=21, alpha=0.6)+
  geom_point(data = all_data_SFC, aes(x=Longitude, y=log_GammaA), fill="#A30D79", size=3, shape=21, alpha=0.4)+
  ylim(0,14)+
  xlim(-73.7,-68)
  
print(p)

f <- ggplot()+
  geom_point(data = all_data_SFC, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
  ylim(0,60)+
  xlim(-73.7,-68)

print(f)


all_data_aDCM <- all_data%>%filter(biodepth == "aDCM")

p <- ggplot()+
  geom_point(data = all_data_aDCM, aes(x=Longitude, y=log_Tricho), fill="#F93F21", size=3, shape=21, alpha=0.8)+
  geom_point(data = all_data_aDCM, aes(x=Longitude, y=log_UCYN_A1), fill="#EDB106",size=3, shape=21, alpha=0.6)+
  geom_point(data = all_data_aDCM, aes(x=Longitude, y=log_GammaA), fill="#A30D79", size=3, shape=21, alpha=0.4)+
  ylim(0,14)+
  xlim(-73.7,-68)

print(p)

f <- ggplot()+
  geom_point(data = all_data_aDCM, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
  ylim(0,60)+
  xlim(-73.7,-68)

print(f)

all_data_DCM <- all_data%>%filter(biodepth == "DCM")

p <- ggplot()+
  geom_point(data = all_data_DCM, aes(x=Longitude, y=log_Tricho), fill="#F93F21", size=3, shape=21, alpha=0.8)+
  geom_point(data = all_data_DCM, aes(x=Longitude, y=log_UCYN_A1), fill="#EDB106",size=3, shape=21, alpha=0.6)+
  geom_point(data = all_data_DCM, aes(x=Longitude, y=log_GammaA), fill="#A30D79", size=3, shape=21, alpha=0.4)+
  ylim(0,14)+
  xlim(-73.7,-68)

print(p)

f <- ggplot()+
  geom_point(data = all_data_DCM, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
  ylim(0,60)+
  xlim(-73.7,-68)

print(f)

all_data_deep <- all_data%>%filter(biodepth == "bDCM")

p <- ggplot()+
  geom_point(data = all_data_deep, aes(x=Longitude, y=log_Tricho), fill="#F93F21", size=3, shape=21, alpha=0.8)+
  geom_point(data = all_data_deep, aes(x=Longitude, y=log_UCYN_A1), fill="#EDB106",size=3, shape=21, alpha=0.6)+
  geom_point(data = all_data_deep, aes(x=Longitude, y=log_GammaA), fill="#A30D79", size=3, shape=21, alpha=0.4)+
  ylim(0,14)+
  xlim(-73.7,-68)

print(p)

f <- ggplot()+
  geom_point(data = all_data_deep, aes(x=Longitude, y=N2), fill="black", size=3, shape=18, alpha=1)+
  ylim(0,60)+
  xlim(-73.7,-68)

print(f)
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




