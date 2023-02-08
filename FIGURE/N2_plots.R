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
  geom_point(aes(x = -(Latitude), y = N2, fill = -(Desired_Depth..m.)), shape = 21, size = 1.8)+
  scale_fill_viridis() + 
  theme_bw() +
  labs(x = "Latitude", y = "fixation rate [nmol L-1 day-1]")

print(p)

p <- all_N2%>%
  ggplot()+
  geom_point(aes(x = Latitude, y = -(Desired_Depth..m.), fill = N2, size = N2), shape = 21, alpha = 0.8)+
  scale_fill_viridis() + 
  theme_bw() +
  labs(x = "Latitude", y = "depth")

print(p)


#safe as 500 x 200
#or for pdf as 10x4


#surface samples (the interpolation only makes sense for these samples as we have enough data through the underway)

N2_surf <- all_N2%>%filter(Desired_Depth..m. == 5)

pop <- N2_surf[,1:3]

pop$x = pop$Longitude
pop$y = pop$Latitude
pop$prop = pop$N2

pop <- pop[,4:6]

cfun <- function(x, bias=2) {
  x <- (x-min(x))/(max(x)-min(x))
  xcol <- colorRamp(c("lightyellow", "orange", "red"), bias=bias)(x)
  rgb(xcol, maxColorValue=255)
}

plot(pop$x, pop$y, col=cfun(pop$prop), cex=4, pch=20,
     xlab="Lon.", ylab="Lat.", main="N2 fixations")

e <- extent(pop[,1:2])

# this simple method of finding the correct number of rows and
# columns by counting the number of unique coordinate values in each
# dimension works in this case because there are no 'islands'
# (or if you wish, just one big 'island'), and the points are already
# regularly spaced.

nun <- function(x) { length(unique(x))}

r <- raster(e, ncol=nun(pop$x), nrow=nun(pop$y))

x <- rasterize(pop[, 1:2], r, pop[,3], fun=sum)
as.matrix(x)

cpal <- colorRampPalette(c("lightyellow", "orange", "red"), bias=2)

plot(x, col=cpal(200),
     xlab="Lon.", ylab="Lat.", main="N2 fixation")

# interpolation
pop.int <- interp(pop$x, pop$y,  pop$prop, duplicate = 'mean')

surface_plot <- filled.contour(pop.int$x, pop.int$y, pop.int$z,
               color.palette=cpal,
               xlab="Longitude", ylab="Latitude",
               main="N2 fixation",
               key.title = title(main="N2 fixation nmol N L-1 d-1", cex.main=0.8))
print(surface_plot)


rm(pop, e, pop.int, x, r, cpal)




