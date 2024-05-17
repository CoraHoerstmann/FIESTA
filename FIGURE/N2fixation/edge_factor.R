#identify core and periphery for the cyclonic eddy

ADCP_cyclonic_eddy_east <- ADCP_speed_noNA%>%filter(Longitude < -70.99)
ADCP_cyclonic_eddy_east <- ADCP_cyclonic_eddy_east%>%filter(Longitude > -71.75)

ADCP_cyclonic_eddy_west <- ADCP_speed_noNA%>%filter(Longitude < -71.75)
ADCP_cyclonic_eddy_west <- ADCP_cyclonic_eddy_west%>%filter(Longitude > -72.5)

#identify where is maximum speed and this should be the point between core and edge
ADCP_cyclonic_eddy_east_R <- ADCP_cyclonic_eddy_east[which.max(ADCP_cyclonic_eddy_east$speed_26.93),]
ADCP_cyclonic_eddy_west_R <- ADCP_cyclonic_eddy_west[which.max(ADCP_cyclonic_eddy_west$speed_26.93),]

#effective radius from the ocean atlas during that time

mean(sampling_time_CE$Effective.Radius..km.)
mean(sampling_time_CE$Longitude)
mean(sampling_time_CE$Latitude)

#calculate the radius of the two sides

library(geosphere)
#east
distm(c(-71.74856, 35.95984), c(-70.99559, 35.09831), fun = distHaversine)/1000
#west
distm(c(-72.49875, 36.54662), c(-71.75183, 35.83081), fun = distHaversine)/1000

#calculate the distance between the core and the edge of the core

distm(c(-71.74856, 35.95984), c(-71.29873, 35.56578), fun = distHaversine)/1000
distm(c(-71.75183, 35.83081), c(-71.8953,36.00808), fun = distHaversine)/1000

59.79371/117.6893
23.59456/104.1753

mean(c(0.5080641,0.226489))
#returns ~0.36*the radius is the actual core

rm(ADCP_cyclonic_eddy_west_R, ADCP_cyclonic_eddy_west, ADCP_cyclonic_eddy_east, ADCP_cyclonic_eddy_east_R)


##calculating the core based on okubo weiss

#import .mat file
require(R.matlab)
ow.mat <- readMat("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/20220725_nrt_cmems_d0.mat")
ow <- ow.mat$owmi
ow_calc <- (60*60*24)^2*ow
ow_calc <- as.data.frame(ow_calc)
rownames(ow_calc) <- ow.mat$yvc
colnames(ow_calc) <- ow.mat$xvc
ow.mat$ow_calc <-(60*60*24)^2*ow.mat$owmi
ow_calc$Latitude <- rownames(ow_calc)
ow_calc_long <- gather(ow_calc, "Longitude", "ow", -Latitude)
ow_calc_long$Latitude <- as.numeric(ow_calc_long$Latitude)
ow_calc_long$Longitude <- as.numeric(ow_calc_long$Longitude)
ow_plot <- ggplot(ow_calc_long, aes(Longitude, Latitude, z = ow))+
  geom_contour_filled()
print(ow_plot)

#define the lat/long of the eddy

ow_calc_long_C <- ow_calc_long%>%filter(Longitude < -70.6)
ow_calc_long_C <- ow_calc_long_C%>%filter(Longitude > -73)
ow_calc_long_C <- ow_calc_long_C%>%filter(Latitude > 34.6)
ow_calc_long_C <- ow_calc_long_C%>%filter(Latitude < 36.5)

Latitude <- c("35.67", "35.34","34.88")
Longitude <- c("-71.75", "-71.75","-71.75")

point_center_out <- data.frame(Latitude, Longitude)
point_center_out$Latitude <- as.numeric(point_center_out$Latitude)
point_center_out$Longitude <- as.numeric(point_center_out$Longitude)

point_center_distances <- geosphere::distm(point_center_out[,c("Longitude","Latitude")], fun = distHaversine)
36735.43/87942.40
ow_plot <- ggplot()+
  geom_contour_filled(data =ow_calc_long_C, aes(x=Longitude, y=Latitude, z = ow), bins = 44)+
  geom_contour(data =ow_calc_long_C, aes(x=Longitude, y=Latitude, z = ow),breaks = c(0,-80), color = "red")+
  geom_point(data = point_center_out, aes(x=Longitude, y=Latitude), color = "#c12188")+
  theme(legend.position="none")
print(ow_plot)
