library(plotly); library(reshape2); library(tidyverse)


library(R.matlab)

##https://pjbartlein.github.io/REarthSysSci/netCDF.html
library(ncdf4)
#load the nc files
ncpath <- "/Users/corahoerstmann/Documents/MIO_FIGURE/Satellite/copernicus/"
ncname <- "cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.25deg_P1D_1678376215597"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)

ncin <- nc_open(ncfname)
print(ncin)

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

adt <- ncvar_get(ncin,"adt")
nadt <- dim(adt)
head(adt)

adt_nc <- as.data.frame(adt)
rownames(adt_nc) <- lon
colnames(adt_nc) <- lat
adt_nc$Longitude <- rownames(adt_nc)
#bring into long format

adt_nc_long <- gather(adt_nc, "Latitude", "adt", -Longitude)
adt_nc_long$z <- -9

adt_nc_long$Longitude <- as.numeric(adt_nc_long$Longitude)
adt_nc_long$Latitude <- as.numeric(adt_nc_long$Latitude)

#cut the satellite image to boundaries of N2 measurements
adt_nc_long <- adt_nc_long%>%filter(Longitude > -73.6)
adt_nc_long <- adt_nc_long%>%filter(Longitude < -68)

adt_nc_long <- adt_nc_long%>%filter(Latitude > 33)
adt_nc_long <- adt_nc_long%>%filter(Latitude < 38)
#min(all_N2$Latitude)


library(plotly)
mat <- tapply(adt_nc_long$z, list(adt_nc_long$Latitude, adt_nc_long$Longitude), sum)
mat_col <- tapply(adt_nc_long$adt, list(adt_nc_long$Latitude, adt_nc_long$Longitude), sum)

Temp_section <- section_plot(section1_top150, "longitude", "depth", "temperature", interpolate = TRUE, MLD = NULL, xlab = "Longitude",
                             zlab = "temperature", contour_color = "white", zscale = "plasma")
Temp_section_interpolated <- Temp_section$plot_env$dt

Temp_section_interpolated$longitude <- Temp_section_interpolated$x
Temp_section_interpolated <- left_join(Temp_section_interpolated, section1_top150[,c(5,7)], by = "longitude")

#interpolate the latitude values
library(dplyr)
library(zoo)

Temp_section_interpolated <- Temp_section_interpolated %>%
  mutate(latitude = na.approx(latitude))
Temp_section_interpolated$mean_lat <- mean(Temp_section_interpolated$latitude)

Temp_section_interpolated$depth <- Temp_section_interpolated$y*(-1)
Temp_section_interpolated <- Temp_section_interpolated[-c(2:24),]

#reduce to 0.5 longitude resolution

Temp_section_interpolated_reduced = Temp_section_interpolated[seq(1, nrow(Temp_section_interpolated), 5), ]

Temp_section <- Temp_section_interpolated_reduced[,c(3:5,7)]
#mat_2 <- tapply(Temp_section_interpolated$depth, list(Temp_section_interpolated$longitude, Temp_section_interpolated$mean_lat), sum)

mat_3 <- sapply(split(Temp_section_interpolated_reduced, Temp_section_interpolated_reduced$latitude), function(d) setNames(Temp_section_interpolated_reduced$depth, Temp_section_interpolated_reduced$longitude))

mat_3 <- tapply(Temp_section$latitude, list(Temp_section$depth, Temp_section$longitude), sum)
mat_3 <- mat_3[,-c(1,21)]

mat_col <- tapply(adt_nc_long$adt, list(adt_nc_long$Latitude, adt_nc_long$Longitude), sum)


#mat_2[,1] <- 35.14592
mat_col_2 <- tapply(Temp_section$z, list(Temp_section$depth, Temp_section$longitude), sum)
mat_col_2 <- mat_col_2[,-c(1,21)]


x_sat <- as.numeric(colnames(mat))
y_sat <- as.numeric(rownames(mat))
plot_ly(z = mat, x= x_sat, y = y_sat, type = "surface", surfacecolor = mat_col)

x_sec <- as.numeric(rownames(mat_3))
y_sec <- as.numeric(colnames(mat_3))

plot_ly(z = ~mat_3, x= ~y_sec, y = ~x_sec, type = "surface", surfacecolor = mat_col_2)

x <- all_N2$Longitude
y <- all_N2$Latitude
z <- -(all_N2$Desired_Depth..m.)
u <- all_N2$N2
colvar <- all_N2$N2

# Plot
plotly_sat <- plot_ly() %>% 
  add_trace(data = all_N2, x = ~x, y = ~y, color = ~ u, z = z, 
            type = "scatter3d", mode = "markers",
            opacity = 1) %>%
  add_surface(z = mat, x= x_sat, y = y_sat, type = "surface", surfacecolor = mat_col, opacity = 0.6, colorscale = list(c(-0.5, 1.5), c("tan", "darkred")))%>%
  #add_surface(z = mat_2, x= x_sec, y = y_sec, type = "surface", surfacecolor = mat_col_2, opacity = 0.6)%>%
  add_trace(data = all_N2, z = colvar,
            x = y,
            y = x,
            type = "surface") %>% 
  layout(scene = list(xaxis = list(title = 'Longitude (•E)'),
                      yaxis = list(title = 'Latitude (•N)'),
                      zaxis = list(title = 'depth (m)')))
print(plotly_sat)

#q <- plot_ly() %>% 
#  add_trace(data = all_N2, x = ~x, y = ~z, color = ~ u, z = y, 
#            type = "scatter3d", mode = "markers",
#            opacity = 1) %>%
#  add_surface(z = mat_3, x= y_sec, y = x_sec, type = "surface", surfacecolor = mat_col_2, opacity = 0.8)%>%
#  add_trace(data = all_N2, z = colvar,
#            x = y,
#            y = x,
#            type = "surface") %>% 
#  layout(scene = list(xaxis = list(title = 'Latitude'),
#                      yaxis = list(title = 'Longitude'),
#                      zaxis = list(title = 'depth')))
#q
