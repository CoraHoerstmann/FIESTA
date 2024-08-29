library(plotly); library(reshape2); library(tidyverse)
packageVersion("plotly")
library(zoo)
packageVersion("zoo")
library(ncdf4) #REF: https://pjbartlein.github.io/REarthSysSci/netCDF.html
packageVersion("ncdf4")


common_N2_data <- c("Name", "Latitude", "Longitude", "N2", "C_N", "Desired_Depth..m.")
N2_UW$Desired_Depth..m. <- 5


all_N2 <- rbind(N2_data_all_CTD[,common_N2_data], N2_UW[,common_N2_data])



#library(R.matlab)

ncpath <- "/Users/choerstm/Documents/MIO_FIGURE/Oceanography/"
ncname <- "20220726_nrt_cmems_d0_lambda_only"  

ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)

ncin <- nc_open(ncfname)
print(ncin)

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

adt <- ncvar_get(ncin,"lambd")
nadt <- dim(adt)
head(adt)

adt_nc <- as.data.frame(adt)
rownames(adt_nc) <- lon
colnames(adt_nc) <- lat
adt_nc$Longitude <- rownames(adt_nc)
#bring into long format

adt_nc_long <- gather(adt_nc, "Latitude", "adt", -Longitude)
adt_nc_long$lambda <- as.numeric(adt_nc_long$adt*-60*60*24)
adt_nc_long$z <- -9

adt_nc_long$Longitude <- as.numeric(adt_nc_long$Longitude)
adt_nc_long$Latitude <- as.numeric(adt_nc_long$Latitude)

#cut the satellite image to boundaries of N2 measurements
adt_nc_long <- adt_nc_long%>%filter(Longitude > -73.6)
adt_nc_long <- adt_nc_long%>%filter(Longitude < -68)

adt_nc_long <- adt_nc_long%>%filter(Latitude > 33)
adt_nc_long <- adt_nc_long%>%filter(Latitude < 38)
#min(all_N2$Latitude)

mat <- tapply(adt_nc_long$z, list(adt_nc_long$Latitude, adt_nc_long$Longitude), sum)
mat_col <- tapply(adt_nc_long$lambda, list(adt_nc_long$Latitude, adt_nc_long$Longitude), sum)

#interpolate the latitude values


x_sat <- as.numeric(colnames(mat))
y_sat <- as.numeric(rownames(mat))
plot_ly(z = mat, x= x_sat, y = y_sat, type = "surface", surfacecolor = mat_col)

x <- all_N2$Longitude
y <- all_N2$Latitude
z <- -(all_N2$Desired_Depth..m.)
u <- all_N2$N2
colvar <- all_N2$N2

# Plot
options(warn = -1)

library("RColorBrewer")
brewer.pal.info

plotly_sat <- plot_ly() %>% 
  add_trace(data = all_N2, x = ~x, y = ~y, color = ~ u, z = z, 
            type = "scatter3d", mode = "markers",
            opacity = 1,
            showlegend = FALSE
           ) %>%
  add_surface(z = mat, x= x_sat, y = y_sat, type = "surface", surfacecolor = mat_col, opacity = 0.9, colorscale='Greys', 
              colorbar = list(title = "FSLE [-d]", x = 1, y = 0.5), showlegend = FALSE)%>%
  #add_surface(z = mat_2, x= x_sec, y = y_sec, type = "surface", surfacecolor = mat_col_2, opacity = 0.6)%>%
  add_trace(data = all_N2, z = colvar,
            x = y,
            y = x,
            type = "surface",
            colorbar= list(title= 'N2 fixation nmol N L-1 d-1', x = 1, y = 1)) %>% 
  layout(scene = list(xaxis = list(title = 'Longitude (°E)'),
                      yaxis = list(title = 'Latitude (°N)'),
                      zaxis = list(title = 'depth (m)')))#%>%

print(plotly_sat)

################################################################################

# TEMPERATURE SECTION N2 in 2D

Temp_section <- section_plot(section1_top150, "longitude", "depth", "temperature", interpolate = TRUE, MLD = NULL, xlab = "Longitude",
                             zlab = "temperature", contour_color = "white", zscale = "plasma")
                             
Temp_section_interpolated <- Temp_section$plot_env$dt
Temp_section_interpolated$longitude <- Temp_section_interpolated$x
Temp_section_interpolated$depth <- -(Temp_section_interpolated$y)
Temp_section_interpolated$temperature <- Temp_section_interpolated$z

Temp_section_interpolated <- Temp_section_interpolated[,4:6]
Temp_section_matrix <- tapply(Temp_section_interpolated$temperature, list(Temp_section_interpolated$depth, Temp_section_interpolated$longitude), sum)
  
fig <- plot_ly()%>%
  add_contour(z = Temp_section_matrix, x = colnames(Temp_section_matrix), y = rownames(Temp_section_matrix),
              colorscale = 'thermal', opacity = 0.8,
              autocontour = T,
              colorbar=list(
                title='Temperature °C', x= 1, y = 0.5
              ))%>%
  add_trace(data = all_N2, x = ~x, y = ~z, color = ~ u, mode = "markers",
            type = "scatter",
            size = 2.5,
            marker = list(
              colorbar = list(title = "N2 fixation nmol N L-1 d-1", x =1, y =1)))%>%
  layout(xaxis = list(title = 'Longitude (°E)'), 
         yaxis = list(title = 'Depth (m)'))



print(fig)

#DENSITY (to look at the pycnocline)

DEN_section <- section_plot(section1_top150, "longitude", "depth", "density", interpolate = TRUE, MLD = NULL, xlab = "Longitude",
                             zlab = "density", contour_color = "white", zscale = "plasma")

DEN_section_interpolated <- DEN_section$plot_env$dt
DEN_section_interpolated$longitude <- DEN_section_interpolated$x
DEN_section_interpolated$depth <- -(DEN_section_interpolated$y)
DEN_section_interpolated$density <- DEN_section_interpolated$z

DEN_section_interpolated <- DEN_section_interpolated[,4:6]
DEN_section_matrix <- tapply(DEN_section_interpolated$density, list(DEN_section_interpolated$depth, DEN_section_interpolated$longitude), sum)

DEN_UNIQUE <- section1_top150%>%group_by(latitude, longitude)%>%
  summarise(pycno = mean(pycnocline))
DEN_UNIQUE$pycno <- -1*DEN_UNIQUE$pycno

fig <- plot_ly()%>%
  add_contour(z = DEN_section_matrix, x = colnames(DEN_section_matrix), y = rownames(DEN_section_matrix),
              colorscale = 'Greys', opacity = 0.8,
              autocontour = T,
              colorbar=list(
                title='density', x= 1, y = 0.5
              ))%>%
  add_trace(data = all_N2, x = ~x, y = ~z, color = ~ u, mode = "markers",
            type = "scatter",
            size = 2.5,
            marker = list(
              colorbar = list(title = "N2 fixation nmol N L-1 d-1", x =1, y =1)))%>%
  add_trace(data = DEN_UNIQUE, x = ~longitude, y = ~pycno, name = 'pycnocline', mode = 'lines+markers', color = "orange")%>%
  layout(xaxis = list(title = 'Longitude (°E)'), 
         yaxis = list(title = 'Depth (m)'))



fig


#do individual plots for the eddies

#ST0
df_st0_c1_150 <- df_st0_c1%>%filter(depth < 151)
all_N2$depth <-all_N2$Desired_Depth..m.
N2_split <- split(all_N2, all_N2$Latitude)

ggplot(df_st0_c1_150, aes(x = density, y = -(depth)))+
  geom_line()

ggplot(N2_split$'33.4927666666667', aes(x = N2, y = -(Desired_Depth..m.)))+
  geom_point(color = "red")+
  ylim(-150, 0)

#ST1
df_st1_c1_150 <- df_st1_c1%>%filter(depth < 151)

ggplot(df_st1_c1_150, aes(x = density, y = -(depth)))+
  geom_line()

ggplot(N2_split$'35.1005333333333', aes(x = N2, y = -(Desired_Depth..m.)))+
  geom_point(color = "red")+
  ylim(-150, 0)

#ST2
df_st2_c1_150 <- df_st2_c1%>%filter(depth < 151)

ggplot(df_st2_c1_150, aes(x = density, y = -(depth)))+
  geom_line()

ggplot(N2_split$'35.5872', aes(x = N2, y = -(Desired_Depth..m.)))+
  geom_point(color = "red")+
  ylim(-150, 0)

#ST3
df_st3_c1_150 <- df_st3_c1%>%filter(depth < 151)

ggplot(df_st3_c1_150, aes(x = density, y = -(depth)))+
  geom_line()

ggplot(N2_split$'36.1076833333333', aes(x = N2, y = -(Desired_Depth..m.)))+
  geom_point(color = "red")+
  ylim(-150, 0)

#Temperature section in 3D (not very informative representation)


#Temp_section_interpolated <- left_join(Temp_section_interpolated, section1_top150[,c(5,7)], by = "longitude")
#Temp_section_interpolated <- Temp_section_interpolated %>%
#  mutate(latitude = na.approx(latitude))
#Temp_section_interpolated$mean_lat <- mean(Temp_section_interpolated$latitude)
#Temp_section_interpolated$depth <- Temp_section_interpolated$y*(-1)
#Temp_section_interpolated <- Temp_section_interpolated[-c(2:24),]

#reduce to 0.5 longitude resolution

#Temp_section_interpolated_reduced = Temp_section_interpolated[seq(1, nrow(Temp_section_interpolated), 5), ]
#Temp_section <- Temp_section_interpolated_reduced[,c(3:5,7)]

#mat_3 <- sapply(split(Temp_section_interpolated_reduced, Temp_section_interpolated_reduced$latitude), function(d) setNames(Temp_section_interpolated_reduced$depth, Temp_section_interpolated_reduced$longitude))
#mat_3 <- tapply(Temp_section$latitude, list(Temp_section$depth, Temp_section$longitude), sum)
#mat_3 <- mat_3[,-c(1,21)]
#mat_col_2 <- tapply(Temp_section$z, list(Temp_section$depth, Temp_section$longitude), sum)
#mat_col_2 <- mat_col_2[,-c(1,21)]
#x_sec <- as.numeric(rownames(mat_3))
#y_sec <- as.numeric(colnames(mat_3))

#q <- plot_ly() %>% 
#  add_trace(data = all_N2, x = ~x, y = ~z, color = ~ u, z = y, 
#            type = "scatter3d", mode = "markers",
#            opacity = 1) %>%
#  add_surface(z = mat_3, x= y_sec, y = x_sec, type = "surface", surfacecolor = mat_col_2, opacity = 0.5, colorscale='Inferno', 
#              colorbar = list(title = "Temperature •C", x = 1, y = 0.5))%>%
#  add_trace(data = all_N2, z = colvar,
#            x = y,
#            y = x,
#            type = "surface",
#            colorbar= list(title= 'N2 fixation nmol N L-1 d-1', x = 1, y = 1)) %>% 
#  layout(scene = list(xaxis = list(title = 'Longitude'),
#                      yaxis = list(title = 'depth'),
#                      zaxis = list(title = 'Latitude')))
#q
