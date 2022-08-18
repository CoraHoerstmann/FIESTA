require(oce)
require(ocedata)
require(dplyr)
#remotes::install_github("jiho/castr")
#require(castr)

#read profiles of the 1st section
stn0_c1 = read.ctd("D://AE_2215/RAW/PROCESSED/AE2215_stn00_cast1.cnv")
stn1_c1 = read.ctd("D://AE_2215/RAW/PROCESSED/AE2215_stn01_cast1.cnv")
stn2_c1 = read.ctd("D://AE_2215/RAW/PROCESSED/AE2215_stn02_cast1.cnv")
stn3_c1 = read.ctd("D://AE_2215/RAW/PROCESSED/AE2215_stn03_cast1.cnv")
stn4_c1 = read.ctd("D://AE_2215/RAW/PROCESSED/AE2215_stn04_cast1.cnv")
stn5_c1 = read.ctd("D://AE_2215/RAW/PROCESSED/AE2215_stn05_cast1.cnv")
stn6_c1 = read.ctd("D://AE_2215/RAW/PROCESSED/AE2215_stn06_cast1.cnv")

#check out the variables

stn0_c1@data%>%as.data.frame()%>%dplyr::glimpse()

#plot
#stn0_c1%>%plot()

#stn0_c1_downcast = stn0_c1%>%
  #ctdTrim(method = "downcast")%>% #already discaded i the seabird software
#  ctdDecimate(p = 0.2)

#par(mfrow = c(1,2))
#stn0_c1_downcast%>%plot(which = c("salinity"))
#stn0_c1_downcast%>%plot(which = c("temperature"))

#par(mfrow = c(1,2))
#stn1_c1%>%plot(which = c("salinity"))
#stn1_c1%>%plot(which = c("temperature"))


#convert into dataframes
df_st0_c1 <- as.data.frame(stn0_c1@data)
df_st1_c1 <- as.data.frame(stn1_c1@data)
df_st2_c1 <- as.data.frame(stn2_c1@data)
df_st3_c1 <- as.data.frame(stn3_c1@data)
df_st4_c1 <- as.data.frame(stn4_c1@data)
df_st5_c1 <- as.data.frame(stn5_c1@data)
df_st6_c1 <- as.data.frame(stn6_c1@data)

#remove NAs ## that was only an issue in the previously processed files. 

#discard <- apply(df_st1_c1, 1, function(x) any(is.na(x)))
#df_st1_c1 <- df_st1_c1[!discard,]

#calculate MLD

source("C://Users/choerstm/Documents/mld_m.R")

#castr::mld(df_st0_c1$density, df_st0_c1$depth, ref.depths = 2:10, criteria = c(0.03, 0.01),
#    default.depth = NA, n.smooth = 0, k = 2)

df_st0_c1 <- df_st0_c1%>%cbind(station = paste0("S0_C1"))%>%
  cbind(mld = mld_m(df_st0_c1, density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))
df_st1_c1 <- df_st1_c1%>%cbind(station = paste0("S1_C1"))%>%
  cbind(mld = mld_m(df_st1_c1, density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))
df_st2_c1 <- df_st2_c1%>%cbind(station = paste0("S2_C1"))%>%
  cbind(mld = mld_m(df_st2_c1, density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))
df_st3_c1 <- df_st3_c1%>%cbind(station = paste0("S3_C1"))%>%
  cbind(mld = mld_m(df_st3_c1, density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))
df_st4_c1 <- df_st4_c1%>%cbind(station = paste0("S4_C1"))%>%
  cbind(mld = mld_m(df_st4_c1,density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))
df_st5_c1 <- df_st5_c1%>%cbind(station = paste0("S5_C1"))%>%
  cbind(mld = mld_m(df_st5_c1, density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))
df_st6_c1 <- df_st6_c1%>%cbind(station = paste0("S6_C1"))%>%
  cbind(mld = mld_m(df_st6_c1, density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))

###

#create a section from the files

section1 <- rbind(df_st0_c1, df_st1_c1, df_st2_c1, df_st3_c1, df_st4_c1, df_st5_c1, df_st6_c1)

#plot the map

library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")
theme_set(theme_bw())
library("sf")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

station_df <- section1%>%group_by(station)%>%
  summarise(latitude = mean(latitude), longitude = mean(longitude))

suppressMessages(sf_use_s2(FALSE))

ggplot(data = world) +
  geom_sf() +

  geom_point(data = station_df, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-85, -60), ylim = c(30, 45), expand = FALSE)



source("C://Users/choerstm/Documents/oceanography_plots.R")


section_plot(section1, "latitude", "depth", "temperature", interpolate = TRUE, interp_method = "mba", MLD = NULL,
             xlim = c(33,37.5), xlab = "Latitude", zlab = "temperature", contour_color = "white", zscale = "plasma")

section_plot(section1, "latitude", "depth", "fluorescence", interpolate = TRUE, xlab = "Latitude",
             zlab = "fluorescence", contour_color = "white", zscale = "viridis")

section1_top100 <- section1%>%filter(depth <= 100)

section_plot(section1_top100, "latitude", "depth", "temperature", interpolate = TRUE, MLD = "mld", xlab = "Latitude",
             zlab = "temperature", contour_color = "white", zscale = "plasma")

section_plot(section1_top100, "latitude", "depth", "fluorescence", interpolate = TRUE, xlab = "Latitude",
             zlab = "fluorescence", contour_color = "white", zscale = "viridis")


files = dir("D://AE_2215/RAW/PROCESSED/", full.names = TRUE, pattern = ".cnv")

# loop through the files
ctd = list()

for (i in 1:18){
  
  ctd[[i]] = read.ctd(files[i])%>%
    #return(castr::mld(density,depth, ref.depths = 2:10, criteria = c(0.03, 0.01),
    #           default.depth = NA, n.smooth = 0, k = 2))%>%
    ctdDecimate(p = 0.2)

}

section1 = list(ctd[[1]], ctd[[2]],
               ctd[[4]],ctd[[6]], ctd[[8]],
               ctd[[9]])%>%
  as.section()

section_shallow = list(ctd[[1]],ctd[[3]], ctd[[5]],ctd[[6]], ctd[[7]], ctd[[11]], ctd[[14]])%>%
  as.section()

section2 = list(ctd[[10]],ctd[[11]], ctd[[12]], ctd[[13]], ctd[[14]],
                ctd[[15]], ctd[[16]], ctd[[17]], ctd[[18]])%>%
  as.section()

section1
section1%>%plot(which = c("map", "temperature"),
               ztype = "image", showStations = TRUE)

section_shallow%>%plot(which = c("map", "temperature"),
                ztype = "image", showStations = TRUE)


#finer control

s <- sectionGrid(section1, p='levitus')

