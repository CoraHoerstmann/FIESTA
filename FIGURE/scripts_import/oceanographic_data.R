
#neccessary subscripts

source("/Users/choerstm/Documents/MIO_FIGURE/scripts/oceanography_plots.R")
source("/Users/choerstm/Documents/MIO_FIGURE/scripts/mld_m.R")
source("/Users/choerstm/Documents/MIO_FIGURE/scripts/pycnocline.R")
#Nutrients
nutrients <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Nutrients/CARING_btl_nutr_data.txt", sep = "\t")
nutrients$Niskin <- as.character(nutrients$botnum)
nutrients$Station <- as.character(nutrients$stnum)
#Load the underway log

UW_ship_log <- read.csv2("/Users/choerstm/Documents/MIO_FIGURE/SAMPLES/Nat_dat.csv")
UW_ship_log <- UW_ship_log[,c(1:8)]
UW_ship_log$Lat_deg <- as.numeric(UW_ship_log$Lat_deg)
UW_ship_log$Lat_min <- as.numeric(UW_ship_log$Lat_min)
UW_ship_log$Lon_deg <- as.numeric(UW_ship_log$Lon_deg)
UW_ship_log$Lon_min <- as.numeric(UW_ship_log$Lon_min)


UW_ship_log$Latitude <- UW_ship_log$Lat_deg+(UW_ship_log$Lat_min/60)
UW_ship_log$Longitude <- UW_ship_log$Lon_deg+(UW_ship_log$Lon_min/60)

lapply(UW_ship_log, class)%>%unlist()

UW_ship_log$Time <- gsub(".*2022 ", "", UW_ship_log$TIMESTAMP)
UW_ship_log$Date <- gsub(" .*", "", UW_ship_log$TIMESTAMP)
UW_ship_log$Date <- as.Date(UW_ship_log$Date, "%d/%m/%Y")
UW_ship_log$CR6_Temp_C <-as.numeric(UW_ship_log$CR6_Temp_C)

#read profiles of the 1st section
stn0_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn00_cast1.cnv")
stn1_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn01_cast1.cnv")
stn2_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn02_cast1.cnv")
stn3_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn03_cast1.cnv")
stn4_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn04_cast1.cnv")
stn5_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn05_cast1.cnv")
stn6_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn06_cast1.cnv")
#stn7_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn07_cast1.cnv")
stn8_c1 = read.ctd("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/AE2215_stn08_cast1.cnv")

#check out the variables

stn0_c1@data%>%as.data.frame()%>%dplyr::glimpse()

#plot
par(mfrow = c(1,2))
stn0_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn0_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))
stn1_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn1_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))
stn2_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn2_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))
stn3_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn3_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))
stn4_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn4_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))
stn5_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn5_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))
stn6_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn6_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))
stn8_c1%>%oce::plotProfile(plim = c(150, 0), Slim = c(34,37), Tlim = c(10,32))
stn8_c1%>%oce::plot(which = "fluorescence", cex =0.2, pch =16, plim = c(150, 0))

#save as 633 - 600
#stn0_c1_downcast = stn0_c1%>%
#ctdTrim(method = "downcast")%>% #already discarded i the seabird software
#  ctdDecimate(p = 0.2)

#par(mfrow = c(1,3))

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
df_st8_c1 <- as.data.frame(stn8_c1@data)
#remove NAs ## that was only an issue in the previously processed files. 

#discard <- apply(df_st1_c1, 1, function(x) any(is.na(x)))
#df_st1_c1 <- df_st1_c1[!discard,]

#calculate MLD



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
df_st8_c1 <- df_st8_c1%>%cbind(station = paste0("S8_C1"))%>%
  cbind(mld = mld_m(df_st8_c1, density, depth, ref.depth_min=4, ref.depth_max=8, criteria=c(0.03, 0.01)))


##add pycnocline
calculate_pycnocline(df_st0_c1$density, df_st0_c1$depth, mld = 10.166, ref.depth = 5)

df_st0_c1 <- df_st0_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st0_c1$density, df_st0_c1$depth, mld = 10.166, ref.depth = 5))
df_st1_c1 <- df_st1_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st1_c1$density, df_st1_c1$depth, mld = 8.777, ref.depth = 5))
df_st2_c1 <- df_st2_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st2_c1$density, df_st2_c1$depth, mld = 12.763, ref.depth = 5))
df_st3_c1 <- df_st3_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st3_c1$density, df_st3_c1$depth, mld = 8.472, ref.depth = 5))
df_st4_c1 <- df_st4_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st4_c1$density, df_st4_c1$depth, mld = 10.454, ref.depth = 5))
df_st5_c1 <- df_st5_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st5_c1$density, df_st5_c1$depth, mld = 12.83, ref.depth = 5))
df_st6_c1 <- df_st6_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st6_c1$density, df_st6_c1$depth, mld = 12.558, ref.depth = 5))
df_st8_c1 <- df_st8_c1%>%
  cbind(pycnocline = calculate_pycnocline(df_st8_c1$density, df_st8_c1$depth, mld = 8.014, ref.depth = 5))

###extract salinity info
readLines("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Waterwall.dat", 
          n=10)
data_TSG <- read.table("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Waterwall.dat", header=TRUE, skip=1, sep = ",") 
#remove the metadatainfo, ain't pretty but oh well
data_TSG <- data_TSG[-1,]
data_TSG <- data_TSG[-1,]

#FOR the UW
DNA_log_UW <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/DNA/FIGURE_DNA_mastersheet.csv")
DNA_log_UW <- DNA_log_UW%>%filter(Niskin == "UW")


DNA_log_UW$TIMESTAMP <- paste(DNA_log_UW$Date, DNA_log_UW$Time_UTC, sep = " ")
DNA_log_UW$TIMESTAMP2 <- as.POSIXct(DNA_log_UW$TIMESTAMP, format = "%d/%m/%Y %H:%M")
data_TSG$TIMESTAMP2 <- as.POSIXct(data_TSG$TIMESTAMP, format = "%Y-%m-%d %H:%M")
data_TSG$PriCHL_ug_l <- as.numeric(data_TSG$PriCHL_ug_l)
data_TSG$SBE45Pri_Sal_PSU <- as.numeric(data_TSG$SBE45Pri_Sal_PSU)
data_TSG$SBE45Pri_Temp_C <- as.numeric(data_TSG$SBE45Pri_Temp_C)
data_TSG2 <- data_TSG%>%group_by(TIMESTAMP2)%>%
  summarise(CHL = median(PriCHL_ug_l), SAL = median(SBE45Pri_Sal_PSU), TEMP = median(SBE45Pri_Temp_C))
data_TSG_UW <- left_join(DNA_log_UW, data_TSG2, by = "TIMESTAMP2")

# combining date and time into single object
#DNA_log_UW$TIMESTAMP <- as.POSIXct(paste(DNA_log_UW$Date, DNA_log_UW$Time_UTC), format=format)
#For CTD
Des_depth <- df_st8_c1%>%filter(between(depth, 99.9, 100.1)) 
mean(Des_depth$salinity)
#create a section from the files

section1 <- rbind(df_st0_c1, df_st1_c1, df_st2_c1, df_st3_c1, df_st4_c1, df_st5_c1, df_st6_c1, df_st8_c1)

#plot the map

theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

station_df <- section1%>%group_by(station)%>%
  summarise(latitude = mean(latitude), longitude = mean(longitude))

suppressMessages(sf_use_s2(FALSE))

stationmap <- ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = station_df, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-85, -60), ylim = c(30, 45), expand = FALSE)

print(stationmap)

## print with oceanography plots


#section_plot(section1, "longitude", "depth", "temperature", interpolate = TRUE, interp_method = "mba", MLD = NULL,
#             xlim = c(-75,-67.5), xlab = "Latitude", zlab = "temperature", contour_color = "white", zscale = "plasma")

#section_plot(section1, "latitude", "depth", "fluorescence", interpolate = TRUE, xlab = "Latitude",
#             zlab = "fluorescence", contour_color = "white", zscale = "viridis")


#section_plot(section1, "longitude", "depth", "oxygen", interpolate = TRUE, xlab = "Latitude",
#             zlab = "oxygen", contour_color = "white", zscale = "viridis")

#section_plot(section1, "longitude", "depth", "salinity", interpolate = TRUE, xlab = "Latitude",
#             zlab = "salinity", contour_color = "white", zscale = "viridis")

section1_top150 <- section1%>%filter(depth <= 155)

section1_top500 <- section1%>%filter(depth <= 500)


section_plot(section1_top150, "longitude", "depth", "temperature", interpolate = TRUE, MLD = NULL, xlab = "Longitude",
             zlab = "temperature", contour_color = "white", zscale = "plasma")

section_plot(section1_top150, "longitude", "depth", "fluorescence", interpolate = TRUE, xlab = "Longitude",
             zlab = "fluorescence", contour_color = "white", zscale = "viridis")

section_plot(section1_top150, "longitude", "depth", "salinity", interpolate = TRUE, xlab = "Longitude",
             zlab = "salinity", contour_color = "white", zscale = "viridis")


section_plot(section1_top150, "longitude", "depth", "oxygen", interpolate = TRUE, xlab = "Longitude",
             zlab = "oxygen", contour_color = "white", zscale = "viridis")



section1_top150$log_par <- log(section1_top150$par)

section_plot(section1_top150, "longitude", "depth", "log_par",contour = NULL, interpolate = TRUE, xlab = "Longitude",
             zlab = "par", contour_color = "white", zscale = "viridis")


#save as 760 x 420

##TS plot
section2 <- rbind(df_st0_c1,df_st1_c1, df_st2_c1, df_st3_c1, df_st4_c1, df_st6_c1, df_st8_c1)
section2_top150 <- section2%>%filter(depth <= 155)
ts_plot(section2, temp_col = "theta", sal_col = "salinity", WM = NULL, color = "station", symbol_size = 0.8)+
  scale_color_manual(values = c("#0f3cf7","#f45a45","#ee9a00","#f9d606","#58d805", "#3f560d","#4af496"))
ts_plot(section2_top150, temp_col = "theta", sal_col = "salinity", WM = NULL, color = "station", symbol_size = 0.8)+
  scale_color_manual(values = c("#0f3cf7","#f45a45","#ee9a00","#f9d606","#58d805", "#3f560d","#4af496"))


ts_plot(section2_top150, temp_col = "theta", sal_col = "salinity", WM = NULL, symbol_size = 0.8, color = "pressure")+
  scale_colour_gradientn(colours = c("darkred","red", "orange", "yellow", "white","#f987f1","#a375cc","#4b4bc4", "darkblue"))
ts_plot(section2, temp_col = "theta", sal_col = "salinity", WM = NULL, nlevels = 20, xlim = c(33.5, 37.5), ylim = c(0,32), color = "pressure", symbol_size = 0.02)+
  scale_colour_gradientn(colours = c("darkred","red", "orange", "yellow", "white","#f987f1","#a375cc","#4b4bc4", "darkblue", "black"),
                         breaks = c(100, 500, 1000, 1500, 2000))


#ts_plot(df_st8_c1,temp_col = "theta", sal_col = "salinity", WM = NULL, color = "pressure")

#ANOTHER WAY TO PLOT BUT UNUNSED
#CAREFUL: VERY COMPUTING INTENSE

#files = dir("/Users/choerstm/Documents/MIO_FIGURE/AE_2215/RAW/PROCESSED/", full.names = TRUE, pattern = ".cnv")

# loop through the files
#ctd = list()

#for (i in 1:18){
#  ctd[[i]] = read.ctd(files[i])%>%
    #return(castr::mld(density,depth, ref.depths = 2:10, criteria = c(0.03, 0.01),
    #           default.depth = NA, n.smooth = 0, k = 2))%>%
#    ctdDecimate(p = 0.2)
  
#}

#section1 = list(ctd[[1]], ctd[[2]],
#                ctd[[4]],ctd[[6]], ctd[[8]],
#                ctd[[9]])%>%
#  as.section()

#section1
#section1%>%plot(which = c("map", "temperature"),
#                ztype = "image", showStations = TRUE)


#finer control

#s <- sectionGrid(section1, p='levitus')



#####

#ybreaks <- pretty(range(c(section1[["theta"]], c(0, 32))), n = 20)
#ylim <- range(ybreaks)

#xbreaks <- pretty(range(c(section1[["salinity"]], c(33, 37))), n = 20)
#xlim <- range(xbreaks)

#rho <- oce::swRho(salinity = xlim, temperature = ylim, pressure = rep(1, length(xlim))) - 1000
#rho_breaks <- pretty(range(rho), n = 19)
#rho_breaks <- seq(10, 30, length.out = nlevels)
