
require(geosphere)

stats_CE <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Model_output/Stats_cyclonic_eddies_atlas.csv")
stats_ACE <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Model_output/Stats_AC_atlas.csv")
model_polygons <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Model_output/Model_Polygons.csv")
stats_CE_summer <- stats_CE%>%filter(Start.Time..month. == c(1,2,3))

##N2 values that will be used to calculate N input
N2_mean_all_data_eddy_C <- N2_eddies_NorthAtlantic_mean_annotated%>%filter(str_detect(Structure, "NA-C"))
N2_mean_all_data_eddy_C <- N2_mean_all_data_eddy_C%>%filter(DEPTH..m. < 100)
N2_mean_all_data_eddy_C <- N2_mean_all_data_eddy_C%>%filter(LATITUDE > 32.5)
N2_mean_all_data_eddy_C_SFC <- N2_mean_all_data_eddy_C%>%filter(DEPTH..m. < 10)
N2_mean_all_data_eddy_C_below <- N2_mean_all_data_eddy_C%>%filter(DEPTH..m. > 10)

N2_mean_all_data_eddy_AC <- N2_eddies_NorthAtlantic_mean_annotated%>%filter(str_detect(Structure, "NA-AC"))
N2_mean_all_data_eddy_AC <- N2_mean_all_data_eddy_AC%>%filter(DEPTH..m. < 100)
N2_mean_all_data_eddy_AC <- N2_mean_all_data_eddy_AC%>%filter(LATITUDE > 32.5)
N2_mean_all_data_eddy_AC_SFC <- N2_mean_all_data_eddy_AC%>%filter(DEPTH..m. < 10)
N2_mean_all_data_eddy_AC_below <- N2_mean_all_data_eddy_AC%>%filter(DEPTH..m. > 10)

N2_open_ocean <- N2_eddies_NorthAtlantic_mean_annotated%>%filter((str_detect(Structure, "NA-O")))
N2_open_ocean <- N2_open_ocean%>%filter(DEPTH..m. < 100)
N2_open_ocean <- N2_open_ocean%>%filter(LATITUDE > 32.5)
N2_open_ocean_SFC <- N2_open_ocean%>%filter(DEPTH..m. < 10)
N2_open_ocean_below <- N2_open_ocean%>%filter(DEPTH..m. > 10)

mean(N2_mean_all_data_eddy_C$N2)
sd(N2_mean_all_data_eddy_C$N2)
mean(N2_mean_all_data_eddy_C_SFC$N2)
sd(N2_mean_all_data_eddy_C_SFC$N2)
mean(N2_mean_all_data_eddy_C_below$N2)
sd(N2_mean_all_data_eddy_C_below$N2)

mean(N2_open_ocean$N2)
sd(N2_open_ocean$N2)
mean(N2_open_ocean_SFC$N2)
sd(N2_open_ocean_SFC$N2)
mean(N2_open_ocean_below$N2)
sd(N2_open_ocean_below$N2)

mean(N2_mean_all_data_eddy_AC$N2)
sd(N2_mean_all_data_eddy_AC$N2)
mean(N2_mean_all_data_eddy_AC_SFC$N2)
sd(N2_mean_all_data_eddy_AC_SFC$N2)
mean(N2_mean_all_data_eddy_AC_below$N2)
##########

df_C_minmax = data.frame()
df_C_minmax[6,] = c(6)
Lon <- c(print(max(stats_CE_summer$End.Longitude)), print(min(stats_CE_summer$End.Longitude)),print(min(stats_CE_summer$End.Longitude)),"-72.0", "-62.5",print(max(stats_CE_summer$End.Longitude)))
Lat <- c("33.0","33.0", "34.5", "37.7","39.0","39.0")
df_C_minmax$Lon <- as.numeric(Lon)
df_C_minmax$Lat <- as.numeric(Lat)

#function returns m2 so turn into m3 across 100m depth
geosphere::areaPolygon(df_C_minmax)*100
geosphere::areaPolygon(df_C_minmax)
#returns the CE effect range: 7.7 * 10^11

CE_box <- ggplot(data = world) +
  geom_sf() +
  geom_polygon(data = model_polygons, aes(x = C_lon, y = C_lat), size = 1, color = "blue", fill = NA)+
  #geom_polygon(data = model_polygons, aes(x = AC_lon, y = AC_lat), size = 1, color = "lightblue", fill = NA)+
  geom_polygon(data = df_C_minmax, aes(x = Lon, y = Lat), size = 1, color = "red", fill = NA)+
  geom_point(data = N2_mean_all_data_eddy_C, aes(x = LONGITUDE, y = LATITUDE), color = "darkred", size = 2)+
  geom_point(data = N2_open_ocean, aes(x = LONGITUDE, y = LATITUDE), color = "green", size = 2)+
  coord_sf(xlim = c(-85, -50), ylim = c(30, 45), expand = FALSE)

print(CE_box)

#Turn into datetime vektors

stats_CE$DateTime_Start <- paste(stats_CE$Start.Time..year., stats_CE$Start.Time..month., sep = "-")
stats_CE$DateTime_Start <- paste(stats_CE$DateTime_Start, stats_CE$Start.Time..day., sep = "-")
stats_CE$DateTime_Start <- as.Date(stats_CE$DateTime_Start)

stats_CE$DateTime_End <- paste(stats_CE$End.Time..year., stats_CE$End.Time..month., sep = "-")
stats_CE$DateTime_End <- paste(stats_CE$DateTime_End, stats_CE$End.Time..day., sep = "-")
stats_CE$DateTime_End <- as.Date(stats_CE$DateTime_End)

#calculate area in m2

stats_CE$area <- pi*stats_CE$Effective.Radius..km.^2*1000

#calculate the volume in m3 #V=πr2h

stats_CE$volume <- stats_CE$area*100

#conservative: remove those eddies that are not alive during the summer
stats_CE$eddy_ID <- paste0("eddy_", seq.int(nrow(stats_CE)))
stats_CE_conservative <- stats_CE%>%filter(End.Time..month. > 5)
stats_CE_conservative <- stats_CE_conservative%>%filter(Start.Time..month. < 10)
stats_CE_conservative$analysis <- "YES"
stats_CE_non_conservative <- stats_CE%>%filter(!eddy_ID %in% stats_CE_conservative$eddy_ID)
stats_CE_non_conservative$analysis <- "NO"
stats_CE_tableS4 <- rbind(stats_CE_conservative, stats_CE_non_conservative)
#write.csv(stats_CE_tableS4, "/Users/choerstm/Documents/MIO_FIGURE/Manuscript/SUBMISSION/REVISION1/Table_S4.csv")
#calculate averages per year


#write.csv(stats_CE_conservative, "/Users/choerstm/Documents/MIO_FIGURE/Oceanography/stats_CE_conservative.csv")
#calculate averages per year

stats_CE_conservative_average_year <- stats_CE_conservative%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.), area_mean = mean(area),
            volume_mean = mean(volume),n = n())

#Calculate N2 per m2 integrated over 100m depth

mean(N2_mean_all_data_eddy_C$N2)*100*100

#Integrate over 100m depth and receive umol N m-2 d-1
mean(N2_mean_all_data_eddy_C$N2)*100*100/1000

#159.82 umol Nm-2 d-1


N2_mean_all_data_eddy_C%>%filter(REF == "Hoerstmann")%>%
  summarise((mean(N2)/1000))
N2_mean_all_data_eddy_C%>%filter(REF == "Shao")%>%
  summarise((mean(N2)/1000))

#Big calculation
#N input is nmol N per year total across all eddies - after that it needs to be normalized to the gridbox
N2_mean_all_data_eddy_C$N2_m3 <- N2_mean_all_data_eddy_C$N2*1000
N2_open_ocean$N2_m3 <- N2_open_ocean$N2*1000
stats_CE_conservative_average_year$N_input_eddies_total <- (mean(N2_mean_all_data_eddy_C$N2_m3))*(stats_CE_conservative_average_year$Lifetime_mean*0.8)*stats_CE_conservative_average_year$volume_mean*stats_CE_conservative_average_year$n
stats_CE_conservative_average_year$N_input_eddies_total_if_no_eddy <- (mean(N2_open_ocean$N2_m3))*stats_CE_conservative_average_year$Lifetime_mean*stats_CE_conservative_average_year$volume_mean*stats_CE_conservative_average_year$n

#calculate it for the polygon for  non-eddying ocean

#only vol for eddies compared
mean(stats_CE_conservative_average_year$N_input_eddies_total)/mean(stats_CE_conservative_average_year$N_input_eddies_total_if_no_eddy)

mean(N2_open_ocean$N2)*1000
#0.0001854857

#N input in the polygon across 100m depth and per year
#4.288767e+13 m3
stats_CE_conservative_average_year$N_input_non_eddying_ocean<- (mean(N2_open_ocean$N2_m3))*areaPolygon(df_C_minmax)*100*365
#substract the area/time coverd by the eddies: returns you the m3 per year
stats_CE_conservative_average_year$Vol_year_no_eddy <- (areaPolygon(df_C_minmax)*100*365)-(stats_CE_conservative_average_year$volume_mean*stats_CE_conservative_average_year$n*stats_CE_conservative_average_year$Lifetime_mean)
stats_CE_conservative_average_year$N2input_year_noEddy_eddyOcean <- stats_CE_conservative_average_year$Vol_year_no_eddy*(mean(N2_open_ocean$N2_m3))
stats_CE_conservative_average_year$N2input_year_eddy_ocean <- stats_CE_conservative_average_year$N2input_year_noEddy_eddyOcean + stats_CE_conservative_average_year$N_input_eddies_total

#N_input_eddies_total
##This gives you the total N2 input from cyclonic eddies (without ambient waters)

#N2input_year_noEddy_eddyOcean
##This give you the total N2 input in the ambient waters (i.e., Total area- area all cyclones)

#N2input_year_eddy_ocean
##This gives you the total N2 input from eddies and ambient waters together



stats_CE_conservative_average_year_new <- stats_CE_conservative_average_year
stats_CE_conservative_average_year_new$Start.Time..year. <- as.numeric(stats_CE_conservative_average_year_new$Start.Time..year.)
stats_CE_conservative_average_year_new <- stats_CE_conservative_average_year_new%>%filter(Start.Time..year. > 2011)

#calculate the mol N per m2-1 year-1

stats_CE_conservative_average_year$N2input_year_noEddy_global_estimate <- (((stats_CE_conservative_average_year$N_input_non_eddying_ocean*100*100)/(areaPolygon(df_C_minmax)*100))/1000000000)/100
stats_CE_conservative_average_year$N2input_year_Eddy_global_estimate <- (((stats_CE_conservative_average_year$N2input_year_eddy_ocean*100*100)/(areaPolygon(df_C_minmax)*100))/1000000000)/100

##

mean(stats_CE_conservative_average_year$N2input_year_noEddy_global_estimate)

mean(stats_CE_conservative_average_year$N2input_year_Eddy_global_estimate)
sd(stats_CE_conservative_average_year$N2input_year_Eddy_global_estimate)
#Check whether times and volumes match

stats_CE_conservative_average_year$Vol_year_no_eddy+(stats_CE_conservative_average_year$volume_mean*stats_CE_conservative_average_year$n*stats_CE_conservative_average_year$Lifetime_mean)
stats_CE_conservative_average_year$fraction_eddies <-stats_CE_conservative_average_year$volume_mean/(areaPolygon(df_C_minmax)*100*365)*100
mean(stats_CE_conservative_average_year$fraction_eddies)
sd(stats_CE_conservative_average_year$fraction_eddies)
#PERCENTAGE

stats_CE_conservative_average_year$PERCENT_EddyContribution_Ninput <- (stats_CE_conservative_average_year$N2input_year_Eddy_global_estimate - stats_CE_conservative_average_year$N2input_year_noEddy_global_estimate)/stats_CE_conservative_average_year$N2input_year_Eddy_global_estimate*100
mean(stats_CE_conservative_average_year$PERCENT_EddyContribution_Ninput)
sd(stats_CE_conservative_average_year$PERCENT_EddyContribution_Ninput)


##0.058 (as in MS)
#plot?

stats_CE_conservative_average_year_longer <- stats_CE_conservative_average_year%>%gather(key = "scenario", value = "N_input", N2input_year_eddy_ocean,N_input_non_eddying_ocean)

#other units? 
stats_CE_conservative_average_year_longer$N_input_mol <- stats_CE_conservative_average_year_longer$N_input/1000000000
stats_CE_conservative_average_year_longer$N_input_Gmol <- stats_CE_conservative_average_year_longer$N_input_mol/1000000000
DB1 <- ggplot(data = stats_CE_conservative_average_year_longer, aes(x = scenario, y = N_input_Gmol))+
  geom_boxplot(aes(fill = scenario))+
  scale_fill_manual(values = c("#69f4f4", "#37378c"))+
  #geom_boxplot(width = 0.08)+
  stat_summary(fun.y=median, geom="point", size=2, color="red")

print(DB1)


#DIFFERENCE of the mol N m2 year

stats_CE_conservative_average_year$N2input_diff <- stats_CE_conservative_average_year$N2input_year_Eddy_global_estimate - stats_CE_conservative_average_year$N2input_year_noEddy_global_estimate

#do the differene in umol
mean(stats_CE_conservative_average_year$N2input_diff)*1000000
sd(stats_CE_conservative_average_year$N2input_diff)*1000000
#turn into gramm or kg

stats_CE_conservative_average_year$N2input_diff_gramm <- stats_CE_conservative_average_year$N2input_diff*14.01
#stats_CE_conservative_average_year$N2input_diff_KG <- stats_CE_conservative_average_year$N2input_diff_gramm/1000
stats_CE_conservative_average_year$N2input_diff_teragramm <- stats_CE_conservative_average_year$N2input_diff_gramm/1000000000000

mean(stats_CE_conservative_average_year$N2input_diff_teragramm)
mean(stats_CE_conservative_average_year$N2input_diff_gramm)
###############Considering all eddies throughout the year


#calculate averages per year

stats_CE_average_year <- stats_CE%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.), area_mean = mean(area),
            volume_mean = mean(volume),n = n())

#Calculate N2 per m3

mean(N2_mean_all_data_eddy_C$N2)*1000

N2_mean_all_data_eddy_C%>%filter(REF == "Hoerstmann")%>%
  summarise((mean(N2)*1000))
N2_mean_all_data_eddy_C%>%filter(REF == "Shao")%>%
  summarise((mean(N2)*1000))

#Big calculation
#N input is nmol N per year total across all eddies - after that it needs to be normalized to the gridbox

stats_CE_average_year$N_input_eddies_total <- (mean(N2_mean_all_data_eddy_C$N2)*1000)*(stats_CE_average_year$Lifetime_mean)*stats_CE_average_year$volume_mean*stats_CE_average_year$n

#N input in the polygon across 100m depth and per year
#4.288767e+13 m3
stats_CE_average_year$N_input_non_eddying_ocean<- (mean(N2_open_ocean$N2)*1000)*areaPolygon(df_C_minmax)*100*365

#substract the area/time coverd by the eddies: returns you the m3 per year
stats_CE_average_year$Vol_year_no_eddy <- (areaPolygon(df_C_minmax)*100*365)-(stats_CE_average_year$volume_mean*stats_CE_average_year$n*stats_CE_average_year$Lifetime_mean)
stats_CE_average_year$N2input_year_noEddy_eddyOcean <- stats_CE_average_year$Vol_year_no_eddy*(mean(N2_open_ocean$N2)*1000)
stats_CE_average_year$N2input_year_eddy_ocean <- stats_CE_average_year$N2input_year_noEddy_eddyOcean + stats_CE_average_year$N_input_eddies_total

#Check whether times and volumes match

stats_CE_average_year$Vol_year_no_eddy+(stats_CE_average_year$volume_mean*stats_CE_average_year$n*stats_CE_average_year$Lifetime_mean)
areaPolygon(df_C_minmax)*100*365
#PERCENTAGE

stats_CE_average_year$PERCENT_EddyContribution_Ninput <- (stats_CE_average_year$N2input_year_eddy_ocean - stats_CE_average_year$N_input_non_eddying_ocean)/stats_CE_average_year$N2input_year_eddy_ocean*100
mean(stats_CE_average_year$PERCENT_EddyContribution_Ninput)
#plot?

stats_CE_average_year_longer <- stats_CE_average_year%>%gather(key = "scenario", value = "N_input", N2input_year_eddy_ocean,N_input_non_eddying_ocean)


rm(DB1, DB2)
################
#Anticyclonic eddies
################
#eventually the maps should show the locations of sampling points

df_AC_minmax = data.frame()
df_AC_minmax[5,] = c(5)
Lon <- c(print(max(stats_ACE$End.Longitude)), print(min(stats_ACE$End.Longitude)),"-72.0", "-62.5",print(max(stats_ACE$End.Longitude)))
Lat <- c(print(min(stats_ACE$End.Latitude)), print(min(stats_ACE$End.Latitude)), "37.7","39.0","39.0")
df_AC_minmax$Lon <- as.numeric(Lon)
df_AC_minmax$Lat <- as.numeric(Lat)

areaPolygon(df_AC_minmax)

N2_mean_all_data_eddy_AC$LATITUDE <- as.numeric(N2_mean_all_data_eddy_AC$LATITUDE)
N2_mean_all_data_eddy_AC$LONGITUDE <- as.numeric(N2_mean_all_data_eddy_AC$LONGITUDE)

ACE_box <- ggplot(data = world) +
  geom_sf() +
  #geom_polygon(data = model_polygons, aes(x = C_lon, y = C_lat), size = 1, color = "blue", fill = NA)+
  geom_polygon(data = model_polygons, aes(x = AC_lon, y = AC_lat), size = 1, color = "lightblue", fill = NA)+
  geom_polygon(data = df_AC_minmax, aes(x = Lon, y = Lat), size = 1, color = "red", fill = NA)+
  geom_point(data = N2_mean_all_data_eddy_AC, aes(x = LONGITUDE, y = LATITUDE), size = 2, color = "darkred")+
  geom_point(data = N2_open_ocean, aes(x = LONGITUDE, y = LATITUDE), color = "green", size = 2)+
  coord_sf(xlim = c(-85, -50), ylim = c(30, 45), expand = FALSE)

print(ACE_box)

#Turn into datetime vektors

stats_ACE$DateTime_Start <- paste(stats_ACE$Start.Time..year., stats_ACE$Start.Time..month., sep = "-")
stats_ACE$DateTime_Start <- paste(stats_ACE$DateTime_Start, stats_ACE$Start.Time..day., sep = "-")
stats_ACE$DateTime_Start <- as.Date(stats_ACE$DateTime_Start)

stats_ACE$DateTime_End <- paste(stats_ACE$End.Time..year., stats_ACE$End.Time..month., sep = "-")
stats_ACE$DateTime_End <- paste(stats_ACE$DateTime_End, stats_ACE$End.Time..day., sep = "-")
stats_ACE$DateTime_End <- as.Date(stats_ACE$DateTime_End)

#calculate area in m2

stats_ACE$area <- pi*stats_ACE$Effective.Radius..km.^2*1000

mean(stats_ACE$area)
sd(stats_ACE$area)

#calculate the volume in m3 #V=πr2h

stats_ACE$volume <- stats_ACE$area*100

#conservative: remove those eddies that are not alive during the summer
stats_ACE$eddy_ID <- paste0("eddy_", seq.int(nrow(stats_ACE)))
stats_ACE_conservative <- stats_ACE%>%filter(End.Time..month. > 5)
stats_ACE_conservative <- stats_ACE_conservative%>%filter(Start.Time..month. < 10)
stats_ACE_conservative$analysis <- "YES"
stats_ACE_non_conservative <- stats_ACE%>%filter(!eddy_ID %in% stats_ACE_conservative$eddy_ID)
stats_ACE_non_conservative$analysis <- "NO"
stats_ACE_tableS4 <- rbind(stats_ACE_conservative, stats_ACE_non_conservative)
#write.csv(stats_ACE_tableS4, "/Users/choerstm/Documents/MIO_FIGURE/Manuscript/SUBMISSION/REVISION1/stats_ACE_conservative.csv")
#calculate averages per year

stats_ACE_conservative_average_year <- stats_ACE_conservative%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.), area_mean = mean(area),
            volume_mean = mean(volume),n = n())


mean(N2_mean_all_data_eddy_AC$N2)*1000

N2_mean_all_data_eddy_AC%>%filter(REF == "Hoerstmann")%>%
  summarise((mean(N2)*1000))
N2_mean_all_data_eddy_AC%>%filter(REF == "Shao")%>%
  summarise((mean(N2)*1000))

#Big calculation
#N input is nmol N per year total across all eddies - after that it needs to be normalized to the gridbox

stats_ACE_conservative_average_year$N_input_eddies_total <- (mean(N2_mean_all_data_eddy_AC$N2)*1000)*(stats_ACE_conservative_average_year$Lifetime_mean)*stats_ACE_conservative_average_year$volume_mean*stats_ACE_conservative_average_year$n #*0.8 if we are conservative and reduceit to mature phase only

#calculate it for the polygon for  non-eddying ocean

mean(N2_open_ocean$N2)*1000
#0.0001854857

#N input in the polygon across 100m depth and per year
#4.288767e+13 m3
stats_ACE_conservative_average_year$N_input_non_eddying_ocean<- (mean(N2_open_ocean$N2)*1000)*areaPolygon(df_AC_minmax)*100*365

#substract the area/time covered by the eddies: returns you the m3 per year
stats_ACE_conservative_average_year$Vol_year_no_eddy <- (areaPolygon(df_AC_minmax)*100*365)-(stats_ACE_conservative_average_year$volume_mean*stats_ACE_conservative_average_year$n*stats_ACE_conservative_average_year$Lifetime_mean)
stats_ACE_conservative_average_year$N2input_year_noEddy_eddyOcean <- stats_ACE_conservative_average_year$Vol_year_no_eddy*(mean(N2_open_ocean$N2)*1000)
stats_ACE_conservative_average_year$N2input_year_eddy_ocean <- stats_ACE_conservative_average_year$N2input_year_noEddy_eddyOcean + stats_ACE_conservative_average_year$N_input_eddies_total

#Check whether times and volumes match

stats_ACE_conservative_average_year$Vol_year_no_eddy+(stats_ACE_conservative_average_year$volume_mean*stats_ACE_conservative_average_year$n*stats_ACE_conservative_average_year$Lifetime_mean)
areaPolygon(df_AC_minmax)*100*365
#PERCENTAGE

#calculate the mol N per m2-1 year-1

stats_ACE_conservative_average_year$N2input_year_noEddy_global_estimate <- (((stats_ACE_conservative_average_year$N_input_non_eddying_ocean*100*100)/(areaPolygon(df_AC_minmax)*100))/1000000000)/100
stats_ACE_conservative_average_year$N2input_year_Eddy_global_estimate <- (((stats_ACE_conservative_average_year$N2input_year_eddy_ocean*100*100)/(areaPolygon(df_AC_minmax)*100))/1000000000)/100


stats_ACE_conservative_average_year_new <- stats_ACE_conservative_average_year%>%filter(Start.Time..year. > 2011)


#PERCENTAGE

stats_ACE_conservative_average_year$PERCENT_EddyContribution_Ninput <- (stats_ACE_conservative_average_year$N2input_year_Eddy_global_estimate - stats_ACE_conservative_average_year$N2input_year_noEddy_global_estimate)/stats_ACE_conservative_average_year$N2input_year_Eddy_global_estimate*100
mean(stats_ACE_conservative_average_year$PERCENT_EddyContribution_Ninput)
sd(stats_ACE_conservative_average_year$PERCENT_EddyContribution_Ninput)

#DIFFERENCE of the mol N m2 year

stats_ACE_conservative_average_year$N2input_diff <- stats_ACE_conservative_average_year$N2input_year_Eddy_global_estimate - stats_ACE_conservative_average_year$N2input_year_noEddy_global_estimate

##Overall difference:

mean(stats_ACE_conservative_average_year$N2input_diff) + mean(stats_CE_conservative_average_year$N2input_year_Eddy_global_estimate)

#do the differene in umol
mean(stats_ACE_conservative_average_year$N2input_diff)*1000000
sd(stats_ACE_conservative_average_year$N2input_diff)*1000000

##Total differences
mean(stats_ACE_conservative_average_year$N2input_diff)*1000000 + mean(stats_CE_conservative_average_year$N2input_diff)*1000000
sd(stats_ACE_conservative_average_year$N2input_diff)*1000000 + sd(stats_CE_conservative_average_year$N2input_diff)*1000000
#turn into gramm or kg

stats_ACE_conservative_average_year$N2input_diff_gramm <- stats_ACE_conservative_average_year$N2input_diff*14.01
stats_ACE_conservative_average_year$N2input_diff_KG <- stats_ACE_conservative_average_year$N2input_diff_gramm/1000


mean(stats_ACE_conservative_average_year$N2input_diff_gramm)


stats_ACE_conservative_average_year_longer <- stats_ACE_conservative_average_year%>%gather(key = "scenario", value = "N_input", N2input_year_eddy_ocean,N_input_non_eddying_ocean)

#convert to Gmol
stats_ACE_conservative_average_year_longer$N_input_mol <- stats_ACE_conservative_average_year_longer$N_input/1000000000
stats_ACE_conservative_average_year_longer$N_input_Gmol <- stats_ACE_conservative_average_year_longer$N_input_mol/1000000000



DB1 <- ggplot(data = stats_ACE_conservative_average_year_longer, aes(x = scenario, y = N_input_Gmol))+
  geom_boxplot(aes(fill = scenario))+
  scale_fill_manual(values = c("#69f4f4", "#37378c"))+
  #geom_boxplot(width = 0.08)+
  stat_summary(fun.y=median, geom="point", size=2, color="red")

print(DB1)

