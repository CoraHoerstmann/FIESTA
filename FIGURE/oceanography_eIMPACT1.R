#neccessary subscripts

source("/Users/corahoerstmann/Documents/MIO_FIGURE/scripts/oceanography_plots.R")
source("/Users/corahoerstmann/Documents/MIO_FIGURE/scripts/mld_m.R")

#packages
require(dplyr)
require(tidyverse)
require(oce)
require(ocedata)
require(anytime) #date format is unambigious for base function
require(reshape2)
require(rnaturalearth)
require(s2)


####NOTE: filenames don't match the stationnames - stationnames must be extracted from the ctd metadata!
#load the data
workdir <- "/Users/corahoerstmann/Documents/MIO_eIMPACT/eIMPACT1/CTD/all/"
fnFs.raw <- sort(list.files(workdir, pattern = ".cnv", full.names = TRUE))
preFilt_dir <- file.path(workdir,"preFilt")
fnFs.preFilt <- file.path(preFilt_dir,basename(fnFs.raw))
#GET SAMPLE NAMES
fnFs.raw
basename(fnFs.raw)
list <- as.list(fnFs.raw)
sample.names <- str_remove(basename(fnFs.raw),".cnv")
head(sample.names)
BGC2 <- list()
for (i in list) {
  sample.names <- str_remove(basename(i),".cnv")
  assign(sample.names, read.ctd(i))
}


#create dataframes
CTD_list <- sapply(ls(pattern="ctd"), function(x) get(x), simplify = FALSE)

#for (i in seq_along(CTD_list)) {
#  ab <- CTD_list[[i]]@metadata
#  ac <- ab[["station"]]
#  filename <- paste0("df_", ac)
#  a <- CTD_list[[i]]@data
#  data <- as.data.frame(a)
#  data2 <- data%>%cbind(station = paste0("ST_",ac))
#  assign(filename, data2)
#}


for (i in seq_along(CTD_list)) {
  ab <- CTD_list[[i]]@metadata
  ac <- names(CTD_list[i])
  filename <- paste0("df_eIMPACT1", ac)
  a <- CTD_list[[i]]@data
  data <- as.data.frame(a)
  data2 <- data%>%cbind(station = paste0("eIMPACT1_",ac))
  assign(filename, data2)
}


#one could calculate MLD now, but we can also leave it for now.


#create a section from the files
reduced_col <- intersect(colnames(df_eIMPACT1ctd001), colnames(df_eIMPACT1ctd022))

e1_all <- rbind(df_eIMPACT1ctd001[reduced_col], df_eIMPACT1ctd002[reduced_col], df_eIMPACT1ctd003[reduced_col], df_eIMPACT1ctd004[reduced_col],
                df_eIMPACT1ctd005[reduced_col], df_eIMPACT1ctd006[reduced_col], df_eIMPACT1ctd007[reduced_col], df_eIMPACT1ctd008[reduced_col], 
                df_eIMPACT1ctd009[reduced_col], df_eIMPACT1ctd010[reduced_col], df_eIMPACT1ctd011[reduced_col], df_eIMPACT1ctd012[reduced_col],
                df_eIMPACT1ctd013[reduced_col], df_eIMPACT1ctd014[reduced_col], df_eIMPACT1ctd015[reduced_col], df_eIMPACT1ctd016[reduced_col],
                df_eIMPACT1ctd017[reduced_col], df_eIMPACT1ctd018[reduced_col], df_eIMPACT1ctd019[reduced_col], df_eIMPACT1ctd020[reduced_col], 
                df_eIMPACT1ctd021[reduced_col], df_eIMPACT1ctd022[reduced_col], df_eIMPACT1ctd023[reduced_col], df_eIMPACT1ctd024[reduced_col],
                df_eIMPACT1ctd025[reduced_col], df_eIMPACT1ctd026[reduced_col], df_eIMPACT1ctd027[reduced_col], df_eIMPACT1ctd028[reduced_col],
                df_eIMPACT1ctd029[reduced_col], df_eIMPACT1ctd030[reduced_col], df_eIMPACT1ctd031[reduced_col], df_eIMPACT1ctd032[reduced_col],
                df_eIMPACT1ctd033[reduced_col], df_eIMPACT1ctd034[reduced_col], df_eIMPACT1ctd035[reduced_col], df_eIMPACT1ctd036[reduced_col],
                df_eIMPACT1ctd037[reduced_col], df_eIMPACT1ctd038[reduced_col], df_eIMPACT1ctd039[reduced_col], df_eIMPACT1ctd040[reduced_col], 
                df_eIMPACT1ctd041[reduced_col], df_eIMPACT1ctd042[reduced_col], df_eIMPACT1ctd043[reduced_col], df_eIMPACT1ctd044[reduced_col],
                df_eIMPACT1ctd045[reduced_col], df_eIMPACT1ctd046[reduced_col], df_eIMPACT1ctd047[reduced_col], df_eIMPACT1ctd048[reduced_col],
                df_eIMPACT1ctd049[reduced_col], df_eIMPACT1ctd050[reduced_col], df_eIMPACT1ctd051[reduced_col], df_eIMPACT1ctd052[reduced_col],
                df_eIMPACT1ctd053[reduced_col], df_eIMPACT1ctd054[reduced_col], df_eIMPACT1ctd055[reduced_col], df_eIMPACT1ctd056[reduced_col],
                df_eIMPACT1ctd057[reduced_col], df_eIMPACT1ctd058[reduced_col], df_eIMPACT1ctd059[reduced_col], df_eIMPACT1ctd060[reduced_col],
                df_eIMPACT1ctd061[reduced_col], df_eIMPACT1ctd062[reduced_col], df_eIMPACT1ctd063[reduced_col], df_eIMPACT1ctd064[reduced_col],
                df_eIMPACT1ctd065[reduced_col], df_eIMPACT1ctd066[reduced_col], df_eIMPACT1ctd067[reduced_col], df_eIMPACT1ctd068[reduced_col])

e1_all <- e1_all[!is.na(e1_all$latitude),]
e1_all <- e1_all[!is.na(e1_all$longitude),]

e1_all$Cast <- as.character(gsub("eIMPACT1_ctd", "CTD", e1_all$station))

e1_prio <- unique(eIMPACT1_DNA_prio$Cast)
e1_priority <- e1_all%>%filter(Cast %in% e1_prio)


#e1_BGC1 <- rbind(df_eIMPACT1ctd001[reduced_col], df_eIMPACT1ctd002[reduced_col], df_eIMPACT1ctd003[reduced_col], df_eIMPACT1ctd004[reduced_col],
#                df_eIMPACT1ctd005[reduced_col], df_eIMPACT1ctd006[reduced_col], df_eIMPACT1ctd007[reduced_col], df_eIMPACT1ctd008[reduced_col], 
#                df_eIMPACT1ctd009[reduced_col], df_eIMPACT1ctd010[reduced_col])

#e1_BGC2 <- rbind(df_eIMPACT1ctd037[reduced_col], df_eIMPACT1ctd038[reduced_col], df_eIMPACT1ctd039[reduced_col], df_eIMPACT1ctd040[reduced_col], 
#                df_eIMPACT1ctd041[reduced_col], df_eIMPACT1ctd042[reduced_col], df_eIMPACT1ctd043[reduced_col], df_eIMPACT1ctd044[reduced_col],
#                df_eIMPACT1ctd045[reduced_col], df_eIMPACT1ctd046[reduced_col], df_eIMPACT1ctd047[reduced_col], df_eIMPACT1ctd048[reduced_col],
#                df_eIMPACT1ctd049[reduced_col], df_eIMPACT1ctd050[reduced_col], df_eIMPACT1ctd051[reduced_col], df_eIMPACT1ctd052[reduced_col],
#                df_eIMPACT1ctd053[reduced_col], df_eIMPACT1ctd054[reduced_col], df_eIMPACT1ctd055[reduced_col], df_eIMPACT1ctd056[reduced_col],
#                df_eIMPACT1ctd057[reduced_col], df_eIMPACT1ctd058[reduced_col], df_eIMPACT1ctd059[reduced_col], df_eIMPACT1ctd060[reduced_col],
#                df_eIMPACT1ctd061[reduced_col], df_eIMPACT1ctd062[reduced_col])

#e1_TSS <- rbind(df_eIMPACT1ctd013[reduced_col], df_eIMPACT1ctd014[reduced_col], df_eIMPACT1ctd015[reduced_col], df_eIMPACT1ctd016[reduced_col],
#                df_eIMPACT1ctd017[reduced_col], df_eIMPACT1ctd018[reduced_col], df_eIMPACT1ctd019[reduced_col], 
#                 df_eIMPACT1ctd031[reduced_col])


#e1_TSS_extra <- rbind(df_eIMPACT1ctd011[reduced_col], df_eIMPACT1ctd012[reduced_col], df_eIMPACT1ctd020[reduced_col],
#                      df_eIMPACT1ctd021[reduced_col], df_eIMPACT1ctd022[reduced_col],
#                df_eIMPACT1ctd023[reduced_col], df_eIMPACT1ctd024[reduced_col],
#                df_eIMPACT1ctd025[reduced_col], df_eIMPACT1ctd026[reduced_col], 
#                df_eIMPACT1ctd027[reduced_col], df_eIMPACT1ctd028[reduced_col], 
#                df_eIMPACT1ctd029[reduced_col], df_eIMPACT1ctd030[reduced_col], df_eIMPACT1ctd032[reduced_col],
#                df_eIMPACT1ctd033[reduced_col], df_eIMPACT1ctd034[reduced_col],
#                df_eIMPACT1ctd035[reduced_col], df_eIMPACT1ctd036[reduced_col])

#delete rows where lat/long din't work


#plot the map

theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

station_df <- e1_all%>%group_by(station)%>%
  summarise(latitude = mean(latitude), longitude = mean(longitude))

station_df_P <- e1_priority%>%group_by(station)%>%
  summarise(latitude = mean(latitude), longitude = mean(longitude))

suppressMessages(sf_use_s2(FALSE))

stationmap_eIMPACT1<- ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = station_df_P, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-22, -14), ylim = c(24, 30), expand = FALSE)



print(stationmap_eIMPACT1)
#######
stations_eIMPACT1_DNA <- inner_join(e1_all, eIMPACT1_DNA, by = c("Cast", "pressure"))
station_df <- stations_eIMPACT1_DNA%>%group_by(station)%>%
  summarise(latitude = mean(latitude), longitude = mean(longitude))

stationmap_eIMPACT1<- ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = station_df, aes(x = longitude, y = latitude), size = 1, 
             shape = 25, fill = "orange") +
  coord_sf(xlim = c(-22, -14), ylim = c(24, 30), expand = FALSE)
print(stationmap_eIMPACT1)
#######

stationmap_eIMPACT1_BGC1<- ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = station_df_BGC1, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-22, -14), ylim = c(24, 30), expand = FALSE)

print(stationmap_eIMPACT1_BGC1)

stationmap_eIMPACT1_BGC2<- ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = station_df_BGC2, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "darkblue") +
  coord_sf(xlim = c(-22, -14), ylim = c(24, 30), expand = FALSE)

print(stationmap_eIMPACT1_BGC2)

stationmap_eIMPACT1_TSS<- ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = station_df_TSS, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "orange") +
  coord_sf(xlim = c(-22, -14), ylim = c(24, 30), expand = FALSE)

print(stationmap_eIMPACT1_TSS)

stationmap_eIMPACT1_TSS_extra<- ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = station_df_TSS_extra, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "orange") +
  coord_sf(xlim = c(-22, -14), ylim = c(24, 30), expand = FALSE)

print(stationmap_eIMPACT1_TSS_extra)


eIMPACT1_CTD <- sapply(ls(pattern="ctd0"), function(x) get(x), simplify = FALSE)
eIMPACT1_CTD_df <- sapply(ls(pattern="df_eIMPACT1ctd"), function(x) get(x), simplify = FALSE)
rm(list = ls(pattern="df_eIMACT1ctd"))
rm(list = ls(pattern="ctd0"))
#write.csv(station_df, "eIMPACT_CTD_stations.csv")



##Sectionplots

section_plot(e1_BGC1, "longitude", "depth", "temperature", interpolate = TRUE, interp_method = "mba", MLD = NULL,
             xlim = c(-17,-15.5), ylim = c(1500, 0), xlab = "Longitude", zlab = "temperature", contour_color = "white", zscale = "plasma")

section_plot(e1_BGC1, "longitude", "depth", "salinity", interpolate = TRUE, interp_method = "mba", MLD = NULL,
             xlim = c(-17,-15.5), ylim = c(1500, 0), xlab = "Longitude", zlab = "salinity", contour_color = "white", zscale = "plasma")

section_plot(e1_BGC2, "longitude", "depth", "temperature", interpolate = TRUE, interp_method = "mba", MLD = NULL,
             xlim = c(-17,-15.5), ylim = c(1500, 0), xlab = "Longitude", zlab = "temperature", contour_color = "white", zscale = "plasma")

section_plot(e1_BGC2, "longitude", "depth", "salinity", interpolate = TRUE, interp_method = "mba", MLD = NULL,
             xlim = c(-17,-15.5), ylim = c(1500, 0), xlab = "Longitude", zlab = "salinity", contour_color = "white", zscale = "plasma")

