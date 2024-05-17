u <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/ADCP_4Cora_nb_u.csv", sep = ";")
v <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/ADCP_4Cora_nb_v.csv", sep = ";")
w <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/allbins_w.txt", sep = ";")
#ADCP_direction <- (180/3.14) * atan2(u[,c(3)],v[,c(3)])
u[u==1e+38] <- NA
v[v==1e+38] <- NA
geo_dir <- function(vector1,vector2) (180/3.14) * atan2(vector1,vector2)
ADCP_direction <-as.data.frame(mapply(geo_dir, u[3:62], v[3:62]))
geo_speed <- function(vec1, vec2) sqrt(vec1^2 + vec2^2)
ADCP_speed <- as.data.frame(mapply(geo_speed, u[3:62], v[3:62]))

ADCP_direction$Latitude <- u$X..lat
ADCP_direction$Longitude <- u$lon

ADCP_speed$Latitude <- u$X..lat
ADCP_speed$Longitude <- u$lon

w$Latitude_2 <- round(w$lat, digits = 2)
w$Longitude_2 <- round(w$lon, digits = 2)

ADCP_direction$Latitude_2 <- round(ADCP_direction$Latitude, digits = 2)
ADCP_direction$Longitude_2 <- round(ADCP_direction$Longitude, digits = 2)

ADCP_speed$Latitude_2 <- round(ADCP_speed$Latitude, digits = 2)
ADCP_speed$Longitude_2 <- round(ADCP_speed$Longitude, digits = 2)

##rename columnnames for merging later

ADCP_speed <- ADCP_speed%>%rename(
  "speed_26.93" = "X26.93" ,
  "speed_42.93" = "X42.93" ,
  "speed_58.93" = "X58.93",
  "speed_74.93" = "X74.93",
  "speed_90.93" = "X90.93",
  "speed_106.93" = "X106.93"
)

ADCP_direction <- ADCP_direction%>%rename(
  "dir_26.93" = "X26.93" ,
  "dir_42.93" = "X42.93" ,
  "dir_58.93" = "X58.93",
  "dir_74.93" = "X74.93",
  "dir_90.93" = "X90.93",
  "dir_106.93" = "X106.93"
)

#reduce to samples without NA (don't do this for the w)
ADCP_direction_noNA <- na.omit(ADCP_direction[,c(1:6, 61:64)])
ADCP_speed_noNA <- na.omit(ADCP_speed[,c(1:6, 61:64)])
#reduce w to upper 100m

ADCP_w <- w[,c(1:13,103:104)]

#reduce to average from the short lat/longs

ADCP_directionR <- ADCP_direction_noNA%>%group_by(Latitude_2, Longitude_2)%>%
  summarise(dir_m26.93 = mean(dir_26.93), dir_m42.93 = mean(dir_42.93), dir_m58.93 = mean(dir_58.93),
            dir_m74.93 = mean(dir_74.93), dir_m90.93 = mean(dir_90.93), dir_m106.93 = mean(dir_106.93))

ADCP_speedR <- ADCP_speed_noNA%>%group_by(Latitude_2, Longitude_2)%>%
  summarise(speed_m26.93 = mean(speed_26.93), speed_m42.93 = mean(speed_42.93), speed_m58.93 = mean(speed_58.93),
            speed_m74.93 = mean(speed_74.93), speed_m90.93 = mean(speed_90.93), speed_m106.93 = mean(speed_106.93))
#calculate overall vertical velocity/ station
ADCP_w$w_mean <- rowMeans(subset(ADCP_w, select = c("X19.54", "X27.54", "X35.54", "X43.54", "X51.54", 
                                                    "X59.54", "X67.54", "X75.54", "X83.54", "X91.54", "X99.54")), na.rm = TRUE)

ADCP_wR <- ADCP_w%>%group_by(Latitude_2, Longitude_2)%>%
  summarise(w_m19.54 = mean(X19.54), w_m27.54 = mean(X27.54), w_m35.54 = mean(X35.54), w_m43.54 = mean(X43.54),
            w_m51.54 = mean(X51.54), w_m59.54 = mean(X59.54), w_m67.54 = mean(X67.54), w_m75.54 = mean(X75.54),
            w_m83.54 = mean(X83.54), w_m91.54 = mean(X91.54), w_m99.54 = mean(X99.54), mean_w = mean(w_mean))

#dig out for those with NAs in the summary

#33.19461689	-67.85090169
#33.24013861	-68.04066222
#33.26662286	-68.13333614
#33.29364064	-68.22603925
#33.39841706	-68.63341711
#33.44849311	-68.82400425
#35.10056014	-70.99782669
#36.10810458	-72.14297811
#36.48643794	-72.27495922
#36.40472503	-72.27415756
#35.56504486	-71.55418272


##for 16S
#33.95498897	-69.55942881
#37.02123053	-73.16228958
#35.58729567	-71.72997564
#ADCP_w%>%filter(Latitude_2 == 35.59)
#ADCP_speed_noNA%>%filter(Latitude_2 == 35.59)
#ADCP_direction_noNA%>%filter(Latitude_2 == 35.59)

#reduce to lower resolution to capture the underway data, too

#ADCP_directionR$Latitude_3 <- round(ADCP_directionR$Latitude_2, digits = 1)
#ADCP_directionR$Longitude_3 <- round(ADCP_directionR$Longitude_2, digits = 1)

#ADCP_speedR$Latitude_3 <- round(ADCP_speedR$Latitude_2, digits = 1)
#ADCP_speedR$Longitude_3 <- round(ADCP_speedR$Longitude_2, digits = 1)

#ADCP_directionR <- ADCP_directionR%>%group_by(Latitude_3, Longitude_3)%>%
#  summarise(dir_m26.93 = mean(dir_m26.93), dir_m42.93 = mean(dir_m42.93), dir_m58.93 = mean(dir_m58.93),
#            dir_m74.93 = mean(dir_m74.93), dir_m90.93 = mean(dir_m90.93), dir_m106.93 = mean(dir_m106.93))

#ADCP_speedR <- ADCP_speedR%>%group_by(Latitude_3, Longitude_3)%>%
#  summarise(speed_m26.93 = mean(speed_m26.93), speed_m42.93 = mean(speed_m42.93), speed_m58.93 = mean(speed_m58.93),
#            speed_m74.93 = mean(speed_m74.93), speed_m90.93 = mean(speed_m90.93), speed_m106.93 = mean(speed_m106.93))

#write.csv(ADCP_directionR, "FIGURE_adcp_direction.csv")
#write.csv(ADCP_speedR, "FIGURE_adcp_speed.csv")
