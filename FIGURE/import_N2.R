
require(dplyr)
require(tidyverse)
require(viridis)

#load the data

IRMS <- read.csv2("/Users/corahoerstmann/Documents/MIO_FIGURE/N2_data/synthese_MESURE_FIGURE_IRMS.csv", header = TRUE, sep = ";")

MIMS <- read.csv2("/Users/corahoerstmann/Documents/MIO_FIGURE/N2_data/synthèse_analyses_FIGURE_Mar_tot.csv", header = TRUE, sep = ";")

#The sample names are not identical so to match we need to create a column with station names


#IRMS$Station <- gsub("NISK .* FIX", "",IRMS$Name) useful command but the niskin indicates the depth so we need to add that
IRMS$ST_N <- IRMS$Name

#extract the NAs 

IRMS_NA <- IRMS%>%filter(str_detect(Name, "NAT"))
IRMS_NA$ST_N <- gsub("FIX .*", "",IRMS_NA$Name)
IRMS_NA$delta_NA = IRMS_NA$delta.k.2.

IRMS_NA$Station <- gsub("FIG ST", "",IRMS_NA$Name) 
IRMS_NA$Station <- gsub(" NISK.*", "", IRMS_NA$Station)

IRMS_NA$Niskin <- gsub(".*NISK ", "",IRMS_NA$Name) 
IRMS_NA$Niskin <- gsub(" FIX NAT", "", IRMS_NA$Niskin)



MIMS$ST_N <- gsub("st ", "ST",MIMS$ech)
MIMS$ST_N <- gsub("nisk", "NISK",MIMS$ST_N)
MIMS$ST_N <- gsub("Fix", "FIX",MIMS$ST_N)


N2_data <- dplyr::inner_join(IRMS, MIMS, by = "ST_N")
N2_data$Station <- gsub(" NISK .*", "",N2_data$ST_N)
N2_data$Station <- gsub("FIG ST", "",N2_data$Station)

N2_data$Niskin <- gsub(".*NISK ", "",N2_data$ST_N)
N2_data$Niskin <- gsub(" FIX.*", "",N2_data$Niskin)

N2_NA_CTD <- dplyr::inner_join(IRMS_NA[,c(2,15:17)], CTD_N2, by = c('Station', 'Niskin'))
N2_IRMS_CTD <- dplyr::inner_join(N2_data, CTD_N2[,c(1:4,9:22)], by = c('Station', 'Niskin'))

N2_data_all_CTD <- dplyr::left_join(N2_IRMS_CTD, N2_NA_CTD[,c(1:3,15)], by = c("Station", "Desired_Depth..m."))

#Underway

UW_N2_log <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/N2_data/N2_UW_log.csv")

IRMS_UW <- IRMS%>%filter(str_detect(Name, "UW"))
IRMS_UW$Station = IRMS_UW$Name

MIMS_UW <- MIMS%>%filter(str_detect(ech, "UW"))
MIMS_UW$ST_N <- gsub("exp.*", "", MIMS_UW$ST_N)
MIMS_UW$ST_N <- gsub("FIG UW 22 bottle #2", "FIG UW 22", MIMS_UW$ST_N)
MIMS_UW$ST_N <- gsub("bottle ", "#", MIMS_UW$ST_N)

### NOTE: THERE IS A MISMATCH BETWEEN IDENTIFIERS (wrong in MIMS) - I'll rename here but be careful!

MIMS_UW$ST_N <- gsub("FIG UW 22 bottle #2", "FIG UW 22", MIMS_UW$ST_N)

MIMS_UW$ST_N <- gsub("FIG UW 24 ", "FIG UW 28", MIMS_UW$ST_N)
MIMS_UW$ST_N <- gsub("FIG UW 33 ", "FIG UW 31", MIMS_UW$ST_N)
MIMS_UW$ST_N <- gsub("FIG UW 35 ", "FIG UW 34", MIMS_UW$ST_N)
################

#JOIN

UW_N2_data <- inner_join(IRMS_UW, UW_N2_log, by = "Station")

UW_N2_data$ST_N <- gsub("FIG UW 49 #22", "FIG UW 49", UW_N2_data$ST_N)

UW_N2_data <- left_join(UW_N2_data, MIMS_UW, by = "ST_N")
#Format the date and time to merge with ship log

UW_N2_data$Date <- anydate(UW_N2_data$Date)
UW_N2_data$Time = UW_N2_data$Time_UTC

#Merge with the ship log

Ship_log_selected <- left_join(UW_N2_data, UW_ship_log, by = c("Date", "Time"))

Ship_log_UW_N2_average <- Ship_log_selected%>%group_by(TIMESTAMP)%>%
  summarise(Temperature = mean(CR6_Temp_C), Latitude = mean(Latitude), Longitude = mean(Longitude),
            Date = mean(Date))
Ship_log_UW_N2_average$Time <- gsub(".*2022 ", "", Ship_log_UW_N2_average$TIMESTAMP)

N2_UW <- left_join(UW_N2_data, Ship_log_UW_N2_average, by = c("Date", "Time"))

#USE mean NA values for the underway

mean_NA <- mean(IRMS_NA$delta_NA)

N2_UW$mean_d_NA <- mean_NA
#tidy

rm(IRMS_NA, N2_data, N2_IRMS_CTD, N2_NA_CTD, Ship_log_selected, Ship_log_UW_N2_average, UW_N2_log, UW_N2_data,
   MIMS_UW, IRMS_UW)

#The equation after Montoya 1996 is:
#= PN enriched (nmol L-1) * (delta enriched - NA) / ((delta initial - NA) * time)

#turn into correct units
#4L and 2L incubations #molecular weight of N2 is 28.0134 g/mol and the 1000 is the conversion of g in ug and mol in nanomol

N2_data_all_CTD$PN_nmol_L <- (N2_data_all_CTD$PON_ug_.k.2..x/N2_data_all_CTD$Filtration_volume_L)*(1/14.0067)*1000 
N2_data_all_CTD$at_enriched <- (100*(0.0036764*((N2_data_all_CTD$delta.k.2./1000)+1)))/(1+(0.0036764*((N2_data_all_CTD$delta.k.2./1000)+1)))
N2_data_all_CTD$at_NA <- (100*(0.0036764*((N2_data_all_CTD$delta_NA/1000)+1)))/(1+(0.0036764*((N2_data_all_CTD$delta_NA/1000)+1)))
N2_data_all_CTD$label_percent_MIMS <- N2_data_all_CTD$X15N.14N.At..sample

N2_data_all_CTD$N2 <- (N2_data_all_CTD$PN_nmol_L * (N2_data_all_CTD$at_enriched - N2_data_all_CTD$at_NA))/((N2_data_all_CTD$label_percent_MIMS - N2_data_all_CTD$at_NA)*N2_data_all_CTD$Filtration_time_h)*24

N2_data_all_CTD$Desired_Depth..m. <- as.numeric(N2_data_all_CTD$Desired_Depth..m.)

## UNDERWAY

N2_UW$PN_nmol_L <- (N2_UW$PON_ug_.k.2./N2_UW$Filtration_volume_L)*(1/14.0067)*1000 
N2_UW$at_enriched <- (100*(0.0036764*((N2_UW$delta.k.2./1000)+1)))/(1+(0.0036764*((N2_UW$delta.k.2./1000)+1)))
N2_UW$at_NA <- (100*(0.0036764*((N2_UW$mean_d_NA/1000)+1)))/(1+(0.0036764*((N2_UW$mean_d_NA/1000)+1)))
N2_UW$label_percent_MIMS <- N2_UW$X15N.14N.At..sample

N2_UW$N2 <- (N2_UW$PN_nmol_L * (N2_UW$at_enriched - N2_UW$at_NA))/((N2_UW$label_percent_MIMS - N2_UW$at_NA)*N2_UW$Filtration_time_h)*24


#calculate mean N2 rates

N2_fromR <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/N2_data/R_N2_all_data.csv")

## Previous data

DB_N2_Strucutres <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/N2_data/Satellite_N2_DB/N2_eddies_NorthAtlantic_mean.csv")
