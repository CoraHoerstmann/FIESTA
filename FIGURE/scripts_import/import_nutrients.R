
require(dplyr)
require(tidyverse)
require(viridis)

#load the data

nutrients <- read.csv2("/Users/choerstm/Documents/MIO_FIGURE/Nutrients/CARING_btl_nutr_data_corrected_Neg_values.txt", header = TRUE, sep = "\t")

nutrients$Station <- as.numeric(nutrients$stnum)

#remove the southern transect
nutrients <- nutrients%>%filter(Station < 9)
nutrients$Station <- as.character(nutrients$Station)
#
nutrients$Niskin <- as.character(as.integer(nutrients$botnum))

#add the adctual depth to then be able to extract the biodepths

CTD_logs_nut <- CTD_logs%>%filter(Sample_type == "other")
nutrients <- left_join(nutrients, CTD_logs_nut[,c(1,12:14)], by=c("Station", "Niskin"))

nutrients$pres <- as.numeric(nutrients$pres)
nutrients$lon <- as.numeric(nutrients$lon)
nutrients$sil <- as.numeric(nutrients$sil)
nutrients$phos <- as.numeric(nutrients$phos)
nutrients$nit <- as.numeric(nutrients$nit)

nutrients$P_star <- nutrients$phos-nutrients$nit/16
nutrients$N_star <- nutrients$nit-(16*nutrients$phos)

nutrients$depth <- as.numeric(nutrients$Desired_Depth..m.)

nutrients_upper <- nutrients%>%filter(depth < 250)

#color bars are in the viridis package
#section_plot(nutrients_upper, "lon", "depth", "phos", interpolate = TRUE, MLD = NULL, xlab = "Longitude",
#             zlab = "phosphate conc. uM", contour_color = "white", zscale = "turbo")

print(section_plot(nutrients_upper, "lon", "depth", "P_star", interpolate = TRUE, MLD = NULL, xlab = "Longitude",
             zlab = "P*", contour_color = "white", zscale = "turbo"))

#section_plot(nutrients_upper, "lon", "depth", "N_star", interpolate = TRUE, MLD = NULL, xlab = "Longitude",
#             zlab = "N*", contour_color = "white", zscale = "turbo")


nutrients_SFC <- nutrients%>%filter(Desired_Depth..m. == 5)
nutrients_aDCM <- nutrients%>%filter(Desired_Depth..m. == 25)
nutrients_DCM <- nutrients%>%filter(Desired_Depth..m. == 50)
nutrients_bDCM <- nutrients%>%filter(Desired_Depth..m. == 75)
nutrients_100 <- nutrients%>%filter(Desired_Depth..m. == 100)

