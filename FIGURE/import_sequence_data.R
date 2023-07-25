nifH_ASV <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/nifH_20230410/seqtab_all_FIGURE_nifH.csv", row.names = 1)
nifH_taxonomy_v1 <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/nifH_20230410/ASVs_Taxonomy_FIGURE_nifH20230411.tsv", sep = "\t")
nifH_taxonomy_v2 <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/nifH_20230410/ASVs_Taxonomy_FIGURE_nifH20230411_v2_0_5.tsv", sep = "\t")
rownames(nifH_taxonomy_v2) <- nifH_taxonomy_v2$X

colnames(nifH_ASV) <- gsub("X", "", colnames(nifH_ASV))
colnames(nifH_ASV) <- gsub(".nifH", "", colnames(nifH_ASV))

nif_meta <-  DNA_meta%>%filter(No %in% colnames(nifH_ASV))
nifH_meta2 <- left_join(DNA_meta, all_data, by = "No")
nif_meta2 <- nifH_meta2%>%group_by(No)%>%
  summarise(mean_log_Tricho = mean(log_Tricho), mean_log_GammaA = mean(log_GammaA), mean_log_UCYNA = mean(log_UCYN_A1),
            mean_N2 = mean(N2), mean_CN = mean(C_N))
nif_meta2t <- left_join(nif_meta2, DNA_meta[,c(2:5,8,15)], by = "No")

nif_meta$Date <- as.Date(nif_meta$Date, "%d/%m/%Y")
nif_meta$Time <- nif_meta$Time_UTC
nif_meta_f <- left_join(nif_meta, UW_ship_log, by = c("Time", "Date"))

#nif_meta_f$Latitude_2 <- round(nif_meta_f$Latitude, digits = 1)
#nif_meta_f$Longitude_2 <- round(nif_meta_f$Longitude, digits = 1)

#nif_meta_f <- left_join(nif_meta_f, ADCP, by = c("Latitude_2", "Longitude_2"))
  
  

nifh_meta_f <- nif_meta_f%>%group_by(No)%>%
  summarise(Temperature = mean(CR6_Temp_C), Latitude = mean(Latitude), Longitude = mean(Longitude),
            Date = mean(Date))
nifH_meta <- left_join(nifh_meta_f, nif_meta2t, by = "No")



### add depth as an important variable


nifhM_UW <- nifH_meta%>%filter(str_detect(ID, "_UW"))%>%
  cbind(Desired_Depth..m. = paste0("5"))
nifhM_UW$Desired_Depth..m. <- as.numeric(nifhM_UW$Desired_Depth..m.)
nifHM_CTD <- nifH_meta%>%filter(!str_detect(ID, "_UW"))
nifHM_CTD$Station <- gsub("St ", "", nifHM_CTD$Sampling_method)
nifHM_CTD$Station <- gsub(" C.*", "", nifHM_CTD$Station)
nifHM_CTD <- left_join(nifHM_CTD, CTD_N2[,c(1,9:14,16)], by = c('Station', 'Niskin'))
nifHMM_CTD <- nifHM_CTD[,c(1,3:15,20,22)]
nifHMM_CTD$Temperature <- nifHMM_CTD$Temperature.at
nifHMM_CTD$Temperature.at <- NULL
##add nutrients to the 
nifHM_CTD$Desired_Depth..m. <- as.numeric(nifHM_CTD$Desired_Depth..m.)
nutrients$Desired_Depth..m. <- as.numeric(nutrients$Desired_Depth..m.)
nifHM_CTD <- left_join(nifHM_CTD, nutrients[,c(10,12,14,16,18,20,22)], by = c("Station", "Desired_Depth..m."))

#there were two duplicated to these rows need to be removed
nifHM_CTD <- nifHM_CTD[-c(12,25),]

nifH_meta <- rbind(nifhM_UW, nifHMM_CTD)

nifH_meta$Latitude_2 <- round(nifH_meta$Latitude, digits = 2)
nifH_meta$Longitude_2 <- round(nifH_meta$Longitude, digits = 2)

nifH_meta <- left_join(nifH_meta, ADCP_directionR, by = c("Latitude_2", "Longitude_2"))
nifH_meta <- left_join(nifH_meta, ADCP_speedR, by = c("Latitude_2", "Longitude_2"))
nifH_meta_withW <- left_join(nifH_meta, ADCP_wR, by = c("Latitude_2", "Longitude_2"))
#export and manually assign the correct ADCP to each sample according to depth
##note also that the first UW samples and Station 8 ADCP was taken slightly off since ADCP didn't record at the station directly
#write.csv(nifH_meta_withW, "nifH_meta_all_metadata_joinedInR_W.csv")

nifH_meta <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/nifH_meta_all_metadata_joinedInR.csv")

##CLEAN UP 
rm(nif_meta, nifH_meta2, nifh_meta_f, nif_meta2, nif_meta_f, nif_meta2t, nifH_meta_withW)
