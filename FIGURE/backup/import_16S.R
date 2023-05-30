ASV16S <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/16S_50-861646108/seqtab_all_FIGURE_16S.csv", row.names = 1)
taxonomy_16S <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/sequences/16S_50-861646108/taxonomy_FIGURE_16S_SILVAv138.1.csv", sep = ";", row.names = 1)
colnames(ASV16S) <- gsub("X", "", colnames(ASV16S))
colnames(ASV16S) <- gsub(".16S", "", colnames(ASV16S))


#filter out mitochondira and chloroplasts
taxonomy_16S_r <-taxonomy_16S%>%dplyr::filter(Family == "Mitochondria") ##192
taxonomy_16S <- taxonomy_16S%>%dplyr::filter(!rownames(taxonomy_16S) %in% rownames(taxonomy_16S_r)) 

taxonomy_16S_r <-taxonomy_16S%>%dplyr::filter(Order == "Chloroplast") ##322
taxonomy_16S <- taxonomy_16S%>%dplyr::filter(!rownames(taxonomy_16S) %in% rownames(taxonomy_16S_r)) 

rm(taxonomy_16S_r)
#reduce abundance table to what we have left from the taxonomy table

sequence_list <- as.character(taxonomy_16S$rownames)

ASV16S$ASV <- rownames(ASV16S)
ASV16S.tax <- ASV16S%>%filter(rownames(ASV16S) %in% rownames(taxonomy_16S)) 
ASV16S$ASV <- NULL
ASV16S.tax$ASV <- NULL

a16S_meta <-  DNA_meta%>%filter(No %in% colnames(ASV16S))
a16S_meta2 <- left_join(DNA_meta, all_data, by = "No")
a16S_meta2 <- a16S_meta2%>%group_by(No)%>%
  summarise(mean_log_Tricho = mean(log_Tricho), mean_log_GammaA = mean(log_GammaA), mean_log_UCYNA = mean(log_UCYN_A1),
            mean_N2 = mean(N2), mean_CN = mean(C_N))
a16S_meta2t <- left_join(a16S_meta2, DNA_meta[,c(2:5,8,15)], by = "No")

a16S_meta$Date <- as.Date(a16S_meta$Date, "%d/%m/%Y")
a16S_meta$Time <- a16S_meta$Time_UTC
a16S_meta_f <- left_join(a16S_meta, UW_ship_log, by = c("Time", "Date"))

#a16S_meta_f$Latitude_2 <- round(a16S_meta_f$Latitude, digits = 1)
#a16S_meta_f$Longitude_2 <- round(a16S_meta_f$Longitude, digits = 1)

#a16S_meta_f <- left_join(a16S_meta_f, ADCP, by = c("Latitude_2", "Longitude_2"))



a16S_meta_f <- a16S_meta_f%>%group_by(No)%>%
  summarise(Temperature = mean(CR6_Temp_C), Latitude = mean(Latitude), Longitude = mean(Longitude),
            Date = mean(Date))
a16S_meta <- left_join(a16S_meta_f, a16S_meta2t, by = "No")



### add depth as an important variable


a16SM_UW <- a16S_meta%>%filter(str_detect(ID, "_UW"))%>%
  cbind(Desired_Depth..m. = paste0("5"))
a16SM_UW$Desired_Depth..m. <- as.numeric(a16SM_UW$Desired_Depth..m.)
a16SM_CTD <- a16S_meta%>%filter(!str_detect(ID, "_UW"))
a16SM_CTD$Station <- gsub("St ", "", a16SM_CTD$Sampling_method)
a16SM_CTD$Station <- gsub(" C.*", "", a16SM_CTD$Station)
a16SM_CTD <- left_join(a16SM_CTD, CTD_N2[,c(1,9:14)], by = c('Station', 'Niskin'))
a16SMM_CTD <- a16SM_CTD[,c(1:15,20)]

##add nutrients to the 
a16SM_CTD$Desired_Depth..m. <- as.numeric(a16SM_CTD$Desired_Depth..m.)
nutrients$Desired_Depth..m. <- as.numeric(nutrients$Desired_Depth..m.)
a16SM_CTD <- left_join(a16SM_CTD, nutrients[,c(10,12,14,16,18,20,22)], by = c("Station", "Desired_Depth..m."))

#there were two duplicated to these rows need to be removed
a16SM_CTD <- a16SM_CTD[-c(12,25),]

a16S_meta <- rbind(a16SM_UW, a16SMM_CTD)

a16S_meta$Latitude_2 <- round(a16S_meta$Latitude, digits = 2)
a16S_meta$Longitude_2 <- round(a16S_meta$Longitude, digits = 2)

#aa <- left_join(a16S_meta, ADCP, by = c("Latitude_2", "Longitude_2"))

##CLEAN UP 
rm(a16SMM_CTD, a16S_meta_f, a16S_meta2, a16S_meta_f, a16S_meta2t)
