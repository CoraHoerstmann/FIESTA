
DNA_meta <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/FIGURE_DNA_mastersheet.csv")
DNA_meta$Int_Order <- sprintf("%03d", DNA_meta$Int_Order)

qPCR <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/FIGURE_qPCR.csv")


#####

qPCR_1 <- left_join(qPCR, DNA_meta, by = "No")
qPCR_1$TIMESTAMP <- paste(qPCR_1$Date, qPCR_1$Time_UTC, sep = " ")
lapply(qPCR_1, class)%>%unlist()


#Tricho
qPCR_1$Tricho_gene.copies.ul.extract <- as.numeric(qPCR_1$Tricho_gene.copies.ul.extract)
qPCR_1$Tricho_avg.g.c..ul.extract <- as.numeric(qPCR_1$Tricho_avg.g.c..ul.extract)
qPCR_1$Volume_ml <- as.numeric(qPCR_1$Volume_ml)
qPCR_1$Dilution.factor <- as.numeric(qPCR_1$Dilution.factor)

qPCR_1$Tricho_g.c._L_Dil_samples <- (qPCR_1$Tricho_avg.g.c..ul.extract*50/qPCR_1$Volume_ml)*1000
qPCR_1$Tricho_g.c._L_seawater <- qPCR_1$Tricho_g.c._L_Dil_samples*qPCR_1$Dilution.factor

#UCYN-A
qPCR_1$UCYN_A1_gene.copies.ul.extract <- as.numeric(qPCR_1$UCYN_A1_gene.copies.ul.extract)
qPCR_1$UCYN_A1_avg.g.c..ul.extract <- as.numeric(qPCR_1$UCYN_A1_avg.g.c..ul.extract)


qPCR_1$UCYN_A1_g.c._L_Dil_samples <- (qPCR_1$UCYN_A1_avg.g.c..ul.extract*50/qPCR_1$Volume_ml)*1000
qPCR_1$UCYN_A1_g.c._L_seawater <- qPCR_1$UCYN_A1_g.c._L_Dil_samples*qPCR_1$Dilution.factor


#GammaA
qPCR_1$GammaA_gene.copies.ul.extract <- as.numeric(qPCR_1$GammaA_gene.copies.ul.extract)
qPCR_1$GammaA_avg.g.c..ul.extract <- as.numeric(qPCR_1$GammaA_avg.g.c..ul.extract)


qPCR_1$GammaA_g.c._L_Dil_samples <- (qPCR_1$GammaA_avg.g.c..ul.extract*50/qPCR_1$Volume_ml)*1000
qPCR_1$GammaA_g.c._L_seawater <- qPCR_1$GammaA_g.c._L_Dil_samples*qPCR_1$Dilution.factor

#write.csv(qPCR_1, "FIGURE_qPCR_all.csv")
###


#SEPARATE BETWEEN UW AND CTD TO LINK TO METADATA

qPCR_1_UW <- qPCR_1%>%filter(str_detect(ID, "_UW_"))
qPCR_1_CTD <- qPCR_1%>%filter(!str_detect(ID, "_UW_"))

Shiplog_qPCR <- left_join(qPCR_1_UW, UW_ship_log, by = "TIMESTAMP")


Shiplog_qPCR <- Shiplog_qPCR%>%group_by(TIMESTAMP)%>%
  summarise(Temperature = mean(CR6_Temp_C), Latitude = mean(Latitude), Longitude = mean(Longitude),
            Date = mean(Date.y))
Shiplog_qPCR$Time_UTC <- gsub(".*2022 ", "", Shiplog_qPCR$TIMESTAMP)
Shiplog_qPCR$Date <- gsub(" .*", "", Shiplog_qPCR$TIMESTAMP)

qPCR_UW <- left_join(qPCR_1_UW, Shiplog_qPCR, by = c("Date", "Time_UTC"))
qPCR_UW$Desired_Depth..m. <- 5


qPCR_1_CTD$Station <- gsub("St ", "", qPCR_1_CTD$Sampling_method)
qPCR_1_CTD$Station <- gsub(" C.*", "", qPCR_1_CTD$Station)
qPCR_CTD <- left_join(qPCR_1_CTD, CTD_N2, by = c('Station', 'Niskin'))
qPCR_CTD$Temperature <- qPCR_CTD$Temperature.at

comm_qPCR <- c("No", "ID", "Latitude", "Longitude", "Tricho_g.c._L_seawater","UCYN_A1_g.c._L_seawater","GammaA_g.c._L_seawater", "Temperature", "Desired_Depth..m.")
qPCR_all <- rbind(qPCR_UW[comm_qPCR], qPCR_CTD[comm_qPCR])

qPCR_all$log_Tricho <- log10(qPCR_all$Tricho_g.c._L_seawater + 1)
qPCR_all$log_UCYN_A1 <- log10(qPCR_all$UCYN_A1_g.c._L_seawater + 1)
qPCR_all$log_GammaA <- log10(qPCR_all$GammaA_g.c._L_seawater + 1)

#To be imported and formatted in qGIS
#qPCR_red <- qPCR_all %>% drop_na()

qPCR_all <- qPCR_all %>% replace(is.na(.), 0)
qPCR_all_SFC <- qPCR_all%>%filter(Desired_Depth..m. < 10)
write.csv(qPCR_all, "FIGURE_qPCR_all_calculated.csv")
write.csv(qPCR_all, "FIGURE_qPCR_all_calculated_SFC.csv")
