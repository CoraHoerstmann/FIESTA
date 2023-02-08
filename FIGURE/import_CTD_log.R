
CTD_logs <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/CTDCastSheets_FIGURE-CARING/SUMMARY_CTD_CASTS_FIGURE.csv")

CTD_logs$Latitude_down..min.N. <- gsub(",", ".", CTD_logs$Latitude_down..min.N.)
CTD_logs$Long_down..min.W. <- gsub(",", ".", CTD_logs$Long_down..min.W.)

CTD_logs <-CTD_logs %>%
  mutate(across(everything(), as.character))

#Calculate the Latitude/ Longitude
CTD_logs$Latitude_down..deg.N. <- as.numeric(CTD_logs$Latitude_down..deg.N.)
CTD_logs$Latitude_down..min.N. <- as.numeric(CTD_logs$Latitude_down..min.N.)
CTD_logs$Long_down..deg.W. <- as.numeric(CTD_logs$Long_down..deg.W.)
CTD_logs$Long_down..min.W. <- as.numeric(CTD_logs$Long_down..min.W.)

CTD_logs$Latitude <- CTD_logs$Latitude_down..deg.N.+(CTD_logs$Latitude_down..min.N./60)
CTD_logs$Longitude <- (CTD_logs$Long_down..deg.W.+(CTD_logs$Long_down..min.W./60))*(-1)

CTD_logs$DCM <-as.numeric(CTD_logs$DCM)
CTD_logs$MLD <-as.numeric(CTD_logs$MLD)

#add the nutrients
##NOTE: Nutrients were taken with the first cast so don't match 100% - data is linked via desired depth
CTD_nut <- left_join(CTD_logs, nutrients[,c(8:21)], by = c("Station", "Niskin"))

CTD_N2 <- CTD_nut%>%dplyr::filter(Sample_type == "N2")

CTD_N2$Desired_Depth..m. <- as.numeric(CTD_N2$Desired_Depth..m.)

CTD_logs_110 <- CTD_N2%>%filter(Desired_Depth..m. <= 110)
#convert into long format

CTD_logs_110_long <- reshape2::melt(data =  CTD_logs_110[,c(13,17,18,21)], id.vars = c("Desired_Depth..m.", "Latitude"),
                                     variable.name = "Bio_depth",
                                     value.name = "depth_m")
CTD_logs_110_long$depth_m <- as.numeric(CTD_logs_110_long$depth_m)

biodepths <- ggplot(CTD_logs_110_long,
       aes(x=Latitude, y=-(depth_m), colour=Bio_depth)) +
  geom_line()+
  scale_colour_manual(values=c("darkgreen", "black")) +
  scale_y_continuous(limits = c(-125, 0), breaks = seq(-100,0, by = 25))

print(biodepths)

#tidy
rm(biodepths)