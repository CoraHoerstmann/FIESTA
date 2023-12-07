##Plot more according to biological depth (SFC- aDCM - DCM - bDCM)
#all_data_long <- gather(all_data, Species, log_cell, log_Tricho:log_GammaA, factor_key=TRUE)
qPCR <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/DNA/FIGURE_qPCR_fromR.csv")

qPCR_CTD <- qPCR%>%filter(!str_detect(Station, "UW"))
p <- qPCR_CTD%>%
  ggplot()+
  geom_point(aes(y = log_Tricho, x = -(Desired_Depth..m.), color = Station))+
  geom_smooth(aes(y = log_Tricho, x = -(Desired_Depth..m.), group=Station, color = Station), method = "loess", level=0.50)+
  scale_color_manual(values = c("#0f3cf7","#f45a45","#ee9a00","#f9d606","#58d805", "#3f560d","#4af496"))+
  coord_flip()+
  theme_bw() +
  labs(x = "Depth", y = "log() cell number")

print(p)

all_data_SFC <- all_data%>%filter(Desired_Depth..m. < 11)

##bring into long format

all_data_SFC_long <- gather(all_data_SFC, Species, log_cell, log_Tricho:log_GammaA, factor_key=TRUE)

a <- ggplot(all_data_SFC_long, aes(x = Longitude, y = log_cell, color = Species))+
  geom_point(size = 6)+
  scale_color_manual(values =c("#148419", "#EDB106", "#A30D79"))+
  ylim(0,16)+
  xlim(-73.7,-68)+
  geom_smooth(method = "loess")

print(a)

#safe as 16x6

#ax <- ggplot(all_data_SFC_long, aes(x = C_N, y = log_cell, color = Species))+
#  geom_point(size = 2)+
#  scale_color_manual(values =c("#148419", "#EDB106", "#A30D79"))+
#  ylim(0,16)+
  #xlim(-73.7,-68)+
#  geom_smooth(method = "loess")

#print(ax)

az <- ggplot(all_data_SFC_long, aes(x = N2, y = log_cell, color = Species))+
  geom_point(size = 2)+
  scale_color_manual(values =c("#148419", "#EDB106", "#A30D79"))+
  ylim(0,16)+
  #xlim(-73.7,-68)+
  geom_smooth(method = "loess")

print(az)

ay <- ggplot(all_data_SFC_long, aes(x = Temperature, y = log_cell, color = Species))+
  geom_point(size = 2)+
  scale_color_manual(values =c("#148419", "#EDB106", "#A30D79"))+
  ylim(0,16)+
  #xlim(-73.7,-68)+
  geom_smooth(method = "loess")

print(ay)

all_data_aDCM <- all_data%>%filter(between(Desired_Depth..m., 11, 45))

##bring into long format

all_data_aDCM_long <- gather(all_data_aDCM, Species, log_cell, log_Tricho:log_GammaA, factor_key=TRUE)

b <- ggplot(all_data_aDCM_long, aes(x = Longitude, y = log_cell, color = Species))+
  geom_point(size = 6)+
  scale_color_manual(values =c("#148419", "#EDB106", "#A30D79"))+
  ylim(0,16)+
  xlim(-73.7,-68)+
  geom_smooth(method = "loess")

print(b)


######
all_data_DCM <- all_data%>%filter(between(Desired_Depth..m., 46, 85))

##bring into long format

all_data_DCM_long <- gather(all_data_DCM, Species, log_cell, log_Tricho:log_GammaA, factor_key=TRUE)

c <- ggplot(all_data_DCM_long, aes(x = Longitude, y = log_cell, color = Species))+
  geom_point(size = 6)+
  scale_color_manual(values =c("#148419", "#EDB106", "#A30D79"))+
  ylim(0,16)+
  xlim(-73.7,-68)+
  geom_smooth(method = "loess")

print(c)


######
all_data_bDCM <- all_data%>%filter(Desired_Depth..m. > 86)

##bring into long format

all_data_bDCM_long <- gather(all_data_bDCM, Species, log_cell, log_Tricho:log_GammaA, factor_key=TRUE)

d <- ggplot(all_data_bDCM_long, aes(x = Longitude, y = log_cell, color = Species))+
  geom_point(size = 6)+
  scale_color_manual(values =c("#148419", "#EDB106", "#A30D79"))+
  ylim(0,16)+
  xlim(-73.7,-68)+
  geom_smooth(method = "loess")

print(d)


#cleanup
rm(p,a,b,c,d,
   all_data_aDCM_long, all_data_bDCM_long, all_data_DCM_long, all_data_SFC_long)
