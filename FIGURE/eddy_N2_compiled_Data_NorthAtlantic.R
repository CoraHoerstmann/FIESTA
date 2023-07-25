

#N2_structure_DB_old <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/N2_data/Satellite_N2_DB/N2_DB_compiled_Fine_scale_structures.csv")
#N2_structure_DB_old$N2.Rate.nmol.L.1.d.1 <- as.numeric(N2_structure_DB_old$N2.Rate.nmol.L.1.d.1)


N2_eddies_NorthAtlantic <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/N2_data/Satellite_N2_DB/DiazotrophsDatabase-20230616_Shao_markedStations.csv")
N2_eddies_NorthAtlantic$Whole.Seawater.N2.Fixation..nmol.N.L.1.d.1. <- as.numeric(N2_eddies_NorthAtlantic$Whole.Seawater.N2.Fixation..nmol.N.L.1.d.1.)

##Average biological replicates

N2_eddies_NorthAtlantic_mean <- N2_eddies_NorthAtlantic%>%group_by(LATITUDE, LONGITUDE, DATE..yyyy.mm.dd., DEPTH..m.)%>%
  summarize(N2 = mean(Whole.Seawater.N2.Fixation..nmol.N.L.1.d.1.), SatelliteBin = mean(Satellite.Bin),
            Temp = mean(Temperature...C.), Salinity = mean(Salinity..PSU.))

write.csv(N2_eddies_NorthAtlantic_mean, "N2_eddies_NorthAtlantic_mean.csv")

N2_structure_DB <- N2_structure_DB%>%filter(!Fine.scale.Structure..Open.Oean..coast.shelf == "unknown")

DB <- ggplot(data = N2_structure_DB, aes(x = Fine.scale.Structure..Open.Oean..coast.shelf, y = N2.Rate.nmol.L.1.d.1))+
  geom_violin(trim=FALSE)+
  ylim(0,100)+
  stat_summary(fun.y=median, geom="point", size=2, color="red")

print(DB)