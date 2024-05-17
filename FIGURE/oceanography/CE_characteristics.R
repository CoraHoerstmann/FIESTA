cyclonic_eddy <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/Oceanography/CE_FIGURE.csv")

sampling_time_CE <- cyclonic_eddy[157:166,]

mean(sampling_time_CE$Effective.Radius..km.)
sd(sampling_time_CE$Effective.Radius..km.)
mean(sampling_time_CE$Rotational.Speed..cm.s.)
sd(sampling_time_CE$Rotational.Speed..cm.s.)


##Plot on a map the path of the eddy with dates

CE_track <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = cyclonic_eddy, aes(x = Longitude, y = Latitude), size = 1, 
             shape = 1)+
  geom_text(data = cyclonic_eddy, aes(x = Longitude, y = Latitude, label=Time..month.),
            nudge_x=0.45, nudge_y=0.1,
            check_overlap=T) +
  coord_sf(xlim = c(-85, -60), ylim = c(30, 45), expand = FALSE)

print(CE_track)
