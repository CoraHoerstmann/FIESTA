
#calculated only for the cyclonic eddy and only using our data

C_eddy_core <- N2_data_all_CTD%>%filter(Station == 2)
C_eddy_periphery <- N2_data_all_CTD%>%filter(Station == c(1,3))

##########calculate the volume in m3 #V=πr2h

stats_CE$vol_total <- (pi*stats_CE$Effective.Radius..km.^2*1000)*100
stats_CE$vol_core <- (pi*(stats_CE$Effective.Radius..km.*0.36)^2*1000)*100#based on ADCP
stats_CE$vol_core_ow <- (pi*(stats_CE$Effective.Radius..km.*0.41)^2*1000)*100
stats_CE$vol_periphery <- stats_CE$vol_total - stats_CE$vol_core
stats_CE$vol_periphery_ow <- stats_CE$vol_total - stats_CE$vol_core_ow

#continue with OW
#calculate area and area percent
stats_CE$area_total <- (pi*stats_CE$Effective.Radius..km.^2*1000)
stats_CE$area_core <- (pi*(stats_CE$Effective.Radius..km.*0.41)^2*1000)
stats_CE$area_periphery <- stats_CE$area_total - stats_CE$area_core
stats_CE$area_periphery/stats_CE$area_total*100

#conservative: remove those eddies that are not alive during the summer
stats_CE_conservative <- stats_CE%>%filter(End.Time..month. > 5)
stats_CE_conservative <- stats_CE_conservative%>%filter(Start.Time..month. < 10)
#calculate averages per year

stats_CE_conservative_average_year <- stats_CE_conservative%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.),
            vol_core_mean = mean(vol_core), vol_periphery_mean = mean(vol_periphery),vol_total_mean = mean(vol_total),n = n())

#Big calculation
#N input is nmol N per year total across all eddies - after that it needs to be normalized to the gridbox

stats_CE_conservative_average_year$N_input_C_core <- (mean(C_eddy_core$N2)/1000)*stats_CE_conservative_average_year$Lifetime_mean*stats_CE_conservative_average_year$vol_core_mean*stats_CE_conservative_average_year$n
stats_CE_conservative_average_year$N_input_C_periphery <- (mean(C_eddy_periphery$N2)/1000)*stats_CE_conservative_average_year$Lifetime_mean*stats_CE_conservative_average_year$vol_periphery_mean*stats_CE_conservative_average_year$n
#how much more is coming from the periphery then?

stats_CE_conservative_average_year$N_input_C_total <- stats_CE_conservative_average_year$N_input_C_core + stats_CE_conservative_average_year$N_input_C_periphery
stats_CE_conservative_average_year$N_input_C_periphery/stats_CE_conservative_average_year$N_input_C_total
stats_CE_conservative_average_year$N_input_C_core/stats_CE_conservative_average_year$N_input_C_total

#how much would be the underestimation? 
#First, extrapolate from the core rate and then compare it to the actual rates including edges
stats_CE_conservative_average_year$N_input_C_core_extrapolated <- (mean(C_eddy_core$N2)/1000)*stats_CE_conservative_average_year$Lifetime_mean*stats_CE_conservative_average_year$vol_total_mean*stats_CE_conservative_average_year$n

stats_CE_conservative_average_year$N_input_C_total/stats_CE_conservative_average_year$N_input_C_core_extrapolated

#overestimation
stats_CE_conservative_average_year$N_input_C_edge_extrapolated <- (mean(C_eddy_periphery$N2)/1000)*stats_CE_conservative_average_year$Lifetime_mean*stats_CE_conservative_average_year$vol_total_mean*stats_CE_conservative_average_year$n
stats_CE_conservative_average_year$N_input_C_edge_extrapolated/stats_CE_conservative_average_year$N_input_C_total

#how much more is the vol i the periphery?

stats_CE_conservative_average_year$vol_core_mean/(stats_CE_conservative_average_year$vol_core_mean + stats_CE_conservative_average_year$vol_periphery_mean)

stats_CE_conservative_average_year$vol_periphery_mean/(stats_CE_conservative_average_year$vol_core_mean + stats_CE_conservative_average_year$vol_periphery_mean)

##this means that through the relative higher productivity in the periphery, the N2 input could be ~ 5times higher than estimated based on samples from the eddy core

#plot?

stats_CE_conservative_average_year_long <- stats_CE_conservative_average_year%>%gather(key = "Structure_C", value = "N_input", N_input_C_periphery,N_input_C_core)

#convert to Gmol

stats_CE_conservative_average_year_long$N_input_mol <- stats_CE_conservative_average_year_long$N_input/1000000000
stats_CE_conservative_average_year_long$N_input_Gmol <- stats_CE_conservative_average_year_long$N_input_mol/1000000000

DB <- ggplot(data = stats_CE_conservative_average_year_long, aes(x = Structure_C, y = N_input_mol))+
  geom_violin(aes(fill = Structure_C),trim=FALSE)+
  scale_fill_manual(values = c("#E7B800", "#386610"))+
  #geom_boxplot(width = 0.08)+
  stat_summary(fun.y=median, geom="point", size=2, color="red")

print(DB)

