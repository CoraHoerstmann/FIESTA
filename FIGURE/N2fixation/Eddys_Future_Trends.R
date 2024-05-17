
#READ SUPPLEMENTARY TABLES:

CYCLONIC <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Model_output/Stats_cyclonic_eddies_atlas.csv")
ANTICYCLONIC <- read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Model_output/Stats_AC_atlas.csv")

#CYCLONIC_conservative <- CYCLONIC%>%filter(End.Time..month. > 5)
CYCLONIC_conservative <- CYCLONIC%>%filter(Start.Time..month. < 10)
CYCLONIC_conservative <- CYCLONIC_conservative%>%filter(Start.Time..month. > 5)

CYCLONIC_CONS <- CYCLONIC_conservative%>%filter(between(Start.Time..year., 2002, 2021))

CYCLONIC_CONS$area <- pi*CYCLONIC_CONS$Effective.Radius..km.^2*1000
CYCLONIC_CONS$volume <- CYCLONIC_CONS$area*100

  CYCLONIC_CONS_average_year <- CYCLONIC_CONS%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.), area_mean = mean(area),
            volume_mean = mean(volume),n = n())

CYCLONIC_CONS_average_year_2002_2011 <- CYCLONIC_CONS_average_year%>%filter(between(Start.Time..year.,2002, 2011))
sum(CYCLONIC_CONS_average_year_2002_2011$n)

CYCLONIC_CONS_average_year_2012_2021 <- CYCLONIC_CONS_average_year%>%filter(between(Start.Time..year.,2012, 2021))
sum(CYCLONIC_CONS_average_year_2012_2021$n)

lm(CYCLONIC_CONS_average_year$n ~ CYCLONIC_CONS_average_year$Start.Time..year.)

#FULL DATASET

CYCLONIC_conservative$area <- pi*CYCLONIC_conservative$Effective.Radius..km.^2*1000
CYCLONIC_conservative$volume <- CYCLONIC_conservative$area*100

CYCLONIC_conservative_average_year <- CYCLONIC_conservative%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.), area_mean = mean(area), area_sum = sum(area),
            volume_mean = mean(volume),n = n())


lm(CYCLONIC_conservative_average_year$n ~ CYCLONIC_conservative_average_year$Start.Time..year.)
lm(CYCLONIC_conservative_average_year$area_mean ~ CYCLONIC_conservative_average_year$Start.Time..year.)


#####


ACYCLONIC_conservative <- ANTICYCLONIC%>%filter(Start.Time..month. > 5)
ACYCLONIC_conservative <- ACYCLONIC_conservative%>%filter(Start.Time..month. < 10)

ACYCLONIC_CONS <- ACYCLONIC_conservative%>%filter(between(Start.Time..year., 2002, 2021))

ACYCLONIC_CONS$area <- pi*ACYCLONIC_CONS$Effective.Radius..km.^2*1000
ACYCLONIC_CONS$volume <- ACYCLONIC_CONS$area*100

ACYCLONIC_CONS_average_year <- ACYCLONIC_CONS%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.), area_mean = mean(area),
            volume_mean = mean(volume),n = n())

ACYCLONIC_CONS_average_year_2002_2011 <- ACYCLONIC_CONS_average_year%>%filter(between(Start.Time..year.,2002, 2011))
sum(ACYCLONIC_CONS_average_year_2002_2011$n)

ACYCLONIC_CONS_average_year_2012_2021 <- ACYCLONIC_CONS_average_year%>%filter(between(Start.Time..year.,2012, 2021))
sum(ACYCLONIC_CONS_average_year_2012_2021$n)

lm(ACYCLONIC_CONS_average_year$n ~ ACYCLONIC_CONS_average_year$Start.Time..year.)
lm(ACYCLONIC_CONS_average_year$area_mean ~ ACYCLONIC_CONS_average_year$Start.Time..year.)

#FULL DATASET

ACYCLONIC_conservative$area <- pi*ACYCLONIC_conservative$Effective.Radius..km.^2*1000
ACYCLONIC_conservative$volume <- ACYCLONIC_conservative$area*100

ACYCLONIC_conservative_average_year <- ACYCLONIC_conservative%>%group_by(Start.Time..year.)%>%
  summarise(Lifetime_mean = mean(Lifetime..days.), Radius_mean = mean(Effective.Radius..km.),
            rotational_speed_mean = mean(Rotational.Speed..cm.s.), Amplitude_mean = mean(Amplitude..cm.),
            Propagation_speed_mean = mean(Propagation.Speed..cm.s.), area_mean = mean(area), area_sum = sum(area),
            volume_mean = mean(volume),n = n())

AC_stats <- lm(ACYCLONIC_conservative_average_year$n ~ ACYCLONIC_conservative_average_year$Start.Time..year.)
print(AC_stats)

lm(ACYCLONIC_conservative_average_year$area_sum ~ ACYCLONIC_conservative_average_year$Start.Time..year.)

