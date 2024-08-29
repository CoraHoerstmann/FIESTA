require(ggpmisc)

#READ SUPPLEMENTARY TABLES:

Nos <-read.csv("/Users/choerstm/Documents/MIO_FIGURE/Oceanography/Model_output/Future_Trends_No_eddies.csv")

a <- ggplot(Nos, aes(x=year, y=AC_eddies))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_poly_eq(use_label(c("eq", "R2")))
print(a)

b <- ggplot(Nos, aes(x=year, y=C_eddies))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_poly_eq(use_label(c("eq", "R2")))
print(b)



lm(Nos$AC_eddies ~ Nos$year)

Nos$Pred <- -313+0.163*Nos$year
Nos$C_Pred <- 71-0.03*Nos$year
#calculate the Standard Deviation (source: https://www2.physnet.uni-hamburg.de/TUHH/Versuchsanleitung/Fehlerrechnung.pdf p.14)

o <- sqrt(1/27*sum((Nos$AC_eddies-Nos$Pred)^2))
sd <- o*sqrt(29/(29*sum(Nos$year^2)-(sum(Nos$year))^2))
print(sd)
o_C <- sqrt(1/27*sum((Nos$C_eddies-Nos$C_Pred)^2))
sd_C <- o_C*sqrt(29/(29*sum(Nos$year^2)-(sum(Nos$year))^2))
print(sd_C)
#Calculate the difference in N2 input from AC eddies between 2021 and 2011

#difference in number of eddies
print((0.163*2021-313)/(0.163*2011-313))
Nos_1Decade <- Nos%>%filter(year > 2011)
Nos_1Decade <- Nos_1Decade%>%filter(year < 2017)
Nos_2Decade <- Nos%>%filter(year > 2016)

mean(Nos_1Decade$Pred)
mean(Nos_2Decade$Pred)


#take the 2021 vs. 2011 (1.11) and then the averages (1.10)
print(1.11*9.3*0.1*100)
print(1.10*9.3*0.1*100)

#clean up
rm(a,b)