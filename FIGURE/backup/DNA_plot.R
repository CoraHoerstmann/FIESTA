##Plot

p <- qPCR_all%>%
  ggplot()+
  geom_point(aes(x = Longitude, y = log(g.c._L_seawater), color = Desired_Depth..m.))+
  scale_color_viridis() + 
  
  theme_bw() +
  labs(x = "Station", y = "qPCR Tricho")

print(p)

### The following is for extrapolation but not used with this dataset
#qPCR_surf <- qPCR_all%>%filter(Desired_Depth..m. == 5)

#pop <- N2_surf[,3:5]

#pop$x = pop$Longitude
#pop$y = pop$Latitude
#pop$prop = pop$g.c._L_seawater

#pop <- pop[,4:6]
#pop <- pop %>% drop_na()
#pop <- pop%>%filter(prop >0)


#pop$log_count <- log(pop$prop)
#cfun <- function(x, bias=2) {
#  x <- (x-min(x))/(max(x)-min(x))
#  xcol <- colorRamp(c("lightyellow", "orange", "red"), bias=bias)(x)
#  rgb(xcol, maxColorValue=255)
#}

#ggplot(pop, aes(x=x, y=y, size = prop)) +
#  geom_point(alpha=0.5)+
#  scale_size(range = c(.1, 20), name="cell count Trichodesmium")
