##Other datasets distance

library("geosphere"); packageVersion("geosphere")

Dugenne_2017 <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/OTHER_DATASETS/Dugenne_2017.csv")
Dugenne_2018 <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/OTHER_DATASETS/Dugenne_2018.csv")
Fong2008 <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/OTHER_DATASETS/Fong2008.csv", row.names = 1)
Olson1 <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/OTHER_DATASETS/Olson1.csv")
Olson2 <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/OTHER_DATASETS/Olson2.csv")


Dugenne_2017 <- Dugenne_2017%>%filter(Depth..m. == 5)
Dugenne_2018 <- Dugenne_2018%>%filter(Depth..m. == 5)
Olson1 <- Olson1%>%filter(depth_n ==0)
Olson1$lon <- as.numeric(Olson1$lon)
Olson1$lat <- as.numeric(Olson1$lat)
Olson1 <- Olson1%>%group_by(station)%>%summarize(Longitude = mean(lon), Latitude = mean(lat))
Olson1$station <- NULL
Olson2 <- Olson2%>%filter(depth_n == 0)
Olson2$lon <- as.numeric(Olson2$lon)
Olson2$lat <- as.numeric(Olson2$lat)
Olson2 <- Olson2%>%group_by(station)%>%summarize(Longitude = mean(lon), Latitude = mean(lat))
Olson2$station <- NULL

#calculate distance between points

Dugenne_2017 <- Dugenne_2017[,c(4:5)]
dist_mat <- distm(Dugenne_2017, fun = distGeo) #result in m

dist_mat <- dist_mat[2:9,1]
mean(dist_mat)
sd(dist_mat)/1000
197465.2/1000

#calculate distance between points

Dugenne_2018 <- Dugenne_2018[,c(4:5)]
dist_mat <- distm(Dugenne_2018, fun = distGeo) #result in m

dist_mat <- dist_mat[2:11,1]
min(dist_mat)/1000
max(dist_mat)/1000
mean(dist_mat)
sd(dist_mat)
248770.3/1000
206665/1000

#calculate distance between points
dist_mat <- distm(Fong2008, fun = distGeo) #result in m

dist_mat <- dist_mat[2:10,1]
mean(dist_mat)/1000
sd(dist_mat)/1000

#calculate distance between points
dist_mat <- distm(Olson1, fun = distGeo) #result in m

dist_mat <- dist_mat[2:17,1]
mean(dist_mat)/1000
sd(dist_mat)/1000

#calculate distance between points
dist_mat <- distm(Olson2, fun = distGeo) #result in m

dist_mat <- dist_mat[2:16,1]
mean(dist_mat)/1000
sd(dist_mat)/1000
