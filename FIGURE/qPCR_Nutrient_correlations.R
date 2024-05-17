#CORRELATION ANALYSIS QPCR AND NUTRIENTS

library(corrplot)
library("Hmisc")
qPCR_NUTS <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/qPCR_nutrients_joined.csv")
N2_NUTS <- read.csv("/Users/corahoerstmann/Documents/MIO_FIGURE/Nutrients/CARING_btl_nutr_data_corrected_Neg_values_N2.txt", sep = "\t")

all_red <- all_data%>%
qPCR_NUTS <- left_join(qPCR_NUTS, all_data, by = "No")
qPCR_NUTS <- qPCR_NUTS[,c(10:14,16,18)]

N2_NUTS$stnum <- as.character(N2_NUTS$stnum)
ggplot(N2_NUTS, aes(x=nit, y=N2, shape = stnum, color = pres))+
  geom_point()

N2_NUTS <- N2_NUTS[,c(6,8,10,14,16,18,20)]

res2 <- cor(as.matrix(qPCR_NUTS),method = "pearson")
res3 <- cor(as.matrix(N2_NUTS), method = "pearson")

summary(res2)
corrplot(res2)

summary(res3)
corrplot(res3)

res <- rcorr(as.matrix(qPCR_NUTS), type="pearson")
res$P
res$r

res <- rcorr(as.matrix(N2_NUTS), type="pearson")
res$P
res$r
