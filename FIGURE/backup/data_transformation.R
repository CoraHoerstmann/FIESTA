#clr transformation

clr <- function(abundance.tax, count){
  
  suppressPackageStartupMessages(require(vegan))
  packageVersion("vegan")
  suppressPackageStartupMessages(require(zCompositions))
  packageVersion("zCompositions")
  
  d <- as.data.frame(abundance.tax)
  sum(d == 0) #filter out low abundance ASVs
  #dd <- subset(d, rowSums(d, na.rm = TRUE) == 0)
  d.1 <- data.frame(d[which(apply(d, 1, function(x){mean(x)}) > count),], 
                    check.names=F) 
  d.czm <- cmultRepl(t(d.1),  label=0, method="CZM", frac= 0.9, adjust=FALSE, z.warning = 0.9) #CZM is a Bayesian toll for count zeros in compositional datasets. Needs to be done when you want to do a log ratio transformation afterwards cause t cannot deal with 0s.
  #NOTE: fraction changed because 0 observations were above the smallest detected fraction. (default usually 0.65)
  d.clr <- t(apply(d.czm, 1, function(x){log(x) - mean(log(x))})) #centered log ratio transformation
  mi <- t(d.clr)
  d.clrdata <- data.matrix(mi)
  d.clrdata <- as.matrix(d.clrdata)
  abundance.clr <- as.data.frame(d.clrdata)
  
  return(abundance.clr)
  
  detach("package:zCompositions", unload=TRUE)
  detach("package:vegan", unload=TRUE)
  
}

dczm <- function(abundance.tax, count){
  
  suppressPackageStartupMessages(require(vegan))
  packageVersion("vegan")
  suppressPackageStartupMessages(require(zCompositions))
  packageVersion("zCompositions")
  require(coda.base)
  packageVersion("coda.base")
  require(usedist)
  packageVersion("usedist")
  
  d <- as.data.frame(abundance.tax)
  sum(d == 0) #filter out low abundance ASVs
  
  d.1 <- data.frame(d[which(apply(d, 1, function(x){mean(x)}) > count),], 
                    check.names=F) 
  d.czm <- cmultRepl(t(d.1),  label=0, method="CZM")
  #CZM is a Bayesian toll for count zeros in compositional datasets. Needs to be done when you want to do a log ratio transformation afterwards cause t cannot deal with 0s.
  a <- as.matrix(d.czm)
  names <- rownames(a)
  aitchinson <- coda.base::dist(a, method = 'ait')
  ait <-usedist::dist_setNames(aitchinson, names)
  return(ait)
  
  #if you want you can also turn it into a normal matrix:
  #as.matrix(ASV_Aitchinson_dist_16S)
  
  detach("package:coda.base", unload=TRUE)
  
  
}
