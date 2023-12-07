

RDA_plots <- function(meta, ASV.clr, ASV.ait, meta_sig){
  
  #load additional packages
  suppressPackageStartupMessages(require("ggords"))
  suppressPackageStartupMessages(require("vegan"))
  library(gridExtra)
  
  
  ##ASV data prep (sorting according to metadata)
  ASV.clr.t <- t(ASV.clr)
  ASV.clr.t.sort <- as.data.frame(ASV.clr.t)
  ASV.clr.t.sort <- cbind(ASV.clr.t.sort, meta$No)
  ASV.clr.t.sort <- with(ASV.clr.t.sort, ASV.clr.t.sort[order(meta$No),])
  ASV.clr.t.sort$`meta$No` <-NULL
  ASV.clr.t.sort <- as.matrix(data.matrix(ASV.clr.t.sort))
  meta.wf <- with(meta, meta[order(No),])
  lapply(meta.wf, class)%>%unlist()
  meta.wf$sea_state <- as.numeric(meta.wf$sea_state)
  meta.wf$swell <- as.numeric(meta.wf$swell)
  meta.wf$wind_waves <- as.numeric(meta.wf$wind_waves)
  meta.wf$Temperature <- as.numeric(meta.wf$Temperature)
  meta.wf$Actual_Depth..m. <- as.numeric(meta.wf$Actual_Depth..m.)
  
  meta.wf.data <- meta.wf[meta_sig]
  gr <- meta.wf$Pysical_Classification
  grl <- factor(gr)
  
  gr2 <- meta.wf$Actual_Depth..m.
  grl2 <- factor(gr2)

  #RDA
  ASV.clr.rda <- rda(
    ASV.clr.t.sort ~ .,
    data = meta.wf.data
  )
  
  
  summary(ASV.clr.rda)
  #return(ASV.clr.rda)
  anova.cca(ASV.clr.rda, step=1000, by="axis")
  plot(ASV.clr.rda, scaling=1, main="Triplot RDA (scaling 1)")
  
  
  ##
  #Advanced plots scaling 1
  #windows()
  plot(ASV.clr.rda, scaling=1, main="Triplot RDA - scaling 1", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1,1))
  points(scores(ASV.clr.rda, display="sites", choices=c(1,2), scaling=1),
         pch=21, col="black", bg="steelblue", cex=1.2)
  arrows(0,0,
         scores(ASV.clr.rda, display="species", choices=c(1), scaling=1),
         scores(ASV.clr.rda, display="species", choices=c(2), scaling=1),
         col="black",length=0)
  text(scores(ASV.clr.rda, display="species", choices=c(1), scaling=1),
       scores(ASV.clr.rda, display="species", choices=c(2), scaling=1),
       labels=rownames(scores(ASV.clr.rda, display="species", scaling=1)),
       col="black", cex=0.8)    
  arrows(0,0,
         scores(ASV.clr.rda, display="bp", choices=c(1), scaling=1),
         scores(ASV.clr.rda, display="bp", choices=c(2), scaling=1),
         col="red")
  text(scores(ASV.clr.rda, display="bp", choices=c(1), scaling=1)+0.05,
       scores(ASV.clr.rda, display="bp", choices=c(2), scaling=1)+0.05,
       labels=rownames(scores(ASV.clr.rda, display="bp", choices=c(2), scaling=1)),
       col="red", cex=1) 
  
  ###
  
  invisible(hist(residuals(ASV.clr.rda), main = ""))
  
  ASV.clr.rda.anova <- anova.cca(ASV.clr.rda)
  print(ASV.clr.rda.anova)
  
  inertia.rda.tot <- ASV.clr.rda$tot.chi
  inertia.rda.tot
  inertia.rda.constrained <- ASV.clr.rda$CCA$tot.chi
  inertia.rda.constrained
  inertia.rda.constrained.prop <- inertia.rda.constrained/inertia.rda.tot
  print(inertia.rda.constrained.prop)
  
  
  #par(xpd=TRUE)
  #provinces

  ##solution to the problem may be here:
  #https://david-barnett.github.io/microViz/articles/web-only/ordination.html
print(ggords::ggrda(ASV.clr.rda, group = grl, spearrow = NULL, farrow = 0.1, fzoom = 5, ellipse = T, scaling = 1, spe = F)+
      scale_color_manual(name = "Groups", values = c("blue","orange2", "#f45a45","#2e630b", "#16f27e", "#F9D606", "orange2"))+
    scale_shape_manual(name = "Groups",values = c(16,16,16,16,16,16,16,16,16,16)))
    
  
 # print(ggords::ggrda(ASV.clr.rda, group = grl2, spearrow = NULL, farrow = 0.1, fzoom = 5, ellipse = F, scaling = 1, spe = F)+
#          scale_color_manual(name = "Groups", values = c("#8e2566","darkgrey","grey","lightblue","blue","red", "#6DB72C","pink", "seagreen", "#F9D606", "orange2", "#8f09af"))+
#          scale_shape_manual(name = "Groups",values = c(10,10,10,10,10,10,10,10,10,10,10,10)))
          
  #print(ggrda(ASV.clr.rda,group = curl.G, spearrow = NULL, farrow = 0.1, fzoom = 5, ellipse = T, scaling = 2, spe = F)+
  #        scale_color_manual(name = "Groups",values = c("seagreen", "orange2")) +
  #        scale_shape_manual(name = "Groups",values = c(16,16)))
  
  
  #province PERMANOVA
  ASV.ait.t <- t(ASV.ait)
  
  
  
  
  #Test physical classifications
  
  print(adonis2(
    formula = ASV.ait.t ~ Pysical_Classification,
    data = meta,
    method = "jaccard"
  ))
  
  
  dis <- vegdist(ASV.ait.t, method = "jaccard")
  mod <- betadisper(dis, meta$Pysical_Classification)
  
  print(mod)
  
  
  #region
}
