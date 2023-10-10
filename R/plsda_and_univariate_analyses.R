# Env
library(pls)
library(DiscriMiner) # plsDA
library(MetabolAnalyze)

source("metaX/R/model.R")
runInlinePLSDA = function(...,xx,y,ncomp,validation,k=7,
                          method = "oscorespls"){
  sid<-sample(length(y),length(y))
  ## cite "https://github.com/lpantano/isomiRs/blob/master/R/PLSDA.R"
  res <- myPLSDA(xx,y[sid],method=method,ncomp=ncomp,
                 validation=validation,k=k)
  res$cor <- cor(y[sid],y)
  return(unlist(res))    
}

# adapted from MetaX
PLSDA_ye <- function(x, y, prefix, ncomp=3, validation="CV", k=7, n=200, method = "oscorespls") {
  x=as.matrix(x)
  
  result <- list()
  
  set.seed(1)
  p <- myPLSDA(x,y,method=method,ncomp=ncomp,validation=validation,k=k,save=TRUE)
  p$cor <- 1
  result$model <- p$model
  
  p$model <- NULL
  
  result$plsda <- list()
  result$plsda$res <- p
  result$VIP <- calcVIP(result$model,ncomp = ncomp)
  result$x <- x
  result$y <- y
  
  result$ncomp <- ncomp
  result$kfold <- k
  result$validation <- validation
  result$method <- method
  
  ## permutation
  dat <- 1:n
  
  result$plsda$perm <- sapply(dat, FUN = function(i){runInlinePLSDA(xx=x,y=y,
                                                                    ncomp=ncomp,
                                                                    validation=validation,
                                                                    k=k,method = method)})
  
  ## pvalue
  result$pvalue <- list()
  result$pvalue$R2 <- sum(result$plsda$perm['R2',] > result$plsda$res$R2)/n
  result$pvalue$Q2 <- sum(result$plsda$perm['Q2',] > result$plsda$res$Q2)/n
  
  
  ## PCA plot
  p <- plsDA(variables = x,group = y,autosel = FALSE,comps=ncomp)
  print(p$confusion)
  print(p$R2)
  print(p$Q2)
  print(p$specs)
  message("error rate: ",p$error_rate)
  
  #r2 <- R2(p,estimate="train")
  
  if(ncomp >=3){
    
    plotData <- data.frame(x=p$components[,1],
                           y=p$components[,2],
                           z=p$components[,3],
                           sample=rownames(x),
                           class=y)
  }else{
    plotData <- data.frame(x=p$components[,1],
                           y=p$components[,2],
                           sample=rownames(x),
                           class=y)
  }
  plotData$class <- as.character(plotData$class)
  plotData$class[is.na(plotData$class)] <- "QC" 
  # sampleList$class <- NULL
  # plotData <- merge(plotData,sampleList,by="sample",sort=FALSE)
  
  ggobj <-ggplot(data = plotData,aes(x=x,y=y,colour=class))+
    geom_hline(yintercept=0,colour="white",size=1)+
    geom_vline(xintercept=0,colour="white",size=1)+
    geom_point()+
    xlab(paste("PC1"," (",sprintf("%.2f%%",100*p$R2[1,1]),") ",sep=""))+
    ylab(paste("PC2"," (",sprintf("%.2f%%",100*p$R2[2,1]),") ",sep=""))+
    theme(legend.justification=c(1,1), 
          legend.position=c(1,1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background=element_rect(fill="#E3E3EE"))+
    stat_ellipse(geom = "polygon", type="euclid",alpha = 0.4, 
                 aes(fill = class))+
    stat_ellipse(geom = "path") +
    theme_classic()
  ggobj
  ggsave(paste0(prefix, "-PLSDA_PCA.pdf"), height = 4, width = 4.5)
  
  
  ## validation plot
  
  plotdat <- as.matrix(cbind(unlist(result$plsda$res),result$plsda$perm))
  plotdat <- as.data.frame(t(plotdat))
  x1 <- plotdat$cor[1]
  y1 <- plotdat$R2[1]
  y2 <- plotdat$Q2[1]
  plotdat$cor <- abs(plotdat$cor)    
  plotdat <- plotdat[order(plotdat$cor),]
  par(mar=c(3,3,2,1),mgp=c(1.6,0.6,0),cex.lab=1.2,cex.main=0.9)
  
  pdf(paste0(prefix, "-validation_plot.pdf"))
  plot(plotdat$cor,plotdat$R2,ylim=c(min(plotdat$R2,plotdat$Q2),1),pch=16,
       xlab="Cor",ylab="Value",col="blue")
  points(plotdat$cor,plotdat$Q2,pch=15,col="red")
  
  lm.r <- lm(I(R2-y1)~I(cor-x1)+0,data=plotdat)
  lm.q <- lm(I(Q2-y2)~I(cor-x1)+0,data=plotdat)
  #lines(plotdat$cor,predict(lm.r,data=plotdat$cor),col="blue",lty=2)
  #lines(plotdat$cor,predict(lm.q,data=plotdat$cor),col="red",lty=2)
  int.R <- predict(lm.r,newdata=list(cor=0))+y1
  int.Q <- predict(lm.q,newdata=list(cor=0))+y2
  
  abline(int.R,coef(lm.r),lty=2,col="blue")
  abline(int.Q,coef(lm.q),lty=2,col="red")
  #abline(lm.q,lty=2,col="red")
  legend("bottomright",pch=c(16,15),legend = c("R2","Q2"),col=c("blue","red"))
  
  title(main = paste("Intercepts:","R2=(0.0,",sprintf("%.4f",int.R),
                     "), Q2=(0.0,",sprintf("%.4f",int.Q),")"))
  dev.off()
  
  
  return(result)
}

# Analysis
{ 
  ## Data transformation
  data2 <- scaling(log2(data), type="pareto") # metabolomic data
  y <- your_grouping_info

  ## PLSDA
  plsda <- PLSDA_ye(x, y, "./prefix_for_your_plots/e.g._A_H-") 
  write.table(data.frame(VIP=plsda$VIP), "path/to/your/output.txt", sep="\t", col.names = NA, quote = F)


  ## Univariate
  res_t <- res_wilcoxon <- fc <- data.frame(matrix(rep(NA, ncol(data)*1), nrow = ncol(data)))
  colnames(res_t) <- colnames(res_wilcoxon) <- colnames(fc) <- "Control_Epilepsy"
  rownames(res_t) <- rownames(res_wilcoxon) <- rownames(fc) <- colnames(data)

  for (i in 1:ncol(data2)) {
    res <- t.test(data2[, i]~y)
    res_t[i, ] <- res$p.value
    
    res <- wilcox.test(data2[, i]~y)
    res_wilcoxon[i, ] <- res$p.value
    
    fc[i, ] <- mean(data[indice_group1, i])/mean(data[indice_group2, i])
  }
  res_t.q <- p.adjust(res_t[,1], method="BH"); names(res_t.q) <- "t.BH.q"
  res_wilcoxon.q <- p.adjust(res_wilcoxon[,1], method="BH"); names(res_wilcoxon) <- "wilcox.BH.q"

  write.table(cbind(fc, res_t, res_t.q, res_wilcoxon, res_wilcoxon.q), "prefix/results_of_univariate_analyses.txt", sep="\t", col.names = NA, quote=F)
}
