
##' @title Classical univariate ROC analysis
##' @description Classical univariate ROC analysis
##' @rdname calcAUROC
##' @docType methods
##' @param x A numeric vector
##' @param y A response vector
##' @param cgroup Sample class used
##' @param plot A logical indicates whether plot
##' @param ... Additional parameter
##' @return A data.frame
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' \donttest{
##' x <- rnorm(50,2,2)
##' y<- rep(c("c","t"),25)
##' calcAUROC(x,y)
##' }
setGeneric("calcAUROC",function(x,y,cgroup,plot,...) 
    standardGeneric("calcAUROC"))

## classical univariate ROC analysis
##' @describeIn calcAUROC
setMethod("calcAUROC", signature(x="numeric"), function(x,y,plot=FALSE,...){
    roc.obj <- pROC::roc(y,x,percent = FALSE)
    auc.ci <- ci.auc(roc.obj, method = "bootstrap",boot.n = 500, 
                     progress = "none")
    rocdata <- data.frame(roc=roc.obj$auc,
                          lowROC=auc.ci[1],
                          upROC=auc.ci[3])
    if(plot==TRUE){
        plot.roc(roc.obj,print.auc=FALSE, legacy.axes=TRUE, 
                 col="navy", grid=TRUE,
                 xlab = "False positive rate", 
                 ylab="True positive rate")
        
    }
    return(rocdata)
    
}
)


##' @title Classical univariate ROC analysis
##' @description Classical univariate ROC analysis
##' @param para A metaXpara object
##' @param cgroup Samples used
##' @param cpu The number of CPU used
##' @param plot A logical indicates whether plot
##' @param ... Additional parameter
##' @return A metaXpara object
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' \dontrun{
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' addValueNorm(para) <- para
##' res <- myCalcAUROC(para,cgroup=c("S","C"))
##' }
myCalcAUROC=function(para,cgroup,cpu=0,plot=FALSE,...){
    peaksData <- dplyr::filter(para@peaksData,class %in% cgroup)
    peaksData$class <- as.character(peaksData$class)
    myROC=function(id,pData){
        dat <- dplyr::filter(pData,ID==id)
        x <- dat$valueNorm
        y <- dat$class
        roc.obj <- pROC::roc(y,x,percent = FALSE)
        auc.ci <- pROC::ci.auc(roc.obj, method = "bootstrap",boot.n = 500, 
                         progress = "none")
        rocdata <- data.frame(ID=id,roc=roc.obj$auc,lowROC=auc.ci[1],
                              upROC=auc.ci[3])
        return(rocdata)
    }
    cpu = ifelse(cpu==0,detectCores(),cpu)
    cl <- makeCluster(getOption("cl.cores", cpu))
    clusterExport(cl, c("myROC","roc","ci.auc"),envir=environment())
    clusterEvalQ(cl,library("pROC"))
    rocRes <- parLapply(cl,unique(peaksData$ID),myROC,pData=peaksData)
    stopCluster(cl)
    rocDF <- as.data.frame(rbindlist(rocRes))
}

