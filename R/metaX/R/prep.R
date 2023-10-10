

##' @title Pre-Processing
##' @description Pre-Processing
##' @param para An metaX object
##' @param log A logical indicates whether do the log transformation
##' @param scale The method of scaling
##' @param center Centering
##' @param valueID The name of column used for transformation
##' @param ... Additional parameter
##' @return An new metaX object
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @export
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' para <- preProcess(para,valueID = "value",scale="uv")
preProcess=function(para,log=FALSE,scale=c("none", "pareto", "vector", "uv"),
                    center=TRUE,valueID="valueNorm",...){
    #sampleList  <- read.delim(para@sampleListFile)
    
    peaksData <- para@peaksData
    #peaksData <- peaksData[!is.na(peaksData$class),]
    #peaksData$class <- as.character(peaksData$class)
    xyData <- dcast(peaksData,sample+class~ID,value.var = valueID)
    
    # Numerical matrix (or an object coercible to such) 
    # with samples in rows and variables as columns.
    scaleData <- prep(xyData[,-c(1:2)],scale = scale,center=center,...)
    scaleData <- cbind(xyData[,1:2],scaleData)
    res <- melt(data = scaleData,value.name = valueID,
                id.vars = names(xyData)[1:2],variable.name  = "ID")
    peaksData[,valueID] <- NULL
    mres <- merge(res,peaksData,by=c("sample","class","ID"))
    
    if(nrow(mres)!=nrow(peaksData)){
        stop("scale error!\n")
    }
    para@peaksData <- mres
    return(para)
}

#Generalized log transformation function
logTransform=function(x){
    min.val <- min(abs(x[x!=0 & !is.na(x)]))/10;
    log2((x + sqrt(x^2 + min.val^2))/2)
}


glog=function (x, a = 1, inverse = FALSE) {
    if (inverse) {
        out <- 0.25 * exp(-x) * (4 * exp(2 * x) - (a * a))
    }else{
        out <- log((x + sqrt(x^2 + a^2))/2)
    }
    return(out)
}


##' @title Data transformation
##' @description Data transformation
##' @param para An metaX object
##' @param method The method for transformation, 1=log, 2=Cube root
##' @param valueID The name of column used for transformation
##' @param ... Additional parameter
##' @return An new metaX object
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @export
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' para <- transformation(para,valueID = "value")
transformation=function(para,method=1,valueID="valueNorm",...){
    if(method==1){
        ## Log transformation (generalized logarithm transformation or glog)
        para@peaksData[,valueID] <- logTransform(para@peaksData[,valueID])
    }else if(method==2){
        x <- para@peaksData[,valueID]
        min.val <- min(abs(x[x!=0 & !is.na(x)]))/10;
        x[is.na(x) | x <=0] <- min.val
        norm.data <- abs(x)^(1/3)
        para@peaksData[,valueID] <- norm.data
    }else{
        message("Please provide valid method!")
    }
    return(para)
}




