
calcRatio=function(x=NULL,class=NULL,weights=NULL,
                   method=c("mean","median","weight"),
                   case=NA,control=NA, ...){
    
    r1 <- x[class==case]
    r2 <- x[class==control]
    
    r1 <- r1[!is.nan(r1)]
    r1 <- r1[!is.infinite(r1)]
    r2 <- r2[!is.nan(r2)]
    r2 <- r2[!is.infinite(r2)]
    
    if(method=="mean"){
        ratio <- mean(r1,na.rm = TRUE)/mean(r2,na.rm = TRUE)
    }else if(method=="median"){
        ratio <- median(r1,na.rm = TRUE)/median(r2,na.rm = TRUE)
    }else{
        stop("method must be in 'mean', 'median' and 'weight'!")
    }
    
    
    
}




##Hotellings T2 statistic and ellipse calculation function##
HotE = function(x, y, len = 200,alfa=0.95){
    N <- length(x)
    A <- 2
    mypi <- seq(0, 2 * pi, length = len)
    r1 <- sqrt(var(x) * qf(alfa,2, N-2) * (2*(N^2-1)/(N*(N - 2))))
    r2 <- sqrt(var(y) * qf(alfa,2, N-2) * (2*(N^2-1)/(N *(N-2))))
    cbind(r1 * cos(mypi) + mean(x), r2*sin(mypi)+mean(y))
}

##' @title permutePLSDA
##' @description Validation of the PLS-DA model by using permutation test 
##' statistics
##' @param x a matrix of observations.
##' @param y a vector or matrix of responses.
##' @param n number of permutations to compute the PLD-DA p-value based on R2 
##' magnitude. Default n=100
##' @param np the number of components to be used in the modelling.
##' @param outdir output dir
##' @param prefix the prefix of output figure file 
##' @param tol tolerance value based on maximum change of cumulative R-squared 
##' coefficient for each additional PLS component. Default tol=0.001
##' @param cpu 0
##' @param ... additional arguments
##' @return pvalue
##' @export
permutePLSDA=function(x,y,n=100,np=2,outdir = "./", prefix="metaX",tol=0.001,
                      cpu=0,...){
    
    p <- plsDA(variables = x,group = y,autosel = FALSE,comps = np)
    r2 <- NA
    if(p$R2[dim(p$R2)[1],3] <= tol){
        a.perm <- which(p$R2[,3] < tol)
        if(a.perm[1] > 1){ 
            r2 <- p$R2[a.perm[1]-1,4] 
        }else if(a.perm[1] == 1){
            r2 <- p$R2[1,4] 
        }
    }else{
        r2 <- p$R2[dim(p$R2)[1],4]
    }
    
    
    
    fig <- paste(outdir,"/",prefix,"-permutation_PLSDA.pdf",sep="")
    pdf(fig,width = 4,height = 4)
    
    
    dat <- 1:n
    
    if(cpu==0){
        cpu <- detectCores()
    }
    cl <- makeCluster(getOption("cl.cores", cpu))
    clusterExport(cl, c("run_PLSDA"),envir=environment())

    res<-parSapply(cl,dat,FUN = run_PLSDA,xx=x,y=y,np=np,tol=tol)
    stopCluster(cl)


    ## sometimes, maybe there are rows that the values are nan.    
    trueTotal <- sum(!is.na(res))
    message("True permutation number is ",trueTotal)
    pvalue <- sum(r2 <= res,na.rm = TRUE)/trueTotal
    message("p-value = ",pvalue)
    plotDat <- data.frame(R2=res)

    
    ggobj <- ggplot(data = plotDat, aes(x=R2))+
        geom_histogram(colour="white")+
        geom_vline(xintercept = r2,colour="red",size=1)+
        ylab("Count")+
        scale_y_continuous(expand = c(0,0))+
        theme(legend.justification=c(1,1), 
              legend.position=c(1,1),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background=element_rect(fill="#E3E3EE"))
    print(ggobj)
    dev.off()
    return(pvalue)
}

run_PLSDA = function(...,xx,y,np,tol=0.001){
    ## must put "..." before x,y, so that the parameter from ddply( or sapply)
    ## transfer to fun is omit.
    sid<-sample(length(y),length(y))
    p <- plsDA(variables = xx,group = y[sid],autosel = FALSE,comps = np)
    R2value <- NA
    if(p$R2[dim(p$R2)[1],3] <= tol){
        a.perm <- which(p$R2[,3] < tol)
        if(a.perm[1] > 1){ 
            R2value <- p$R2[a.perm[1]-1,4] 
        }else if(a.perm[1] == 1){
            R2value <- p$R2[1,4] 
        }
    }else{
        R2value <- p$R2[dim(p$R2)[1],4]
    }
    R2value
}


countMissingValue = function(x,ratio,omit.negative=TRUE){
    if(omit.negative){
        rp <- (sum(x<=0 | is.na(x))/length(x))>=ratio
    }else{
        rp <- ( sum(is.na(x)) / length(x) )>=ratio
    }
    return(rp)
}

##' @title metaXpipe
##' @description metaXpipe
##' @rdname metaXpipe
##' @docType methods
##' @param para A metaXpara object.
##' @param plsdaPara A plsDAPara object.
##' @param cvFilter Filter peaks which cv > cvFilter in QC samples.
##' @param remveOutlier Remove outlier samples.
##' @param outTol The threshold to remove outlier samples.
##' @param doQA Boolean, setting the argument to TRUE will perform plot 
##' quality figures. 
##' @param doROC A logical indicates whether to calculate the ROC
##' @param qcsc Boolean, setting the argument to TRUE to perform quality 
##' control-robust loess signal correction.
##' @param nor.method Normalization method.
##' @param pclean Boolean, setting the argument to TRUE to perform 
##' data cleaning 
##' @param t Data transformation method. See \code{\link{transformation}}.
##' @param scale Data scaling method.
##' @param idres A file containing the metabolite identification result
##' @param nor.order The order of normalization, only valid when \code{qcsc} is 
##' TRUE. 1: before QC-RLSC, 2: after QC-RLSC.
##' @param out.rmqc Boolean, setting the argument to TRUE to remove the QC 
##' samples for the csv file.
##' @param saveRds Boolean, setting the argument to TRUE to save some objects to
##' disk for debug. Only useful for developer. Default is FALSE.
##' @param ... Other argument
##' @return A metaXpara object.
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' \dontrun{
##' ## example 1: no QC sample
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' ratioPairs(para) <- "KO:WT"
##' outdir(para) <- "test"
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' plsdaPara <- new("plsDAPara")
##' p <- metaXpipe(para,plsdaPara=plsdaPara)
##' 
##' ## example 2: has QC samples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' ratioPairs(para) <- "S:C"
##' plsdaPara <- new("plsDAPara")
##' p <- metaXpipe(para,plsdaPara=plsdaPara)
##' }
setGeneric("metaXpipe",function(para,plsdaPara,cvFilter = 0.3,remveOutlier=TRUE,
                              outTol=1.2,doQA=TRUE,doROC=TRUE,qcsc=FALSE,nor.method="pqn",
                              pclean=TRUE,t=1,scale="uv",idres=NULL,
                              nor.order=1,out.rmqc=FALSE,saveRds=FALSE,...) 
    standardGeneric("metaXpipe"))
##' @describeIn metaXpipe
setMethod("metaXpipe", signature(para = "metaXpara"),
          function(para,plsdaPara,cvFilter = 0.3,remveOutlier=TRUE,outTol=1.2,
                   doQA=TRUE,doROC=TRUE,qcsc=FALSE,nor.method="pqn",
                   pclean=TRUE,t=1,scale="uv",idres=NULL,nor.order=1,
                   out.rmqc=FALSE,saveRds=FALSE,...){
    
    if(is.null(para@ratioPairs)){
        stop("Please set the value of ratioPairs!")
    }
    
    ## directory structure, data (all figures and other files), report.html
    raw_outdir <- para@outdir
    ## all figures and other files
    para@outdir <- paste(para@outdir,"/data",sep="") 
    makeDirectory(para)
    
    
    ## create report 
    report<-newCustomReport("metaX Report")    
    report<-setReportTitle(report,
                           "Metabolomic Data Analysis with metaX")
    
    ##The first section
    ## ref : http://www.mdpi.com/2218-1989/3/3/552/pdf
    s1 <-newSection("Introduction")
    s1 <-addTo(s1,newParagraph(
        "Metabolomics is a growing and powerful technology capable of detecting 
        hundreds to thousands of metabolites in tissues and biofluids. 
        metabolomics alterations represent changes in the phenotype and 
        molecular physiology."))
    
    s2<-newSection("Methods and Data")
    ##Introduce the experiment design and data analysis
    s2_sub1 <- newSubSection(asStrong("Summary of Data Set"))
    
    expDesign <- read.delim(para@sampleListFile)
    expDesign$class <- as.character(expDesign$class)
    expDesign$class[is.na(expDesign$class)] <- "QC"
    
    
    expClass <- ddply(expDesign,.(class),summarise,n=length(sample))
    expClassTB <- newTable(expClass,"Sample group information.")
    
    expBatch <- ddply(expDesign,.(batch),summarise,n=length(sample))
    expBatchTB <- newTable(expBatch,"Experiment batch information.")
    
    expTB <- newTable(expDesign,"Sample information.")
    s2_sub1 <- addTo(s2_sub1,expClassTB,expBatchTB,expTB)
    
    
    s2 <- addTo(s2,s2_sub1)
    
    s3<-newSection("Results")
    ##The first subsection
    s3_sub1 <- newSubSection(asStrong("Data quality analysis"))
    s3_sub1 <- addTo(s3_sub1,
                     newParagraph("This part contains the basic data quality."))
              
    ## 1. peak picking
    ## whether para has contained the rawPeaks
    if(ncol(para@rawPeaks)<=0){
        para <- peakFinder(para)
        if(saveRds){
            save(para,file = paste(para@outdir,"/",para@prefix,"-peakFinder.rda",
                                   sep=""))
        }
    }
    message(date(),"\treset the peaksData...")
    para <- reSetPeaksData(para = para)
    makeMetaboAnalystInput(para,rmQC=out.rmqc,valueID="value",prefix="raw")
    
    ## 2. pre-processing, filter noise peaks
    if(hasQC(para)){
        message(date(),"\tfilter peaks in QC sample...")
        pre_para <- para
        para <- filterQCPeaks(para = para,ratio = 0.5)
        rpeak <- length(unique(pre_para@peaksData$ID)) - length(unique(para@peaksData$ID))
        
        s3_sub1 <- addTo(s3_sub1,
                         newParagraph("Remove peaks in QC samples with missing 
                                      value greater than 50 percent:",rpeak,"."))
        rm(pre_para)
        
    }
    message(date(),"\tfilter peaks in non-QC sample...\n")
    pre_para <- para
    para <- filterPeaks(para = para,ratio = 0.8)
    rpeak <- length(unique(pre_para@peaksData$ID)) - length(unique(para@peaksData$ID))
    
    s3_sub1 <- addTo(s3_sub1,
                     newParagraph("Remove peaks in non-QC samples with missing 
                                  value greater than 80 percent:",rpeak,"."))
    rm(pre_para)
    
    
    ## 3. Quality control
    if(doQA==TRUE){
        message("plot peak number distribution...")
        fig <- plotPeakNumber(para)
        fig1 <- paste("data/",basename(fig$fig),sep="")
        fig2 <- paste("data/",basename(fig$highfig),sep="")
        s3_sub1 <- addTo(s3_sub1,
                         newFigure(fig1,"Peak number distribution.",
                                   fileHighRes = fig2))
        
        
        message("plot CV distribution...")
        fig <- metaX::plotCV(para)
        fig1 <- paste("data/",basename(fig$fig),sep="")
        fig2 <- paste("data/",basename(fig$highfig),sep="")
        s3_sub1 <- addTo(s3_sub1,
                         newFigure(fig1,"Peak CV distribution.",
                                   fileHighRes = fig2))
        s3_sub1 <- addTo(s3_sub1,
                         newTable(fig$stat,"CV stat."))
        
        
        message("plot missing value distribution...")
        fig <- plotMissValue(para,...)
        fig1 <- paste("data/",basename(fig$fig),sep="")
        fig2 <- paste("data/",basename(fig$highfig),sep="")
        s3_sub1 <- addTo(s3_sub1,
                         newFigure(fig1,"Missing value distribution.",
                                   fileHighRes = fig2))
        
        
        message("plot peak intensity distribution...")
        fig <- plotIntDistr(para)
        fig1 <- paste("data/",basename(fig$fig),sep="")
        fig2 <- paste("data/",basename(fig$highfig),sep="")
        s3_sub1 <- addTo(s3_sub1,
                         newFigure(fig1,"Peak intensity distribution.",
                                   fileHighRes = fig2))
        
        
        if(hasQC(para)){
            message("plot correlation heatmap...")
            fig <- plotCorHeatmap(para = para,valueID = "value",anno = TRUE,
                                  cluster = FALSE)
            fig1 <- paste("data/",basename(fig$fig),sep="")
            fig2 <- paste("data/",basename(fig$highfig),sep="")
            s3_sub1 <- addTo(s3_sub1,
                             newFigure(fig1,"Correlation heatmap.",
                                       fileHighRes = fig2))
            
        }
        message("plot TIC distribution...")
        fig <- plotPeakSumDist(para,valueID = "value")
        fig1 <- paste("data/",basename(fig$fig),sep="")
        fig2 <- paste("data/",basename(fig$highfig),sep="")
        s3_sub1 <- addTo(s3_sub1,
                         newFigure(fig1,"TIC distribution.",
                                   fileHighRes = fig2))

    }
    
    message(date(),"\tmissing value inputation...")
    para <- missingValueImpute(para,...)
    s3_sub1 <- addTo(s3_sub1,newParagraph("The missing value were imputed by ",
                                          para@missValueImputeMethod,"."))
    ## 4. remove outlier samples
    ## outlier sample remove
    if(remveOutlier){
        message(date(),"\tauto remove outlier sample...")
        pr <- metaX::normalize(para = para, method = nor.method,
                                 valueID="value")
        pr <- transformation(pr,method = t,valueID = "value")
        pr <- metaX::preProcess(pr,scale = scale,center = TRUE,
                                valueID = "value")
        ## 
        removeSampleNames <- autoRemoveOutlier(pr,outTol=outTol,scale="none",
                                               center=FALSE,valueID = "value")
        para <- removeSample(para,rsamples = removeSampleNames)
        #para@peaksData <- para@peaksData[!para@peaksData$sample %in% 
        #                                     removeSampleNames,]
        
        if(length(removeSampleNames)>=1){
            s3_sub1 <- addTo(s3_sub1,newParagraph("Remove outlier sample."))
            reTB <- dplyr::filter(expDesign,sample %in% removeSampleNames)
            s3_sub1 <- addTo(s3_sub1,newTable(reTB,"The sample removed."))
        }else{
            s3_sub1 <- addTo(s3_sub1,newParagraph("Don't find outlier sample."))
        }
    }
    

    
    if(hasQC(para) && qcsc==TRUE){
        
        if(nor.order==1){
            message(date(),"\tnormalization before QC-RLSC...")
            para <- metaX::normalize(para = para, method = nor.method,
                                     valueID="value")
            message(date(),"\tQC-RLSC...")
            res <- doQCRLSC(para = para,cvFilter = cvFilter)
            para <- res$metaXpara
            fig <- plotQCRLSC(para = para)
        }else{
            message(date(),"\tQC-RLSC...")
            res <- doQCRLSC(para = para,cvFilter = cvFilter)
            para <- res$metaXpara
            fig <- plotQCRLSC(para = para)
            message(date(),"\tnormalization after QC-RLSC...")
            para <- metaX::normalize(para = para, method = nor.method,
                                     valueID="valueNorm")
            
        }
        
        s3_sub1 <- addTo(s3_sub1,newParagraph("The data was normalized by QC-RLSC."))
        s3_sub1 <- addTo(s3_sub1,newTable(res$cvBatch,"CV summary after QC-RLSC for each batch."))
        s3_sub1 <- addTo(s3_sub1,newTable(res$cvAll,"CV summary after QC-RLSC for all samples."))
        fig1 <- paste("data/",basename(fig$fig),sep="")
        fig2 <- paste("data/",basename(fig$highfig),sep="")
        s3_sub1 <- addTo(s3_sub1,
                         newFigure(fig1,"QC-RLSC figure.",
                                   fileHighRes = fig2))
        
    }else{
        para@peaksData$valueNorm <- para@peaksData$value
        para <- metaX::normalize(para = para, method = nor.method,
                                 valueID="valueNorm")
        
        para <- filterQCPeaksByCV(para,cvFilter = cvFilter,
                                  valueID = "valueNorm")
    }
    
    
    if(pclean==TRUE){
        message("data clean...")
        para <- dataClean(para,valueID = "valueNorm",...)
    }
    
    if(doQA==TRUE){
        if(hasQC(para)){
            s3_sub1 <- addTo(s3_sub1,
                             newParagraph("Data quality check after 
                                          normalization and pre-processing."))
            
            
            message("plot correlation heatmap...")
            prefix <- para@prefix
            para@prefix <- paste(prefix,"-nor",sep="")
            fig <- plotCorHeatmap(para = para,valueID = "valueNorm",anno = TRUE,
                                  cluster = FALSE)
            fig1 <- paste("data/",basename(fig$fig),sep="")
            fig2 <- paste("data/",basename(fig$highfig),sep="")
            s3_sub1 <- addTo(s3_sub1,
                             newFigure(fig1,"Correlation heatmap.",
                                       fileHighRes = fig2))
            
            
            message("plot peak intensity distribution...")
            pp <- para
            pp@peaksData$value <- pp@peaksData$valueNorm
            fig <- plotIntDistr(pp)
            fig1 <- paste("data/",basename(fig$fig),sep="")
            fig2 <- paste("data/",basename(fig$highfig),sep="")
            s3_sub1 <- addTo(s3_sub1,
                             newFigure(fig1,"Peak intensity distribution.",
                                       fileHighRes = fig2))
            
            
            fig <- plotHeatMap(pp,valueID="valueNorm",log=TRUE,rmQC=FALSE,
                               scale="row",
                               clustering_distance_rows="euclidean",
                               clustering_distance_cols="euclidean",
                               clustering_method="ward.D2",
                               show_colnames=FALSE)
            fig1 <- paste("data/",basename(fig$fig),sep="")
            fig2 <- paste("data/",basename(fig$highfig),sep="")
            s3_sub1 <- addTo(s3_sub1,
                             newFigure(fig1,"Heatmap.",
                                       fileHighRes = fig2))
            
            
            rm(pp)
            para@prefix <- prefix
        }
    }
    
    makeMetaboAnalystInput(para,rmQC=out.rmqc,valueID="valueNorm",
                           prefix="norm")
    
    ppca <- transformation(para,method = t,valueID = "valueNorm")
    ppca <- metaX::preProcess(ppca,scale = scale,center = TRUE,
                              valueID = "valueNorm")
    fig <- metaX::plotPCA(ppca,valueID = "valueNorm",scale = "none",batch = TRUE,
                          rmQC = FALSE)
    fig1 <- paste("data/",basename(fig$fig),sep="")
    fig2 <- paste("data/",basename(fig$highfig),sep="")
    s3_sub1 <- addTo(s3_sub1,
                     newFigure(fig1,"PCA.",
                               fileHighRes = fig2))
    s3 <- addTo(s3,s3_sub1)
    
    #plotPLSDA(para)
    para <- peakStat(para = para,plsdaPara = plsdaPara,doROC=doROC)
    
    
    
    
    ## add identification result
    if(!is.null(idres)){
        para <- addIdentInfo(para=para,file=idres)
    }
    
    s3_sub2 <- newSubSection(asStrong("Metabolite quantification and identification."))
    s3_sub2 <- addTo(s3_sub2,
                     newParagraph("This part contains the quantification and identification."))
    
    qf <- paste("data/",para@prefix,"-quant.txt",sep="")
    show_quant <- para@quant[1:20,]
    names(show_quant) <- gsub(pattern = "_",replacement = "\n",x=names(show_quant))
    s3_sub2 <- addTo(s3_sub2,
                     newTable(show_quant,"Metabolite quantification result.",file=qf))
    
    s3 <- addTo(s3,s3_sub2)
    if(saveRds){
        saveRDS(para,file = paste(para@outdir,"/",para@prefix,"-result.rds",
                                  sep=""))
    }
    
    report<-addTo(report,s1,s2,s3)
    message("Write report to file:",paste(raw_outdir,"/report",sep=""))
    writeReport(report, filename = paste(raw_outdir,"/report",sep=""))
    
    return(para)         
             
}
)


.tTest=function(x,y,log2=TRUE){
    if(log2){
        sv <- !is.na(x) & x>0 & is.finite(x)
        x <- log2(x[sv])
        y <- y[sv]
    }
    z <- t.test(x~y)
    return(z$p.value)
}


isValid=function(x){
    return(x>0 & !is.na(x) & is.finite(x))
    
}

tuneSpline = function(x,y,span.vals=seq(0.1,1,by=0.05)){
    mae <- numeric(length(span.vals))
    
    crossEva <- function(span,x,y) {
        
        fun.fit <- function(x,y,span) {smooth.spline(x = x,y =y ,spar = span)}
        fun.predict <- function(fit,x0) {predict(fit,x0)$y}
        y.cv <- bootstrap::crossval(x,y,fun.fit,fun.predict,span=span,
                                    ngroup = length(x))$cv.fit
        fltr <- !is.na(y.cv)
        return(mean(abs(y[fltr]-y.cv[fltr])))
        
    }
    
    mae <- sapply(span.vals,crossEva,x=x,y=y)
    span <- span.vals[which.min(mae)]
    return(span)
}


myLoessFit = function(x,y,newX,span.vals=seq(0.1,1,by=0.05),log=TRUE,a=1){
    if(log==TRUE){
        y <- glog(y,a = a)
    }
    #sp.obj <- smooth.spline(x,y,spar = tuneSpline(x,y,span.vals = span.vals))
    sp.obj <- smooth.spline(x,y,cv = TRUE)
    
    valuePredict=predict(sp.obj,newX)
    if(log==TRUE){
        valuePredict$y <- glog(valuePredict$y,a = a,inverse = TRUE)
    }
    return(valuePredict$y)
}



##' @title Remove samples from the metaXpara object
##' @description Remove samples from the metaXpara object
##' @rdname removeSample
##' @docType methods
##' @param para A metaXpara object.
##' @param rsamples The samples needed to be removed
##' @param ... Other argument
##' @return A metaXpara object.
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' new_para <- removeSample(para,rsamples=c("batch01_QC01"))
setGeneric("removeSample",function(para,rsamples,...) 
    standardGeneric("removeSample"))
##' @describeIn removeSample
setMethod("removeSample", signature(para = "metaXpara"),
          function(para,rsamples,...){
    para@peaksData <- dplyr::filter(para@peaksData,!sample %in% rsamples)
    return(para)
})


##' @title Add identification result into metaXpara object
##' @description Add identification result into metaXpara object
##' @rdname addIdentInfo
##' @docType methods
##' @param para A metaXpara object.
##' @param file The file name which contains the identification result
##' @param ... Other argument
##' @return A metaXpara object.
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
setGeneric("addIdentInfo",function(para,file,...) 
    standardGeneric("addIdentInfo"))
##' @describeIn addIdentInfo
setMethod("addIdentInfo", signature(para = "metaXpara",file="character"),
          function(para,file,...){
    
    if(str_detect(tolower(file),pattern = ".csv$")){
        message("The identification result file is csv format!")
        res <- read.csv(file,stringsAsFactors=FALSE)
    }else{
        message("The identification result file is txt format!")
        res <- read.delim(file,stringsAsFactors=FALSE)
    }
    ## whether there is a column named "ID"
    checkID <- which(names(res)=="ID")
    if(length(checkID)==0){
        ## no found, ## the first column must be peak ID
        names(res)[1] <- "ID"
    }else if(length(checkID)>=2){
        stop("There are more than 2 column named ID,please check your data!\n")
    }else{
        ## don't need to do anything.
    }
    
    ## please note that maybe there are more than one rows corresponding to 
    ## one peak
    
    ## only output merged result to a file
    if(!is.null(para@quant)){
        ## merge
        ## firstly, check whether there is duplicate column in the two table
        cID <- sum(names(res) %in% names(para@quant))
        if(cID==1){
            idres <- dplyr::left_join(para@quant,res,by="ID")
            message("Peak number: ",length(unique(para@quant$ID)))
            message("Identified peak: ",sum(unique(para@quant$ID) %in% res$ID))
            rfile <- paste(para@outdir,"/",para@prefix,
                           "-quant-identification.txt",sep="")
            message("Merge identification and quantification result and save to", 
                " file ",rfile," ...")
            write.table(idres,file=rfile,col.names = TRUE,row.names = FALSE,
                        quote=FALSE,sep="\t")
            
        }else{
            stop("There are more than two column contained in quantification", 
                 "result! Please check your data!\n")
        }
        
    }
    
    para@idres <- res
    return(para)
})


##' @title Plot boxplot for each feature
##' @description Plot boxplot for each feature
##' @rdname plotPeakBox
##' @docType methods
##' @param para A metaXpara object
##' @param samples Sample class used
##' @param log Whether log transform or not
##' @param ... Additional parameters
##' @return The output figure name.
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)[1:20,]
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' addValueNorm(para) <- para
##' plotPeakBox(para,samples=c("S","C"))
setGeneric("plotPeakBox",function(para,samples,log=FALSE,...) 
    standardGeneric("plotPeakBox"))
##' @describeIn plotPeakBox
setMethod("plotPeakBox", signature(para = "metaXpara"),
          function(para,samples,log=FALSE,...){
    
    para@peaksData$class <- as.character(para@peaksData$class)
    peaksData <- dplyr::filter(para@peaksData,class %in% samples)
    
    ## whther the data is valid
    if(log==TRUE){
        ## can't have zero or negative value
        if(sum(peaksData$valueNorm<=0)>=1){
            message("value <= 0 ",sum(peaksData$valueNorm<=0))
            stop("cannot handle with <=0 value when log...\n")
        }else{
            peaksData$valueNorm <- log2(peaksData$valueNorm)
        }
    }
    
    fig <- paste(para@outdir,"/",para@prefix,"-peakboxplot-",
                 paste(samples,collapse = "_"),".pdf",sep="")
    pdf(fig,width = 4,height = 4)
    
    figres <- ddply(peaksData,.(ID),function(dat){
        dat$class <- as.factor(dat$class)
        ggobj <- ggplot(data=dat,aes(x=class,y=valueNorm))+
                    geom_boxplot(width = 0.3)+
                    theme(legend.justification=c(1,1), 
                          legend.position=c(1,1))+
                    geom_jitter(position = position_jitter(width = 0.1),
                                aes(colour=class))+
                    ggtitle(dat$ID[1])+
                    ylab("Intensity")
                                
        print(ggobj)
        NA
    })
    dev.off()
    #return(para)
})



##' @title Plot the total peak intensity distribution
##' @description Plot the total peak intensity distribution
##' @rdname plotPeakSumDist
##' @docType methods
##' @param para A metaXpara object.
##' @param valueID The name of the column used
##' @param width Width of the figure
##' @param height Height of the figure
##' @param ... Other argument
##' @return The output figure name.
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' plotPeakSumDist(para)
setGeneric("plotPeakSumDist",function(para,valueID="value",width=6,height=4,...) 
    standardGeneric("plotPeakSumDist"))
##' @describeIn plotPeakSumDist
setMethod("plotPeakSumDist", signature(para = "metaXpara"),
          function(para,valueID="value",width=6,height=4,...){
    
    dat <- ddply(para@peaksData,.(sample,batch,class,order),here(summarise),
                   sum=sum(get(valueID)))
    
    dat$batch <- as.character(dat$batch)
    dat$class <- as.character(dat$class)
    dat$class[is.na(dat$class)] <- "QC"
    
    outliers.coef <- 1.5
    qs <- quantile(dat$sum,c(.25,.75),na.rm=TRUE)
    iqr <- qs[2] - qs[1]
    dat$outlier <- dat$sum < qs[1]-outliers.coef*iqr | 
        dat$sum > qs[2]+outliers.coef*iqr
    
    fig <- paste(para@outdir,"/",para@prefix,"-peakSumDist.png",sep="")
    highfig <- sub(pattern = "png$",replacement = "pdf",x = fig)
    pdf(file = highfig,width = width,height = height)
    
    ggobj <- ggplot(data=dat,aes(x=order,y=sum,colour=class,shape=batch))+
        geom_point()+
        geom_text(aes(label=ifelse(outlier,order,"")),hjust=-0.2,size=4)+
        ylab("Total peak intensity")
    
    print(ggobj)
    dev.off()
    png(filename = fig,width = width,height = height,units = "in",res = 150)
    print(ggobj)
    dev.off()
    res <- list(fig=fig,highfig=highfig)
    return(res)
    
})


##' @title dataClean
##' @description dataClean
##' @rdname dataClean
##' @docType methods
##' @param para A metaXpara object.
##' @param valueID The name of the column used
##' @param sd.factor The factor used to filter peak based on SD
##' @param snr The threshold to filter peak
##' @param ... Other argument
##' @return A metaXpara object.
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' p <- dataClean(para)
setGeneric("dataClean",function(para,valueID="value",sd.factor=3,snr=1,...) 
    standardGeneric("dataClean"))
##' @describeIn dataClean
setMethod("dataClean", signature(para = "metaXpara"),
          function(para,valueID="value",sd.factor=3,snr=1,...){
    
    peaksData <- para@peaksData
    ## true sample
    peaksData1 <- dplyr::filter(peaksData,!is.na(class))
    ## QC sample
    peaksData2 <- dplyr::filter(peaksData,is.na(class)) 
    
    dat1 <- ddply(peaksData1,.(ID),here(summarise),
                  mean=mean(get(valueID),na.rm = TRUE),
                  sd=sd(get(valueID),na.rm = TRUE))
    
    dat2 <- ddply(peaksData2,.(ID),here(summarise),
                  meanQC=mean(get(valueID),na.rm = TRUE),
                  sdQC=sd(get(valueID),na.rm = TRUE))
    
    dat <- dplyr::full_join(dat1,dat2,by="ID")
    dat_remove <- dplyr::mutate(dat,remove=abs(meanQC-mean) >sd.factor*sd,
                                SNR=sd/sdQC)
    
    tmpPeaksdData <- peaksData
    tmpPeaksdData$val <- peaksData[,valueID]
    kw_pvalue <- ddply(tmpPeaksdData,.(ID),summarise,
                       pvalue=kruskal.test(val~batch)$p.value)
    dat <- dplyr::full_join(dat_remove,kw_pvalue,by="ID")
    
    rID <- dat_remove$ID[dat_remove$remove==TRUE | dat_remove$SNR < snr]
    
    save2file <- paste(para@outdir,"/",para@prefix,"-dataClean.txt",sep="")
    message("Save removed data to file:",save2file)
    write.table(dat_remove[dat_remove$ID%in%rID,],file = save2file,
                quote = FALSE,sep="\t",
                row.names = FALSE)
    
    message("remove ID: ",length(rID))
    
    ## output the removed feature
    rPeak <- dplyr::filter(peaksData,ID%in%rID)
    rPeak$class <- as.character(rPeak$class)
    rPeak$class[is.na(rPeak$class)] <- "QC"
    rPeak$batch <- as.character(rPeak$batch)
    
    fig <- paste(para@outdir,"/",para@prefix,"-dataClean.pdf",sep="")
    pdf(fig,width = 6,height = 6)
    for(id in unique(rPeak$ID)){
        pdat <- dplyr::filter(rPeak,ID==id)
        pdat$val <- pdat[,valueID]
        tmp <- dplyr::filter(dat_remove,ID==id)
        gtitle <- paste(id,tmp$remove,tmp$SNR,sep="_")
        ggobj <- ggplot(pdat,aes(x=order,y=val,
                                 colour=class,shape=batch))+
            geom_point()+
            ylab(valueID)+
            ggtitle(label = gtitle)
        print(ggobj)    
        
    }
    dev.off()
    
    para@peaksData <-dplyr::filter(peaksData,!ID%in%rID)
    return(para)
})



