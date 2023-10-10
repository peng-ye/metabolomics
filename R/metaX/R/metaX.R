##' An S4 class to represent the parameters and data for data processing
##' 
##' @export
##' @param para A metaXpara object
##' @param value New value
##' @slot dir.case The path names of the NetCDF/mzXML files to read
##' @slot dir.ctrl The path names of the NetCDF/mzXML files to read
##' @slot sampleListFile The file name of containing the experiment design
##' @slot sampleList A data.frame containing the experiment design
##' @slot ratioPairs A character containing the ratio pairs, such as "A:B;A:C"
##' @slot missValueImputeMethod A character of missing value imputation method
##' @slot sampleListHead The name of head of \code{sampleListFile}
##' @slot outdir The output directory
##' @slot prefix The prefix of output file
##' @slot xcmsPeakListFile The file of output from \pkg{XCMS}
##' @slot fig A \code{list} of file names of figures
##' @slot peaksData A \code{data.frame} containing the peaks data
##' @slot VIP A \code{data.frame} containing the VIP
##' @slot rawPeaks A \code{data.frame} containg the raw peaks data
##' @slot xcmsSetObj An object of \code{\link{xcmsSet}}
##' @slot quant A \code{data.frame} containing the quantification result
##' @slot idres A \code{data.frame} containing the identification result
##' @slot xcmsSet.method Method to use for peak detection. See details 
##' \code{\link{findPeaks}} in package \pkg{XCMS}
##' @slot xcmsSet.ppm The maxmial tolerated m/z deviation in consecutive scans, 
##' in ppm (parts per million)
##' @slot xcmsSet.peakwidth Chromatographic peak width, given as 
##' range (min,max) in seconds
##' @slot xcmsSet.snthresh The signal to noise ratio cutoff, definition see 
##' \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.prefilter prefilter=c(k,I), 
##' see \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.mzCenterFun See \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.integrate See \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.mzdiff See \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.noise See \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.verbose.columns See \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.polarity Filter raw data for positive/negative scans. 
##' See \code{\link{xcmsSet}}
##' @slot xcmsSet.profparam Parameters to use for profile generation. 
##' See \code{\link{xcmsSet}}
##' @slot xcmsSet.nSlaves The number of slaves/cores to be used for parallel 
##' peak detection. See \code{\link{xcmsSet}}
##' @slot xcmsSet.fitgauss See \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.sleep The number of seconds to pause between plotting 
##' peak finding cycles. See \code{\link{findPeaks.centWave}}
##' @slot xcmsSet.fwhm See \code{\link{findPeaks.matchedFilter}}
##' @slot xcmsSet.max See \code{\link{findPeaks.matchedFilter}}
##' @slot xcmsSet.step See \code{\link{findPeaks.matchedFilter}}
##' @slot group.bw0 See \code{\link{group.density}}
##' @slot group.mzwid0 See \code{\link{group.density}}
##' @slot group.bw See \code{\link{group.density}}
##' @slot group.mzwid See \code{\link{group.density}}
##' @slot group.minfrac See \code{\link{group.density}}
##' @slot group.minsamp See \code{\link{group.density}}
##' @slot group.max See \code{\link{group.density}}
##' @slot group.sleep See \code{\link{group.density}}
##' @slot retcor.method See \code{\link{retcor}}
##' @slot retcor.profStep See \code{\link{retcor.obiwarp}}
##' @slot retcor.plottype See \code{\link{retcor.obiwarp}}
##' @slot qcRlscSpan The value of span for QC-RLSC
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @return A object of metaXpara
setClass("metaXpara", slots=c(
    ## input
    dir.case = "character",
    dir.ctrl = "character",
    sampleListFile = "character",
    sampleListHead = "character",
    sampleList = "data.frame",
    ratioPairs = "character",
    missValueImputeMethod = "character",
    
    ## output
    outdir = "character",
    prefix = "character",
    xcmsPeakListFile = "character",
    fig = "list",
    peaksData = "data.frame",
    VIP = "data.frame",
    #sampleList = "data.frame",
    rawPeaks = "data.frame",
    xcmsSetObj = "xcmsSet", 
    quant = "data.frame",
    
    ## identification result
    idres = "data.frame",
    
    xcmsSet = representation(
        method = "character",                      
        ppm = "numeric",                     
        peakwidth = "numeric",                      
        snthresh = "numeric", 
        prefilter = "numeric",      
        mzCenterFun = "character",       
        integrate = "numeric",
        mzdiff = "numeric",
        noise = "numeric",
        verbose.columns = "logical",
        polarity = "character", 
        profparam = "list", 
        nSlaves = "numeric" ,
        fitgauss = "logical",
        sleep = "numeric",
        
        ## findPeaks.matchedFilter-methods 
        fwhm = "numeric",
        max = "numeric",
        step = "numeric"
        
        
    ),
    group = representation(
        ## The first round
        bw0 = "numeric",                
        mzwid0 = "numeric",
        
        ## The second round
        bw = "numeric",                
        mzwid = "numeric",             
        minfrac = "numeric",          
        minsamp = "numeric",        
        max = "numeric",          
        sleep = "numeric"
    ),
    
    retcor = representation(
        method = "character",
        profStep = "numeric",
        plottype = "character"
    ),
    
    qcRlscSpan = "numeric"
    
    
    
),
prototype = prototype(xcmsSet.method = "centWave",
                      xcmsSet.ppm = 10,
                      xcmsSet.peakwidth = c(5,15),
                      xcmsSet.snthresh = 6,
                      xcmsSet.prefilter = c(1,5000),
                      xcmsSet.mzCenterFun = "wMean",
                      xcmsSet.integrate = 1,
                      xcmsSet.mzdiff = -0.001,
                      xcmsSet.noise = 5000,
                      xcmsSet.verbose.columns = TRUE,
                      xcmsSet.polarity = "positive",
                      xcmsSet.profparam = list(step=0.005),
                      xcmsSet.nSlaves = 1,
                      xcmsSet.fitgauss = FALSE,
                      xcmsSet.sleep = 0,
                      
                      ## findPeaks.matchedFilter
                      xcmsSet.fwhm = 30,
                      xcmsSet.max = 5,
                      xcmsSet.step = 0.1,
                      
                      group.bw0 = 10,
                      group.mzwid0 = 0.015,
                      
                      group.bw = 5,
                      group.mzwid = 0.015,
                      group.minfrac = 0.3,
                      group.sleep = 0,
                      group.minsamp = 1,
                      group.max = 1000,
                      
                      retcor.method = "obiwarp",
                      retcor.plottype = "deviation",
                      retcor.profStep = 0.005,
                      
                      
                      outdir = "./",
                      prefix = "metaX",
                      peaksData = NULL,
                      VIP = NULL,
                      ratioPairs = NULL,
                      qcRlscSpan = 0,
                      missValueImputeMethod = "knn",#"svdImpute",
                      quant = NULL,
                      idres = NULL,
                      
                      sampleListHead = c(sample="sample",
                                         order="order",
                                         batch="batch",
                                         class="class",
                                         isQC="isQC")
                      
                      
                      
)
)


## show function for metaXpara
setMethod("show","metaXpara",function(object){
    samList <- read.delim(object@sampleListFile)
    message("total samples:")
    message(nrow(samList))
    samList$class <- as.character(samList$class)
    samList$class[is.na(samList$class)] <- "QC"
    message("sample group information:")
    sl <- as.data.frame(table(samList$class,useNA = "ifany"))
    names(sl) <- c("Group","Number of samples")
    print(sl)
    message("batch information:")
    bl <- as.data.frame(table(samList$batch))
    names(bl) <- c("Batch","Number of samples")
    print(bl)
})


setMethod("initialize", "metaXpara", function(.Object, ...) {
    .Object <- callNextMethod()
    if(is.null(.Object@outdir)){
        stop("Please set the outdir parameter!") 
    }else{
        if(!isDirectory(.Object@outdir)){
            dir.create(.Object@outdir)
        }
    }
    .Object
})



##' An S4 class to represent the parameters for PLS-DA analysis
##' 
##' @export
##' @param para A metaXpara object
##' @param value New value
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @slot scale The method used to scale the data, 
##' see \code{\link{preProcess}} in \pkg{metaX}
##' @slot center A logical which indicates if the matrix should be mean 
##' centred or not
##' @slot t The method used to transform the data, 
##' see \code{\link{transformation}} in \pkg{metaX}
##' @slot validation The method for validation, default is "CV"
##' @slot ncomp The number of components used for PLS-DA, default is 2
##' @slot nperm The number of permutations, default is 200
##' @slot kfold The number of folds for cross-validation, default is 7
##' @slot do A logical which indicates whether to do the plsDA analysis, 
##' default is TRUE
##' @slot method The method used in PLS-DA. See \code{\link{plsr}} 
##' in \pkg{pls}
##' @return A object of plsDAPara
setClass("plsDAPara", slots=c(
    scale = "character",
    center = "logical",
    t = "numeric",
    validation = "character",
    ncomp = "numeric",
    nperm = "numeric",
    kfold = "numeric",
    do = "logical",
    method = "character"),
    prototype = prototype(scale = "uv",
                          center = TRUE,
                          t = 1, #1=log,2=cubic
                          validation = "CV",#LOO
                          ncomp = 2,
                          nperm = 200,
                          kfold = 7,
                          do = TRUE,
                          method = "oscorespls")
)


##' @title Create directory
##' @description Create directory
##' @rdname makeDirectory
##' @docType methods
##' @param para A metaXpara object
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @return NULL
##' @examples
##' para <- new("metaXpara")
##' outdir(para) <- "outdir"
##' makeDirectory(para)
setGeneric("makeDirectory",function(para) standardGeneric("makeDirectory"))
##' @describeIn makeDirectory
setMethod("makeDirectory", signature(para = "metaXpara"), function(para){
    if(!isDirectory(para@outdir)){
        dir.create(para@outdir,recursive=TRUE)
    }
}
)


##' @title Peak detection by using XCMS package
##' @description  peakFinder takes a set of MS sample data and performs a peak 
##' detection, retention time correction and peak grouping steps using XCMS 
##' package.
##' @rdname peakFinder
##' @docType methods
##' @param para A metaXpara object
##' @param ... Additional parameter
##' @return A metaXpara object
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' \dontrun{
##' library(faahKO)
##' para <- new("metaXpara")
##' dir.case(para) <- system.file("cdf/KO", package = "faahKO")
##' dir.ctrl(para) <- system.file("cdf/WT", package = "faahKO")
##' ## set parameters for peak picking
##' xcmsSet.peakwidth(para) <- c(20,50)
##' xcmsSet.snthresh(para) <- 10
##' xcmsSet.prefilter(para) <- c(3,100)
##' xcmsSet.noise(para) <- 0
##' xcmsSet.nSlaves(para) <- 4
##' ## run peak picking
##' p <- peakFinder(para)
##' }
setGeneric("peakFinder",function(para,...) standardGeneric("peakFinder"))
##' @describeIn peakFinder
setMethod("peakFinder", signature(para = "metaXpara"), function(para,...){
    message(date(),"\txcmsSet...")
    makeDirectory(para)
    sampleClassA = basename(para@dir.case)
    sampleClassB = basename(para@dir.ctrl)
    
    msFiles <- c(list.files(para@dir.case,recursive=FALSE,
                            full.names=TRUE),
                 list.files(para@dir.ctrl,recursive=FALSE,
                            full.names=TRUE))
    xset<-xcmsSet(msFiles,
                  method = para@xcmsSet.method,
                  ppm = para@xcmsSet.ppm,
                  peakwidth = para@xcmsSet.peakwidth,
                  snthresh = para@xcmsSet.snthresh,
                  prefilter = para@xcmsSet.prefilter,
                  mzCenterFun = para@xcmsSet.mzCenterFun,
                  integrate = para@xcmsSet.integrate,
                  mzdiff = para@xcmsSet.mzdiff,
                  noise = para@xcmsSet.noise,
                  verbose.columns = para@xcmsSet.verbose.columns,
                  fitgauss = para@xcmsSet.fitgauss,
                  sleep = para@xcmsSet.sleep,
                  polarity = para@xcmsSet.polarity,
                  #profparam = para@xcmsSet.profparam,
                  nSlaves = para@xcmsSet.nSlaves,...
    )
    
    message(date(),"\txcmsSet done!")
    
    xset
    
    message(date(),"\tgroup...")
    xset1<-group(xset,bw = para@group.bw0, mzwid = para@group.mzwid0)
    
    
    retcor.pdf <- paste(para@outdir,"/", para@prefix, "-retcor.pdf",
                        sep="")
    pdf(retcor.pdf)
    if(para@retcor.method == "obiwarp"){
        xset2<-retcor(xset1, method="obiwarp",
                      profStep = para@retcor.profStep,
                      plottype = para@retcor.plottype,...)
    }else{
        xset2<-retcor(xset1, method=para@retcor.method,
                      plottype = para@retcor.plottype,...)
        
    }
    dev.off()
    
    #xset2<-group(xset2,bw=5,mzwid=0.01,minfrac=0.5,minsamp=1,max=1000,sleep=0)
    xset2<-group(xset2,
                 bw = para@group.bw, 
                 mzwid = para@group.mzwid,
                 minfrac = para@group.minfrac,
                 minsamp = para@group.minsamp,
                 max = para@group.max,
                 sleep = para@group.sleep,...)
    
    
    message(date(),"\tfillpeaks...")
    
    xset3<-fillPeaks(xset2)
    xset3
    message(date(),"\tfillpeaks done!")
    
    
    para@xcmsSetObj <- xset3
    reporttab<-diffreport(xset3,sampleClassA,sampleClassB,
                          paste(para@outdir,"/",para@prefix,sep=""),
                          15000,sortpval=FALSE,
                          h=480,w=640)
    reporttab[1:4,]
    
    message(date(),"\tannotate...")
    
    ## In parallel mode, the adducts result are empty.
    xsa<-xsAnnotate(xset3,
                    #nSlaves=para@xcmsSet.nSlaves,
                    polarity = para@xcmsSet.polarity)
    xsaF<-groupFWHM(xsa)
    
    xsaFI<-findIsotopes(xsaF,
                        ppm = para@xcmsSet.ppm
                        #polarity = para@xcmsSet.polarity,
                        #maxcharge=3,
                        #maxiso=3,
                        #ppm=10,
                        #mzabs=0.01
    )
    xsaC<-groupCorr(xsaFI,calcIso=TRUE,calcCiS=TRUE,calcCaS=TRUE)
    xsaFA<-findAdducts(
        xsaC,
        ppm=para@xcmsSet.ppm,
        #mzabs=0.01,
        #multiplier=3,
        polarity = para@xcmsSet.polarity
    )
    peaklist<-getPeaklist(xsaFA)
    
    message(date(),"\tannotate done")
    
    reporttab.combine<-cbind(reporttab,
                             peaklist[,c("isotopes","adduct","pcgroup")])
    
    featureFile = paste(para@outdir, "/", para@prefix, "-feature.txt",
                        sep="") 
    message("Save the feature data to file: ",featureFile)
    write.table(reporttab.combine, file = featureFile,
                quote = FALSE, 
                sep="\t", 
                row.names = FALSE, 
                col.names = TRUE)
    
    para@rawPeaks <- reporttab.combine
    
    date()
    para@xcmsPeakListFile = featureFile
    return(para)
    
}
)


##' @title Normalisation of peak intensity
##' @description The normalize method performs normalisation on peak 
##' intensities.
##' @rdname normalize
##' @docType methods
##' @param para A metaXpara object.
##' @param method The normalization method: sum, vsn, quantiles, 
##' quantiles.robust, sum, pqn. Default is sum.
##' @param valueID The name of the column which will be normalized.
##' @param ... Additional parameter
##' @return A metaXpara object.
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' para <- reSetPeaksData(para)
##' para <- metaX::normalize(para)
setGeneric("normalize",function(para,method="sum",valueID="value",...) 
    standardGeneric("normalize"))
##' @describeIn normalize
setMethod("normalize",signature(para="metaXpara"),
          function(para,method="sum",valueID="value",...){
              message("normalize: ",valueID)
              para@peaksData->x
              x<-dcast(x,ID~sample,value.var = valueID)
              dat <- as.matrix(x[,-1])
              
              if(method == "vsn"){
                  fdata <- ExpressionSet(assayData=dat)
                  fit<-vsn2(fdata)
                  fitdata = predict(fit, newdata=fdata)  ## apply fit
                  nd<-exprs(fitdata)
                  x[,-1] <- 2^nd
                  
              }else if(method == "quantiles"){
                  x[,-1] <- preprocessCore::normalize.quantiles(dat)
                  
              }else if(method == "quantiles.robust"){
                  x[,-1] <- preprocessCore::normalize.quantiles.robust(dat)
                  
              }else if(method == "sum"){
                  x[,-1] <- apply(dat,2,function(x){x/sum(x,na.rm=TRUE)})
                  
              }else if(method == "pqn"){
                  ## Normalization of data with the Probabilistic Quotient 
                  ## Normalization method.
                  ## "pqn": the Probabilistic Quotient Normalization is computed as 
                  ## described in Dieterle, et al. (2006).
                  x[,-1] <- apply(dat,2,function(y){y/sum(y,na.rm=TRUE)})
                  newX <- t(apply(x[,-1],1,function(y){y/median(y,na.rm=TRUE)}))
                  coe <- apply(newX, 2, median, na.rm = TRUE)
                  for(i in 2:ncol(x)){
                      x[,i] <- x[,i]/coe[i-1]
                  }
                  
              }#else{
              #   stop("Normalization method must be in ",
              #        "'vsn,quantiles,quantiles.robust,sum'\n")
              #}
              
              if(method != "none"){
                  normValue <- melt(x,id.vars="ID",value.name=valueID,
                                    variable.name="sample")
                  para@peaksData[,valueID] <- NULL
                  para@peaksData <- plyr::join(para@peaksData,normValue,
                                               by = c("ID","sample"))
              }
              
              
              
              
              
              return(para)
              
          }
          
)


##' @title Automatically detect outlier samples
##' @description Automatically detect outlier samples 
##' @rdname autoRemoveOutlier
##' @docType methods
##' @param para A metaXpara object
##' @param outTol A factor to define the outlier tolerance, default is 1.2
##' @param pcaMethod See \code{\link{pca}} in \pkg{pcaMethods}
##' @param valueID The name of the column which will be used
##' @param scale Scaling, see \code{\link{pca}} in \pkg{pcaMethods}
##' @param center Centering, see \code{\link{pca}} in \pkg{pcaMethods}
##' @param ... Additional parameter
##' @return The name of outlier samples
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' rs <- autoRemoveOutlier(para,valueID="value")
setGeneric("autoRemoveOutlier",
           function(para,outTol=1.2,pcaMethod="svdImpute",
                    valueID="valueNorm",scale="none",center=FALSE,...) 
               standardGeneric("autoRemoveOutlier"))
##' @describeIn autoRemoveOutlier
setMethod("autoRemoveOutlier", signature(para = "metaXpara"), 
          function(para,outTol=1.2,pcaMethod="svdImpute",valueID="valueNorm",
                   scale="none",center=FALSE,...){
              
              out.tol <- outTol
              prefix <- para@prefix
              outdir <- para@outdir 
              sampleList  <- read.delim(para@sampleListFile)
              
              
              ## Removal of outliers :performs automated removal of outliers in the
              ## pre-processed data based on expansion of the Hotellings T2 
              ## distribution ellipse.Ref:http://www.ncbi.nlm.nih.gov/pubmed/25348215
              para@peaksData->x
              x<-dcast(x,ID~sample,value.var = valueID)
              x$ID <- NULL
              
              allSampleNames <- names(x)
              #               sample2class <- data.frame(sample=names(x))
              #               sample2class <- merge(sample2class,samList,by="sample",
              #                                       sort=FALSE)
              #               sample2class$class[is.na(sample2class$class)] <- "QC"
              #               sample2class$class <- is.factor(sample2class$class)
              
              outLog <- paste(outdir,"/",prefix,"-autoRemoveOutlier.log",sep="")
              cat(date(),"\n",file = outLog)
              ## x is a data.frame, each col is a sample, each row is a feature
              
              ## The 1st round
              ## Please notice the impact of the missing value and outlier 
              ## sample.
              pca.res <- pca(t(x), nPcs = 2, method=pcaMethod,scale=scale, 
                             center=center, cv="q2",seed=123)
              
              outFig <- paste(outdir,"/",prefix,"-autoRemoveOutlier.pdf",sep="")
              pdf(outFig)
              
              plotData <- data.frame(x=pca.res@scores[,1],
                                     y=pca.res@scores[,2],
                                     sample=names(x))
              plotData <- merge(plotData,sampleList,by="sample",sort=FALSE)
              plotData$class <- as.character(plotData$class)
              plotData$class[is.na(plotData$class)] <- "QC" 
              ggobj <-ggplot(data = plotData,aes(x=x,y=y,colour=class))+
                  geom_point()+
                  xlab("PC1")+
                  ylab("PC2")+
                  theme(legend.justification=c(1,1), legend.position=c(1,1))
              print(ggobj)
              
              plot(pca.res@scores[,1],pca.res@scores[,2],main="Raw PCA plot",
                   xlab="PC1",ylab="PC2")
              abline(h = 0,v = 0,lty=2,col="gray")
              text(pca.res@scores[,1],pca.res@scores[,2],labels = names(x),cex=0.5,
                   col = ifelse(names(x) %in% sampleList$sample[is.na(sampleList$class)],
                                "red","blue"))
              
              
              scores <- pca.res@scores
              HotEllipse<-abs(cbind(HotE(scores[,1],scores[,2])))*out.tol
              outliers<-as.numeric()
              for (i in 1:nrow(scores)){
                  
                  sample<-abs(scores[i,])
                  out.PC1<-which(HotEllipse[,1]<sample[1])
                  out.PC1.PC2<-any(HotEllipse[out.PC1,2]<sample[2])*1
                  
                  outlier<-ifelse(out.PC1.PC2>0,1,0)#+out.PC1.PC3+out.PC2.PC3>0,1,0)
                  outliers<-c(outliers,outlier)
              }
              #outliers
              plotPcs(pca.res, type = "scores",col=outliers+1,pch=19,cex=0.8,
                      main="1st PCA model")
              
              outliers<-which(outliers==1)
              cat("Outliers were detected in the 1st PCA model calculation and removed:",
                  names(x)[outliers],"\n")
              cat("Outliers were detected in the 1st PCA model calculation and removed:",
                  names(x)[outliers],"\n",file = outLog,append = TRUE)
              
              if(length(outliers)>=1){
                  
                  ## The 2st round
                  x <- x[,-outliers]
                  pca.res2 <- pca(t(x), nPcs = 2, method=pcaMethod,scale=scale, 
                                  center=center, cv="q2",seed=123)
                  scores2 <- pca.res2@scores
                  
                  plotData <- data.frame(x=pca.res2@scores[,1],
                                         y=pca.res2@scores[,2],
                                         sample=names(x))
                  plotData <- merge(plotData,sampleList,by="sample",sort=FALSE)
                  plotData$class <- as.character(plotData$class)
                  plotData$class[is.na(plotData$class)] <- "QC" 
                  ggobj <-ggplot(data = plotData,aes(x=x,y=y,colour=class))+
                      geom_point()+
                      xlab("PC1")+
                      ylab("PC2")+
                      theme(legend.justification=c(1,1), legend.position=c(1,1))
                  print(ggobj)
                  ##HotE(scores[,2],scores[,3])))*out.tol
                  HotEllipse<-abs(cbind(HotE(scores2[,1],scores2[,2])))*out.tol 
                  outliers2<-as.numeric()
                  for (i in 1:nrow(scores2)){
                      
                      sample<-abs(scores2[i,])
                      out.PC1<-which(HotEllipse[,1]<sample[1])
                      out.PC1.PC2<-any(HotEllipse[out.PC1,2]<sample[2])*1
                      
                      outlier<-ifelse(out.PC1.PC2>0,1,0)#+out.PC1.PC3+out.PC2.PC3>0,1,0)
                      outliers2<-c(outliers2,outlier)
                  }
                  
                  plotPcs(pca.res2, type = "scores",col=outliers2+1,pch=19,cex=0.8,
                          main="2st PCA model")
                  
                  outliers2<-which(outliers2==1)
                  cat("Outliers were detected in the 2st PCA model",
                      "calculation and removed:",
                      names(x)[outliers2],"\n")
                  cat("Outliers were detected in the 2st PCA model",
                      "calculation and removed:",
                      names(x)[outliers2],"\n",file = outLog,append = TRUE)
                  
                  if(length(outliers2)>=1) {
                      
                      ## The 3st round
                      x <- x[,-outliers2]
                      pca.res3 <- pca(t(x), nPcs = 2, method=pcaMethod,scale=scale, 
                                      center=center, cv="q2",seed=123)
                      scores3 <- pca.res3@scores
                      
                      plotData <- data.frame(x=pca.res3@scores[,1],
                                             y=pca.res3@scores[,2],
                                             sample=names(x))
                      plotData <- merge(plotData,sampleList,by="sample",sort=FALSE)
                      plotData$class <- as.character(plotData$class)
                      plotData$class[is.na(plotData$class)] <- "QC" 
                      ggobj <-ggplot(data = plotData,aes(x=x,y=y,colour=class))+
                          geom_point()+
                          xlab("PC1")+
                          ylab("PC2")+
                          theme(legend.justification=c(1,1), legend.position=c(1,1))
                      print(ggobj)
                      
                      ##HotE(scores[,2],scores[,3])))*out.tol
                      HotEllipse<-abs(cbind(HotE(scores3[,1],scores3[,2])))*out.tol 
                      outliers3<-as.numeric()
                      for (i in 1:nrow(scores3)){
                          
                          sample<-abs(scores3[i,])
                          out.PC1<-which(HotEllipse[,1]<sample[1])
                          out.PC1.PC2<-any(HotEllipse[out.PC1,2]<sample[2])*1
                          
                          #+out.PC1.PC3+out.PC2.PC3>0,1,0)
                          outlier<-ifelse(out.PC1.PC2>0,1,0)
                          outliers3<-c(outliers3,outlier)
                      }
                      
                      plotPcs(pca.res3, type = "scores",col=outliers3+1,pch=19,cex=0.8,
                              main="3st PCA model")
                      
                      outliers3<-which(outliers3==1)
                      cat("Outliers were detected in the 3st PCA model", 
                          "calculation and removed:",
                          names(x)[outliers3],"\n")
                      cat("Outliers were detected in the 3st PCA model",
                          "calculation and removed:",
                          names(x)[outliers3],"\n",file = outLog,append = TRUE)
                      
                      if (length(outliers3)>=1){
                          message("WARNING: after two rounds of outlier removal",
                                  " outliers are still detected either:")
                          x <- x[,-outliers3]
                      }
                  }
              }
              
              
              dev.off()
              removeSampleNames <- allSampleNames[!allSampleNames %in% names(x)]
              return(removeSampleNames)    
              
              
          }
)

##' @title Export a csv file which can be used for MetaboAnalyst
##' @description Export a csv file which can be used for MetaboAnalyst
##' @rdname makeMetaboAnalystInput
##' @docType methods
##' @param para A metaXpara object
##' @param rmQC A logical indicates whether remove the QC data
##' @param valueID The name of the column which will be used
##' @param zero2NA A logical indicates whether convert the value <=0 to NA
##' @param prefix The prefix of output file
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' makeMetaboAnalystInput(para,valueID="value")
setGeneric("makeMetaboAnalystInput",
           function(para,rmQC=TRUE,valueID="valueNorm",zero2NA=TRUE,prefix=NA,
                    ...) 
               standardGeneric("makeMetaboAnalystInput"))
##' @describeIn makeMetaboAnalystInput
setMethod("makeMetaboAnalystInput", signature(para = "metaXpara"), 
          function(para,rmQC=TRUE,valueID="valueNorm",zero2NA=TRUE,prefix=NA,
                   ...){
              
              if(is.na(prefix)){
                  prefix <- para@prefix
              }else{
                  prefix <- paste(para@prefix,"-",prefix,sep="")
              }
              metaboAnalystInput <- paste(para@outdir,"/",prefix,
                                          "-metaboAnalystInput.csv",sep="")
              
              samList  <- read.delim(para@sampleListFile)
              if(zero2NA){
                  para <- zero2NA(para,valueID=valueID)
              }
              x<-dcast(para@peaksData,ID~sample,value.var = valueID)
              
              ## remove qc sample data
              if(rmQC==TRUE){
                  x <- x[,!names(x) %in% samList$sample[is.na(samList$class)]]
              }
              sample2class <- data.frame(sample=names(x)[-1])
              sample2class <- merge(sample2class,samList,by="sample",sort=FALSE)
              sample2class$class <- as.character(sample2class$class)
              sample2class$class[is.na(sample2class$class)] <- "QC"
              sample2class$Sample <- paste(sample2class$class,sample2class$order,sep="_")
              sample2class$Lable  <- as.character(sample2class$class)
              sample2class$Lable[is.na(sample2class$Lable)] <- "QC"
              sample2class <- t(sample2class[,c("Sample","Lable")])
              message("Save data to file for MetaboAnalyst input: ",metaboAnalystInput)
              write.table(sample2class, file = metaboAnalystInput,
                          col.names=FALSE,sep=",")
              write.table(x,file = metaboAnalystInput,row.names = FALSE,
                          col.names=FALSE,sep=",",append = TRUE)
          }
          
)


##' @title Do the univariate and multivariate statistical analysis
##' @description Do the univariate and multivariate statistical analysis
##' @rdname peakStat
##' @docType methods
##' @param para A \code{metaXpara} object
##' @param plsdaPara A \code{plsDAPara} object
##' @param doROC A logical indicates whether to calculate the ROC
##' @param saveRds Boolean, setting the argument to TRUE to save some objects to
##' disk for debug. Only useful for developer. Default is FALSE.
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @return An object of \code{metaXpara}
##' @examples
##' \dontrun{
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' ratioPairs(para) <- "S:C"
##' addValueNorm(para) <- para
##' plsdaPara <- new("plsDAPara")
##' res <- peakStat(para,plsdaPara)
##' }
setGeneric("peakStat",function(para,plsdaPara,doROC=TRUE,saveRds=FALSE,...) standardGeneric("peakStat"))
##' @describeIn peakStat
setMethod("peakStat", signature(para = "metaXpara",plsdaPara = "plsDAPara"), 
          function(para,plsdaPara,doROC=TRUE,saveRds=FALSE,...){
              
              ratioPairs <- para@ratioPairs #A:B;A:C;B:C
              ratioPairs <- unlist(strsplit(x = ratioPairs, split = ";"))
              if(length(ratioPairs)<1){
                  stop("Please check the parameter: ",ratioPairs)
              }
              
              ## output fig 
              outFig <- paste(para@outdir,"/",para@prefix,"-peakStat.pdf",sep="")
              pdf(file = outFig,width = 5,height = 5)
              
              assign(x="plsdaPara",value=plsdaPara,envir=.GlobalEnv)
              
              statResult <- lapply(ratioPairs,function(rp){
                  cgroup <- unlist(strsplit(x = rp, split = ":"))
                  peaksData <- para@peaksData[para@peaksData$class%in%cgroup,]
                  peaksData$class <- as.character(peaksData$class)
                  if(nrow(peaksData)==0){
                      stop("Please check your class ",rp)
                  }
                  
                  message(date(),"\tUnivariate analysis")
                  statTest <- ddply(peaksData,.(ID),here(summarise),
                                    ratio = calcRatio(x = valueNorm,
                                                      class = class,
                                                      method = "mean",
                                                      case = cgroup[1],
                                                      control = cgroup[2]),
                                    #t.test_p.value = t.test(value~class)$p.value,
                                    t.test_p.value = .tTest(value,class),
                                    wilcox.test_p.value = wilcox.test(value~class)$p.value)
                  
                  ## Adjust P-values for Multiple Comparisons
                  statTest$t.test_p.value_BHcorrect <- p.adjust(p=statTest$t.test_p.value,
                                                                method = "BH")
                  statTest$wilcox.test_p.value_BHcorrect <- 
                      p.adjust(p=statTest$wilcox.test_p.value,method = "BH")
                  
                  message(date(),"\tUnivariate analysis done")
                  
                  ## ROC
                  if(length(cgroup)==2 && doROC==TRUE){
                      message(date(),"\tclassical univariate ROC analysis")
                      rocRes <- myCalcAUROC(para = para,cgroup = cgroup)    
                      statTest <- merge(statTest,rocRes,by="ID") 
                      message(date(),"\tclassical univariate ROC analysis done!")
                  }
                  ## plot ratio fig
                  ## t.test with fdr correct
                  plotdata <- data.frame(x=statTest$ratio,
                                         y=statTest$t.test_p.value_BHcorrect)
                  ## maybe there are negative value
                  plotdata <- plotdata[ isValid(plotdata$x),]
                  plotdata$sig <- ( abs(log2(plotdata$x)) >= log2(1.5) ) & plotdata$y <= 0.05
                  ggobj <- ggplot(data=plotdata,aes(x=log2(x),y = -log10(y), colour=sig))+
                      geom_point(alpha=0.7)+
                      scale_colour_manual(values=c("#9999CC","red"))+
                      xlab("Log2(Fold change)")+
                      ylab("-Log10(P-value)")+
                      ggtitle(paste(rp," t.test with FDR correct"))+
                      theme(legend.position="none")
                  print(ggobj)
                  
                  ## t.test
                  plotdata <- data.frame(x=statTest$ratio,
                                         y=statTest$t.test_p.value)
                  ## maybe there are negative value
                  plotdata <- plotdata[ isValid(plotdata$x),]
                  plotdata$sig <- ( abs(log2(plotdata$x)) >= log2(1.5) ) & plotdata$y <= 0.05
                  ggobj <- ggplot(data=plotdata,aes(x=log2(x),y = -log10(y), colour=sig))+
                      geom_point(alpha=0.7)+
                      scale_colour_manual(values=c("#9999CC","red"))+
                      xlab("Log2(Fold change)")+
                      ylab("-Log10(P-value)")+
                      ggtitle(paste(rp," t.test"))+
                      theme(legend.position="none")
                  print(ggobj)
                  
                  ## histogram of log2(ratio)
                  ggobj <- ggplot(data=plotdata,aes(x=log2(x)))+
                      geom_histogram(aes(y = ..density..),colour="white",fill="#56B4E9")+
                      geom_density(colour="#E69F00")+
                      xlab("Log2(Fold change)")+
                      stat_function(fun=dnorm, colour="blue",
                                    args=list(mean=mean(log2(plotdata$x)),
                                              sd=sd(log2(plotdata$x))))+
                      geom_vline(xintercept=0,size=1.1,colour="#D55E00")
                  print(ggobj)
                  
                  ## wilcox.test with fdr correct
                  plotdata <- data.frame(x=statTest$ratio,
                                         y=statTest$wilcox.test_p.value_BHcorrect)
                  ## maybe there are negative value
                  plotdata <- plotdata[ isValid(plotdata$x),]
                  plotdata$sig <- ( abs(log2(plotdata$x)) >= log2(1.5) ) & plotdata$y <= 0.05
                  ggobj <- ggplot(data=plotdata,aes(x=log2(x),y = -log10(y), colour=sig))+
                      geom_point(alpha=0.7)+
                      #scale_colour_manual(values=c("gray","red"))+
                      scale_colour_manual(values=c("#9999CC","red"))+
                      xlab("Log2(Fold change)")+
                      ylab("-Log10(P-value)")+
                      ggtitle(paste(rp," wilcox.test with FDR correct"))+
                      theme(legend.position="none")
                  print(ggobj)
                  
                  ## wilcox.test
                  plotdata <- data.frame(x=statTest$ratio,
                                         y=statTest$wilcox.test_p.value)
                  ## maybe there are negative value
                  plotdata <- plotdata[ isValid(plotdata$x),]
                  plotdata$sig <- ( abs(log2(plotdata$x)) >= log2(1.5) ) & plotdata$y <= 0.05
                  ggobj <- ggplot(data=plotdata,aes(x=log2(x),y = -log10(y), colour=sig))+
                      geom_point(alpha=0.7)+
                      # scale_colour_manual(values=c("gray","red"))+
                      scale_colour_manual(values=c("#9999CC","red"))+
                      xlab("Log2(Fold change)")+
                      ylab("-Log10(P-value)")+
                      ggtitle(paste(rp," wilcox.test"))+
                      theme(legend.position="none")
                  print(ggobj)
                  
                  
                  if(plsdaPara@do==TRUE){
                      ## PLS-DA
                      message(date(),"\tPLS-DA analysis")
                      plsda.res <- runPLSDA(para = para,plsdaPara = plsdaPara,sample = cgroup)
                      ## save the PLS-DA model to file
                      if(saveRds){
                        sfile <- paste(para@outdir,"/",para@prefix,"-",
                                       paste(cgroup,collapse = "_"),"-plsDAmodel.rds",sep="")
                        saveRDS(object = plsda.res,file = sfile)
                      }
                      message(date(),"\tPLS-DA analysis done!")
                      
                      peaksVIP <- as.data.frame(plsda.res$VIP) # for plsDA
                      peaksVIP <- peaksVIP[,ncol(peaksVIP),drop=FALSE]
                      names(peaksVIP)[1] <- "VIP"
                      peaksVIP$ID <- row.names(peaksVIP)
                      statTest <- plyr::join(statTest,peaksVIP,by = "ID")
                  }
                  statTest$sample <- rp     
                  
                  ## The venn plot of significant metabolites defined by different methods
                  venn.fig <- paste(para@outdir,"/",para@prefix,"-",rp,
                                    "-sigMetaboliteVenn.tiff",sep="")
                  venn.fig <- gsub(pattern = ":",replacement = "VS",x=venn.fig)
                  ## maybe there are negative value
                  tmp <- statTest[ isValid(statTest$ratio),]
                  sigA <- tmp$ID[ abs(log2(tmp$ratio)) >= log2(1.5) & 
                                      tmp$t.test_p.value_BHcorrect <= 0.05 ]
                  sigB <- tmp$ID[ abs(log2(tmp$ratio)) >= log2(1.5) & 
                                      tmp$wilcox.test_p.value_BHcorrect <= 0.05]
                  if(plsdaPara@do==TRUE){
                      sigC <- tmp$ID[ tmp$VIP >=1 ]
                      venn.data <- list("t.test"=sigA,"wilcox.test"=sigB,"VIP"=sigC)
                      VennDiagram::venn.diagram(x = venn.data,
                                                filename = venn.fig,col = "transparent",
                                                fill = c("cornflowerblue", "green", "yellow"),
                                                alpha = 0.50,
                                                label.col = "black",
                                                cat.col=c("darkblue","darkgreen","darkgreen"))
                  }else{
                      venn.data <- list("t.test"=sigA,"wilcox.test"=sigB)
                      if(length(venn.data$t.test)!=0 || length(venn.data$wilcox.test)!=0){
                          VennDiagram::venn.diagram(x = venn.data,
                                                    filename = venn.fig,col = "transparent",
                                                    fill = c("cornflowerblue", "green"),
                                                    alpha = 0.50,
                                                    label.col = "black",
                                                    cat.col=c("darkblue","darkgreen"))
                      }
                  }
                  
                  ## return value
                  statTest
              })
              statResult <- do.call(rbind,statResult)
              
              ## if there are CV (RSD) information in peaksData, put it into statResult
              cvInd <- grep(pattern = "cv",x = names(para@peaksData),ignore.case = TRUE)
              if(length(cvInd)>=1){
                  cvInfo <-  dplyr::distinct(dplyr::select(para@peaksData,ID,
                                                           contains("cv", 
                                                                    ignore.case = TRUE))) 
                  statResult <- dplyr::left_join(statResult,cvInfo,by="ID")
              }
              
              
              para@quant <- statResult
              outFile <- paste(para@outdir,"/",para@prefix,"-quant.txt",
                               sep="")
              message("Save quantification result to file: ",outFile)
              write.table(para@quant,file = outFile,
                          col.names = TRUE,
                          row.names = FALSE,quote=FALSE,sep="\t")
              dev.off()
              return(para)
              
          })



##' @title Plot figures for QC-RLSC
##' @description Plot figures for QC-RLSC
##' @rdname plotQCRLSC
##' @docType methods
##' @param para A \code{metaXpara} object
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @seealso \code{\link{doQCRLSC}}
##' @exportMethod
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)[1:20,]
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' res <- doQCRLSC(para,cpu=1)
##' plotQCRLSC(res$metaXpara)
setGeneric("plotQCRLSC",function(para) standardGeneric("plotQCRLSC"))
##' @describeIn plotQCRLSC
setMethod("plotQCRLSC", signature(para = "metaXpara"), function(para){
    x <- para@peaksData
    fig <- paste(para@outdir,"/",para@prefix,"-QC-RLSC.png",sep="")
    highfig <- sub(pattern = "png$",replacement = "pdf",x = fig)
    pdf(file = highfig,width = 8,height = 6)
    
    x$isQC <- ifelse(is.na(x$class),"QC","Sample")
    fid <- unique(x$ID)
    
    #for(i in 1:length(fid)){
    maxn <- ifelse(length(fid)>=100,100,length(fid))
    for(i in 1:maxn){
        dat <- x[x$ID==fid[i],]
        
        y <- melt(dat,
                  measure.vars = c("value","valueNorm"),
                  value.name = "Intensity",
                  variable.name = "Method") 
        y$class <- as.character(y$class)
        y$class[is.na(y$class)] <- "QC"
        y$batch <- as.character(y$batch)
        y$Intensity <- log10(y$Intensity)
        ggObj <- ggplot(data=y,aes(x=order,y=Intensity,
                                   shape=batch,
                                   colour=class))+
            geom_point()+
            geom_line(data=y[y$class=="QC",],
                      aes(x=order,y=Intensity))+
            facet_grid(Method~.)+
            ggtitle(label = fid[i])+
            ylab("log10(Intensity)")
        #scale_y_log10()
        print(ggObj)
        
        if(i==1){
            png(filename = fig,width = 8,height = 6,units = "in",res = 150)
            print(ggObj)
            dev.off()
        }
        
    }
    dev.off()
    res <- list(fig=fig,highfig=highfig)
    return(res)
}
)




##' @title Plot PCA figure
##' @description Plot PCA figure
##' @rdname plotPCA
##' @docType methods
##' @param para A \code{metaXpara} object
##' @param pcaMethod See \code{\link{pca}} in \pkg{pcaMethods}
##' @param valueID The name of the column which will be used
##' @param scale Scaling, see \code{\link{pca}} in \pkg{pcaMethods}
##' @param center Centering, see \code{\link{pca}} in \pkg{pcaMethods}
##' @param label The label used for plot PCA figure, default is "order"
##' @param rmQC A logical indicates whether remove QC data
##' @param batch A logical indicates whether output batch information
##' @param saveRds Boolean, setting the argument to TRUE to save some objects to
##' disk for debug. Only useful for developer. Default is FALSE.
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' para <- transformation(para,valueID = "value")
##' metaX::plotPCA(para,valueID="value",scale="uv",center=TRUE)
setGeneric("plotPCA",function(para,pcaMethod="svdImpute",valueID="valueNorm",
                              label="order",rmQC=TRUE,batch=FALSE,
                              scale="none",center=FALSE,saveRds=FALSE,...) 
    standardGeneric("plotPCA"))
##' @describeIn plotPCA
setMethod("plotPCA", signature(para = "metaXpara"), 
          function(para,pcaMethod="svdImpute",valueID="valueNorm",
                   label="order",rmQC=TRUE,batch=FALSE,
                   scale="none",center=FALSE,saveRds=FALSE,...){
              
              message("plot PCA for value '",valueID,"'")
              
              prefix <- para@prefix
              outdir <- para@outdir
              
              fig <- paste(para@outdir,"/",para@prefix,"-PCA.png",sep="")
              highfig <- sub(pattern = "png$",replacement = "pdf",x = fig)
              
              pdf(file = highfig,width = 6,height = 6)
              
              sampleList  <- read.delim(para@sampleListFile)
              
              x <- para@peaksData
              x$class <- as.character(x$class)
              if(rmQC==TRUE){
                  x <- x[!is.na(x$class),]
              }else{
                  x$class[is.na(x$class)] <- "QC"
              }
              
              
              x<-dcast(x,ID~sample,value.var = valueID)
              x$ID <- NULL
              
              pca.res <- pca(t(x), nPcs = 3, method=pcaMethod,cv="q2",
                             scale=scale,center=center,seed=123,...)
              
              if(saveRds){
                saveRDS(object = pca.res,file = paste(para@outdir,"/",
                                                      para@prefix,"-pca.rds",sep=""))
              }
              plotData <- data.frame(x=pca.res@scores[,1],
                                     y=pca.res@scores[,2],
                                     z=pca.res@scores[,3],
                                     sample=names(x))
              plotData <- merge(plotData,sampleList,by="sample",sort=FALSE)
              plotData$class <- as.character(plotData$class)
              plotData$class[is.na(plotData$class)] <- "QC" 
              
              
              ggobj <-ggplot(data = plotData,aes(x=x,y=y,colour=class))+
                  geom_hline(yintercept=0,colour="white",size=1)+
                  geom_vline(xintercept=0,colour="white",size=1)+
                  #geom_point()+
                  xlab(paste("PC1"," (",sprintf("%.2f%%",100*pca.res@R2[1]),") ",sep=""))+
                  ylab(paste("PC2"," (",sprintf("%.2f%%",100*pca.res@R2[2]),") ",sep=""))+
                  #theme_bw()+
                  theme(#legend.justification=c(1,1), 
                      #legend.position=c(1,1),
                      legend.position="top",
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background=element_rect(fill="#E3E3EE"))+
                  #theme(legend.direction = 'horizontal', legend.position = 'top')+
                  stat_ellipse(geom = "polygon", type="euclid",alpha = 0.4, 
                               aes(fill = class))+
                  stat_ellipse(geom = "path")
              
              if(label == "order"){
                  ggobj <- ggobj + geom_text(aes(label=order),size=4,hjust=-0.2)
              }else if(label == "sample"){
                  ggobj <- ggobj + geom_text(aes(label=sample),size=4,hjust=-0.2)
              }
              if(batch==TRUE){
                  ggobj <- ggobj + geom_point(aes(shape=as.character(batch)))+
                      scale_shape_discrete(name  ="batch")
              }else{
                  ggobj <- ggobj + geom_point()
              }
              
              
              print(ggobj)
              
              ## 3D PCA plot
              #par(mgp=c(1.6,1,0))
              col <- as.numeric(as.factor(plotData$class))
              s3d <- scatterplot3d(plotData$x,plotData$y,plotData$z,type="h",angle = 24,
                                   col.grid="lightblue",lty.hplot=2,pch="",color="gray",
                                   xlab = paste("PC1"," (",
                                                sprintf("%.2f%%",100*pca.res@R2[1]),") ",sep=""),
                                   ylab = paste("PC2"," (",
                                                sprintf("%.2f%%",100*pca.res@R2[2]),") ",sep=""),
                                   zlab = paste("PC3"," (",
                                                sprintf("%.2f%%",100*pca.res@R2[3]),") ",sep="")
              )#color = as.numeric(as.factor(plotData$class)))
              s3d$points(plotData$x,plotData$y,plotData$z, pch = 1,col = col)
              s3d.coords <- s3d$xyz.convert(plotData$x,plotData$y,plotData$z)
              text(s3d.coords$x, s3d.coords$y, labels = plotData$order,
                   pos = 4,cex=0.5,col = col)
              
              
              
              classLabel <- levels(as.factor(plotData$class))
              legend(s3d$xyz.convert(max(plotData$x)*0.7, 
                                     max(plotData$y), min(plotData$z)), 
                     col=as.numeric(as.factor(classLabel)), yjust=0,pch=1,
                     legend = classLabel, cex = 0.8)
              
              dev.off()
              
              png(filename = fig,width = 6,height = 6,units = "in",res = 150)
              print(ggobj)
              dev.off()
              res <- list(fig=fig,highfig=highfig)
              return(res)
              
          }
)

##' @title Plot PLS-DA figure
##' @description Plot PLS-DA figure
##' @rdname plotPLSDA
##' @docType methods
##' @param para A \code{metaXpara} object
##' @param valueID The name of the column which will be used
##' @param label The label used for plot PLS-DA figure, default is "order"
##' @param ncomp The number of components used for PLS-DA
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' para <- transformation(para,valueID = "value")
##' para <- preProcess(para=para,scale = "uv",center = TRUE,valueID = "value")
##' plotPLSDA(para,valueID="value")
setGeneric("plotPLSDA",function(para,label="order",valueID="valueNorm",
                                ncomp=5,...) 
    standardGeneric("plotPLSDA"))
##' @describeIn plotPLSDA
setMethod("plotPLSDA", signature(para = "metaXpara"), 
          function(para,label="order",valueID="valueNorm",ncomp=5,...){
              
              prefix <- para@prefix
              outdir <- para@outdir
              
              fig <- paste(para@outdir,"/",para@prefix,"-PLSDA.pdf",sep="")
              pdf(file = fig,width = 6,height = 6)
              
              sampleList  <- read.delim(para@sampleListFile)
              
              peaksData <- para@peaksData
              peaksData <- peaksData[!is.na(peaksData$class),]
              peaksData$class <- as.character(peaksData$class)
              plsData <- dcast(peaksData,sample+class~ID,
                               value.var = valueID)
              #row.names(plsData) <- plsData$ID
              plsData$class[is.na(plsData$class)] <- "QC"
              pls.X <- as.matrix(plsData[,-c(1:2)])
              pls.Y <- as.factor(as.character(plsData$class))
              
              message("Remove NA value ...")
              numNA <- apply(pls.X,2,function(x){any(is.na(x))})
              message(sum(numNA)," features are removed!")
              pls.X <- pls.X[,!numNA]
              
              p <- plsDA(variables = pls.X,group = pls.Y,autosel = FALSE,comps=ncomp,...)
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
                                         sample=plsData$sample,
                                         class=pls.Y)
              }else{
                  plotData <- data.frame(x=p$components[,1],
                                         y=p$components[,2],
                                         sample=plsData$sample,
                                         class=pls.Y)
              }
              plotData$class <- as.character(plotData$class)
              plotData$class[is.na(plotData$class)] <- "QC" 
              sampleList$class <- NULL
              plotData <- merge(plotData,sampleList,by="sample",sort=FALSE)
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
                  stat_ellipse(geom = "path")
              
              if(label == "order"){
                  ggobj <- ggobj + geom_text(aes(label=order),size=4,hjust=-0.2)
              }else if(label == "sample"){
                  ggobj <- ggobj + geom_text(aes(label=sample),size=4,hjust=-0.2)
              }
              
              print(ggobj)
              
              
              ## 3D PCA plot
              #par(mgp=c(1.6,1,0))
              if(ncomp >=3){
                  col <- as.numeric(as.factor(plotData$class))
                  s3d <- scatterplot3d(plotData$x,plotData$y,plotData$z,type="h",angle = 24,
                                       col.grid="lightblue",lty.hplot=2,pch="",color="gray",
                                       xlab = paste("PC1"," (",sprintf("%.2f%%",100*p$R2[1,1]),") ",sep=""),
                                       ylab = paste("PC2"," (",sprintf("%.2f%%",100*p$R2[2,1]),") ",sep=""),
                                       zlab = paste("PC3"," (",sprintf("%.2f%%",100*p$R2[3,1]),") ",sep=""),
                  )#color = as.numeric(as.factor(plotData$class)))
                  s3d$points(plotData$x,plotData$y,plotData$z, pch = 1,col = col)
                  s3d.coords <- s3d$xyz.convert(plotData$x,plotData$y,plotData$z)
                  text(s3d.coords$x, s3d.coords$y, labels = plotData$order,
                       pos = 4,cex=0.5,col = col)
                  
                  
                  
                  classLabel <- levels(as.factor(plotData$class))
                  legend(s3d$xyz.convert(max(plotData$x)*0.7, 
                                         max(plotData$y), min(plotData$z)), 
                         col=as.numeric(as.factor(classLabel)), yjust=0,pch=1,
                         legend = classLabel, cex = 0.8)
              }
              dev.off()
              
              
          }
)




##' @title filterQCPeaks
##' @description filter peaks according to the QC sample
##' @rdname filterQCPeaks
##' @param para An object of metaXpara
##' @param ratio filter peaks which have missing value more than percent of 
##' "ratio", default is 0.5
##' @param omit.negative A logical value indicates whether omit the negative 
##' value
##' @param ... Additional parameters 
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' p <- filterQCPeaks(para,ratio=0.5)
setGeneric("filterQCPeaks",function(para,ratio=0.5,omit.negative=TRUE,...) 
    standardGeneric("filterQCPeaks"))
##' @describeIn filterQCPeaks
setMethod("filterQCPeaks", signature(para = "metaXpara"),
          function(para,ratio=0.5,omit.negative=TRUE,...){
              
              peaksData <- para@peaksData
              if(sum(is.na(peaksData$class)) <=0 ){
                  message("Don't find QC sample in your data")
                  return(para)
              }
              
              #assign(x="filterRatio",value=ratio,envir=.GlobalEnv)
              filterRatio <- ratio
              rpeak <- ddply(peaksData[is.na(peaksData$class),],.(ID),here(summarise),
                             rp=countMissingValue(x = value,ratio = filterRatio,omit.negative=TRUE))
              
              para@peaksData <- peaksData[ peaksData$ID %in% rpeak$ID[ !rpeak$rp],]
              
              message("Remove peaks which the percent is more than ", ratio ,
                  " with intensity are NA!")
              message(sum(rpeak$rp))
              
              ## save the remove peak to file
              if(sum(rpeak$rp) >=1){
                  fi <- paste(para@outdir,"/",para@prefix,"-filterQCPeaks",sep="")
                  message("Save the removed peaks to file: ",fi)
                  write.table(rpeak,file=fi,row.names = FALSE,col.names = TRUE,
                              quote=FALSE,sep="\t")
              }
              
              return(para)
              
          })


##' @title filterPeaks
##' @description filter peaks according to the non-QC sample
##' @rdname filterPeaks
##' @param para An object of metaXpara
##' @param ratio filter peaks which have missing value more than percent of 
##' "ratio", default is 0.8
##' @param omit.negative A logical value indicates whether omit the negative 
##' value
##' @param ... Additional parameters 
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' p <- filterPeaks(para,ratio=0.2)
setGeneric("filterPeaks",function(para,ratio=0.8,omit.negative=TRUE,...) standardGeneric("filterPeaks"))
##' @describeIn filterPeaks
setMethod("filterPeaks", signature(para = "metaXpara"),function(para,ratio=0.8,omit.negative=TRUE,...){
    peaksData <- para@peaksData
    #assign(x="filterRatio",value=ratio,envir=.GlobalEnv)
    filterRatio <- ratio
    rpeak <- ddply(peaksData[!is.na(peaksData$class),],.(ID),here(summarise),
                   rp=countMissingValue(value,ratio = filterRatio,omit.negative))
    
    if(sum(rpeak$rp)!=0){
        para@peaksData <- peaksData[ peaksData$ID %in% rpeak$ID[ !rpeak$rp],]
    }
    message("Remove peaks which the percent is more than ", ratio ,
        " with intensity are NA!")
    message(sum(rpeak$rp))
    
    ## save the remove peak to file
    if(sum(rpeak$rp) >=1){
        fi <- paste(para@outdir,"/",para@prefix,"-filterPeaks",sep="")
        message("Save the removed peaks to file: ",fi)
        write.table(rpeak,file=fi,row.names = FALSE,col.names = TRUE,
                    quote=FALSE,sep="\t")
    }
    
    return(para)
    
})

##' @title reSetPeaksData
##' @description reSetPeaksData
##' @rdname reSetPeaksData
##' @docType methods
##' @param para An object of \code{metaXpara}
##' @return none
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
setGeneric("reSetPeaksData",function(para) standardGeneric("reSetPeaksData"))
##' @describeIn reSetPeaksData
setMethod("reSetPeaksData", signature(para = "metaXpara"), function(para){
    
    rawPeaks <- para@rawPeaks
    samList  <- read.delim(para@sampleListFile)
    row.names(rawPeaks) <- rawPeaks$name
    rawPeaks <- rawPeaks[,names(rawPeaks) %in% samList$sample]  
    rawPeaks$ID <- row.names(rawPeaks)
    peaksData <- melt(rawPeaks,id.vars = "ID",variable.name = "sample")
    peaksData <- plyr::join(peaksData,samList,by="sample")
    para@peaksData <- peaksData
    
    return(para)
    
})




##' @title Using the QC samples to do the quality control-robust spline signal 
##' correction
##' @description Using the QC samples to do the quality control-robust spline 
##' signal correction.
##' @rdname doQCRLSC
##' @docType methods
##' @exportMethod
##' @param para An object of metaXpara
##' @param cvFilter The threshold of CV filter
##' @param impute A logical indicates whether impute the result
##' @param cpu The number of cpu used for processing
##' @param ... Additional parameters 
##' @return A list object
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @seealso \code{\link{plotQCRLSC}}
##' @details 
##' The smoothing parameter is optimised using leave-one-out cross validation 
##' to avoid overfitting. 
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)[1:20,]
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' res <- doQCRLSC(para,cpu=1)
setGeneric("doQCRLSC",function(para,cvFilter=0.3,impute=TRUE,cpu=0,...) 
    standardGeneric("doQCRLSC"))
##' @describeIn doQCRLSC
setMethod("doQCRLSC", signature(para = "metaXpara"), 
          function(para,cvFilter=0.3,impute=TRUE,cpu=0,...){
              
              ## output object 
              res <- list()           
              
              message("Using cpu: ",cpu)
              peaksData <- para@peaksData
              if(sum(is.na(peaksData$class)) <=0 ){
                  message("Don't find QC sample in your data!")
                  return(para)
              }
              
              message("The number of NA value in peaksData before QC-RLSC: ",
                  sum(is.na(peaksData$value) | peaksData$value <= 0))
              
              qcData <- peaksData[is.na(peaksData$class),]
              samList <- read.delim(para@sampleListFile,stringsAsFactors=FALSE)
              
              assign(x="maxOrder",value=max(samList[,para@sampleListHead['order']]),
                     envir=.GlobalEnv)
              loessDegree <- 2
              loessSpan <- para@qcRlscSpan
              
              message("QC-RLSC loess span= ",loessSpan)
              
              # require(doSMP) # will no longer work...
              cpu = ifelse(cpu==0,detectCores(),cpu)
              
              cl <- makeCluster(getOption("cl.cores", cpu))
              
              clusterExport(cl, c("myLoessFit","tuneSpline"),envir=environment())
              
              qcData$ID_batch <- paste(qcData$ID,qcData$batch,sep="_")
              
              message(date(),"\tsmooth fitting ...")
              
              if(loessSpan==0){
                  
                  intPredict <- parLapply(cl,unique(qcData$ID_batch),.runFit1,
                                          qcData=qcData,maxOrder=maxOrder)
                  #intPredict <- do.call(rbind,intPredict)
                  intPredict <- rbindlist(intPredict)
                  intPredict <- as.data.frame(intPredict)
              }else{
                  
                  intPredict <- parLapply(cl,unique(qcData$ID_batch),.runFit2,
                                          qcData=qcData,maxOrder=maxOrder)
                  intPredict <- rbindlist(intPredict)
                  intPredict <- as.data.frame(intPredict)
                  
              }
              stopCluster(cl)
              message(date(),"\tsmooth fitting done.")
              intPredict <- dplyr::rename(intPredict,order=newOrder)
              ## head: sample       ID     value batch class order valuePredict
              peaksData$valuePredict <- NULL
              peaksData$valueNorm <- NULL
              peaksData <- plyr::join(peaksData,intPredict,
                                      by=intersect(names(peaksData),names(intPredict)))
              
              mpa <- ddply(peaksData,.(ID),summarise,mpa=median(value,na.rm = TRUE))
              peaksData <- plyr::join(peaksData,mpa,
                                      by=intersect(names(peaksData),names(mpa)))
              peaksData <- dplyr::mutate(peaksData,valuePredict=valuePredict/mpa)
              peaksData$mpa <- NULL
              message("change value which ==0 to NA")
              peaksData$value[ peaksData$value<=0] <- NA
              
              peaksData$valueNorm <- peaksData$value/peaksData$valuePredict
              
              ######################################################################
              ######################################################################
              peaksData$valuePredict[ peaksData$valuePredict<=0] <- NA
              
              ## calculate CV using this value
              peaksData$valueNorm[ peaksData$valueNorm<=0] <- NA
              message("The number of NA value in peaksData after QC-RLSC:",
                  sum(is.na(peaksData$valueNorm)))
              ## TODO: Do we still need to do missing value imputation are QC-RLSC?
              if(impute){
                  message(date(),"\tDo missing value imputation after QC-RLSC...")
                  paraTmp <- para
                  paraTmp@peaksData <- peaksData
                  paraTmp <- missingValueImpute(x = paraTmp,valueID="valueNorm")
                  peaksData <- paraTmp@peaksData
              }
              ## For each batch
              ## CV plot
              cvStat <- ddply(peaksData[is.na(peaksData$class),],.(ID,batch),
                              summarise,
                              rawCV=sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),
                              normCV=sd(valueNorm,na.rm = TRUE)/mean(valueNorm,na.rm = TRUE))
              
              
              cvStatForEachBatch <- melt(cvStat,id.vars = c("ID","batch"),
                                         variable.name = "CV")
              cvStatForEachBatch$batch <- as.factor(cvStatForEachBatch$batch)
              
              ## output information
              message("Summary information of the CV for QC samples:")
              cvTable <- ddply(cvStatForEachBatch,.(batch,CV),summarise,
                               lessThan30=sum(value<=0.3,na.rm = TRUE),
                               total=length(value),ratio=lessThan30/total)
              print(cvTable)
              res$cvBatch <- cvTable
              message("\n")
              para@fig$cv<- paste(para@outdir,"/",para@prefix,"-cv.pdf",sep="") 
              pdf(para@fig$cv,width = 6,height = 6)
              p<-ggplot(data=cvStatForEachBatch,aes(x=value,fill=CV,colour=CV))+
                  facet_grid(batch~.)+
                  geom_density(alpha = 0.5)+
                  xlab(label = "CV")
              print(p)
              p<-ggplot(data=cvStatForEachBatch,aes(x=value,fill=CV,colour=CV))+
                  facet_grid(batch~.)+
                  geom_density(alpha = 0.5)+
                  xlim(0,2)+
                  xlab(label = "CV")
              print(p)
              dev.off()
              
              #######################################
              ## For all
              cvStat <- ddply(peaksData[is.na(peaksData$class),],.(ID),
                              summarise,
                              rawCV=sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),
                              normCV=sd(valueNorm,na.rm = TRUE)/mean(valueNorm,na.rm = TRUE))
              
              
              cvStatForAll <- melt(cvStat,id.vars = c("ID"),
                                   variable.name = "CV")
              ## output information
              message("Summary information of the CV for QC samples:")
              cvTable <- ddply(cvStatForAll,.(CV),summarise,
                               lessThan30=sum(value<=0.3,na.rm = TRUE),
                               total=length(value),ratio=lessThan30/total)
              print(cvTable)
              res$cvAll <- cvTable
              message("\n")
              
              ########################################################
              message("Filter peaks with CV > ",cvFilter,"!")
              message(sum(cvStat$normCV > cvFilter,na.rm = TRUE))
              tmpPeaksData <- merge(peaksData,cvStat,by="ID")
              if(nrow(tmpPeaksData)!=nrow(peaksData)){
                  error_file <- paste(para@outdir,"/",para@prefix,"-doQCRLSC-error.rda",
                                      sep="")
                  message("Please see the file: ",error_file," for detail!")
                  save(peaksData,cvStat,file=error_file)
                  stop("Please see detailed data in ",error_file)
              }
              peaksData <- tmpPeaksData
              peaksData <- dplyr::rename(peaksData,cv=normCV)
              para@peaksData <- dplyr::filter(peaksData,!is.na(cv) & cv<=cvFilter)
              res$metaXpara <- para
              return(res)
              
          })

.runFit1=function(id,qcData,maxOrder){
    out <- tryCatch({
        
        dat <- data.frame(newOrder=1:maxOrder)
        
        piece <- qcData[ qcData$ID_batch==id,]
        
        dat$valuePredict=myLoessFit(piece$order,piece$value,dat$newOrder)
        
        dat$ID <- piece$ID[1]
        dat$batch <- piece$batch[1]
        dat
        
    },
    error=function(e){
        message("Please see the file: runFit_error.rda for related data!")
        save(e,id,qcData,maxOrder,file="runFit_error.rda")
        stop("error in runFit!")
        return(NULL)
    },
    warning=function(cond){
        message("Please see the file: runFit_warning.rda for related data!")
        save(cond,id,qcData,maxOrder,file="runFit_warning.rda")
        return(NULL)
    })
    return(out)
    
}

.runFit2=function(id,qcData,maxOrder){
    out <- tryCatch({
        dat <- data.frame(newOrder=1:maxOrder)
        
        piece <- qcData[ qcData$ID_batch==id,]
        dat$valuePredict=predict(smooth.spline(piece$order,
                                               piece$value,
                                               spar = loessSpan),
                                 dat$newOrder)$y
        dat$ID <- piece$ID[1]
        dat$batch <- piece$batch[1]
        dat
    },
    error=function(e){
        message("Please see the file: runFit_error.rda for related data!")
        save(e,id,qcData,maxOrder,file="runFit_error.rda")
        stop("error in runFit!")
        return(NULL)
    },
    warning=function(cond){
        message("Please see the file: runFit_warning.rda for related data!")
        save(cond,id,qcData,maxOrder,file="runFit_warning.rda")
        return(NULL)
    })
    return(out)
    
}

##' @title Missing value imputation
##' @description Missing value imputation
##' @rdname missingValueImpute
##' @docType methods
##' @exportMethod
##' @param x The value needed to be imputated
##' @param valueID The name of the column which will be used
##' @param method Method for imputation: bpca,knn,svdImpute,rf,min
##' @param negValue A logical indicates whether convert <=0 value to NA
##' @param cpu The number of cpus used
##' @param ... Additional parameters 
##' @return The imputation data
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
setGeneric("missingValueImpute",function(x,valueID="value",method="knn",
                                         negValue = TRUE,cpu=1,...) 
    standardGeneric("missingValueImpute"))
##' @describeIn missingValueImpute
setMethod("missingValueImpute", signature(x = "metaXpara"), 
          function(x,valueID="value",...){
              
              message("missingValueImpute: ",valueID)
              
              para <- x
              peaksData <- para@peaksData
              message(date(),"\tMissing value imputation for \'",valueID,"\'")
              x<-dcast(peaksData,ID~sample,value.var = valueID)
              row.names(x) <- x$ID
              x$ID <- NULL
              x[x<=0] <- NA
              
              message("Missing value in total: ",sum(is.na(x)))
              if(any(is.na(peaksData$class))){
                  qcValue <- peaksData[,valueID][is.na(peaksData$class)]
                  message("Missing value in QC sample: ",
                      sum(qcValue<=0 | is.na(qcValue)))
                  sValue <- peaksData[,valueID][!is.na(peaksData$class)]
                  message("Missing value in non-QC sample: ",
                      sum(sValue<=0 | is.na(sValue)))
              }
              x <- missingValueImpute(x,method=para@missValueImputeMethod,...)
              message("Missing value in total after missing value inputation: ",
                  sum(is.na(x)))
              
              ## maybe x has value which <=0
              message("<=0 value in total after missing value inputation: ",
                  sum(x<=0))
              x$ID <- row.names(x)
              y <- melt(x,id.vars = "ID",variable.name = "sample",value.name = "newValue")
              m <- plyr::join(peaksData,y,by=c("ID","sample"))
              m[,valueID] <- m$newValue
              m$newValue <- NULL
              para@peaksData <- m
              return(para)
              
          })


##' @describeIn missingValueImpute
setMethod("missingValueImpute", signature(x = "data.frame"), 
          function(x,method="knn",negValue = TRUE,cpu=1,...){
              inputedData <- NULL
              colName <- names(x)
              rowName <- row.names(x)
              x <- as.matrix(t(x))
              message(date(),"\tThe ratio of missing value: ",
                  sprintf("%.4f%%",100*sum(is.na(x))/length(x)))
              if(method == "bpca"){
                  ## Numerical matrix with (or an object coercible to such) with samples 
                  ## in rows and variables as columns. Also takes ExpressionSet in which 
                  ## case the transposed expression matrix is used. Can also be a data 
                  ## frame in which case all numberic variables are used to fit the PCA.
                  ## Please note that this method is very time-consuming.
                  mvd <- pca(x, nPcs = 3, method = "bpca")
                  inputedData <- completeObs(mvd)    
              }else if(method == "svdImpute"){
                  ## This method is very fast.
                  mvd <- pca(x, nPcs = 3, method = "svdImpute")
                  inputedData <- completeObs(mvd)
              }else if(method == "knn"){
                  ## An expression matrix with genes in the rows, samples in the columns
                  ## This method is very fast.
                  mvd <- impute.knn(t(x))
                  inputedData <- t(mvd$data)
                  
              }else if(method == "rf"){
                  ## A data matrix with missing values. 
                  ## The columns correspond to the variables and the rows to the 
                  ## observations.
                  ## Please note that this method is very time-consuming.
                  
                  if(cpu>1){
                      cl <- makeCluster(cpu)
                      registerDoParallel(cl)
                      mvd <- missForest(xmis = x,parallelize = "variables")
                      print(mvd$OOBerror)
                      inputedData <- mvd$ximp
                      stopCluster(cl)
                      
                      
                  }else{
                      mvd <- missForest(xmis = x)
                      print(mvd$OOBerror)
                      inputedData <- mvd$ximp
                  }
                  
              }else if(method == "min"){
                  inputedData <- apply(x,1,function(y){
                      y[is.na(y) | y<=0] <- min(y[y>0],na.rm = TRUE)
                      y})    
                  inputedData <- t(inputedData)
              }else{
                  stop("Please provide valid method for missing value inputation!")
              }
              
              if(negValue & method != "min"){
                  message("<=0: ",sum(inputedData<=0))
                  x <- inputedData 
                  inputedData <- apply(x,1,function(y){
                      y[is.na(y) | y<=0] <- min(y[y>0],na.rm = TRUE)
                      y})    
                  inputedData <- t(inputedData)
                  
              }
              
              inputedData <- as.data.frame(t(inputedData))
              row.names(inputedData) <- rowName
              names(inputedData) <- colName
              return(inputedData)    
          })


##' @title Judge whether the data has QC samples
##' @description Judge whether the data has QC samples
##' @rdname hasQC
##' @docType methods
##' @param para An object of data
##' @param ... Additional parameters 
##' @return A logical value
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' hasQC(para)
setGeneric("hasQC",function(para,...) standardGeneric("hasQC"))
##' @describeIn hasQC
setMethod("hasQC", signature(para = "metaXpara"), function(para,...){
    samList <- read.delim(para@sampleListFile)
    qc <- is.na(samList$class)
    if(any(qc)){
        message("Find QC samples in your data!")
        message("There are ",sum(qc)," QC samples in your data!")
        return(TRUE)
    }else{
        message("Don't find QC samples in your data!")
        return(FALSE)
    }
})


##' @title Convert the value <=0 to NA
##' @description Convert the value <=0 to NA
##' @rdname zero2NA
##' @docType methods
##' @param x An object of data
##' @param valueID The name of the column which will be used
##' @param ... Additional parameters 
##' @return An object of \code{metaXpara}
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' \donttest{
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- zero2NA(para)
##' }
setGeneric("zero2NA",function(x,valueID="value",...) standardGeneric("zero2NA"))
##' @describeIn zero2NA
setMethod("zero2NA", signature(x = "metaXpara"), function(x,valueID="value",...){
    dat <- x@peaksData[,valueID]
    message(date(),"\tzero2NA:",sum(dat<=0))
    dat[dat<=0] <- NA
    x@peaksData[,valueID] <- dat
    return(x)
    
})

##' @title Plot the distribution of the peaks intensity
##' @description  Plot the distribution of the peaks intensity for both raw 
##' intensity and normalized intensity.
##' @rdname plotIntDistr
##' @docType methods
##' @param x A metaXpara object.
##' @param width The width of pdf, default is 14.
##' @param ... Additional parameter
##' @return The figure name
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' para <- reSetPeaksData(para)
##' plotIntDistr(para)
##' ## after normalization
##' para <- metaX::normalize(para)
##' plotIntDistr(para)
setGeneric("plotIntDistr",function(x,width=14,...) standardGeneric("plotIntDistr"))
##' @describeIn plotIntDistr
setMethod("plotIntDistr", signature(x = "metaXpara"), function(x,width=14,...){
    dat <- x@peaksData
    fig <- paste(x@outdir,"/",x@prefix,"-intensityBoxplot.png",sep="")
    highfig <- sub(pattern = "png$",replacement = "pdf",x = fig)
    
    pdf(file = highfig,width = width,height = 5)
    dat$value[dat$value<=0] <- NA
    
    dat$class <- as.character(dat$class)
    dat$class[is.na(dat$class)] <- "QC"
    
    
    dat$value <- log2(dat$value)
    
    
    dat$order <- factor(dat$order,levels = sort(unique(dat$order)))
    dat$batch <- as.character(dat$batch)
    ggobj1 <- ggplot(data=dat,aes(x=order,y=value,fill=class))+
        geom_boxplot(outlier.size=0.4)+
        scale_x_discrete(labels = "")+
        ggtitle("Raw intensity")+
        ylab("log2(Intensity)")
    print(ggobj1)
    
    ##
    if("valueNorm" %in% names(dat)){
        dat$valueNorm[dat$valueNorm<=0] <- NA
        dat$valueNorm <- log2(dat$valueNorm)
        ggobj <- ggplot(data=dat,aes(x=order,y=valueNorm,fill=class))+
            geom_boxplot(outlier.size=0.4)+
            scale_x_discrete(labels = "")+
            ggtitle("Normalized intensity")+
            ylab("log2(Intensity)")
        print(ggobj)
    }
    dev.off()
    
    png(filename = fig,width = width,height = 5,units = "in",res=150)
    print(ggobj1)
    dev.off()
    res <- list(fig=fig,highfig=highfig)
    return(res)
    
    
})

##' @title Plot the distribution of the peaks S/N
##' @description  Plot the distribution of the peaks S/N, only suitable for 
##' XCMS result. This function generates a figure.
##' @rdname plotPeakSN
##' @docType methods
##' @param x A metaXpara object
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' para <- new("metaXpara")
##' xcmsSetObj(para) <- xset
##' plotPeakSN(para)
setGeneric("plotPeakSN",function(x,...) standardGeneric("plotPeakSN"))
##' @describeIn plotPeakSN
setMethod("plotPeakSN", signature(x = "metaXpara"), function(x,...){
    dat <- as.data.frame(x@xcmsSetObj@peaks)
    if(!"sn" %in% names(dat)){
        warning("Don't find column named 'sn' in xcmsSet object!")
        return(NULL)
    }
    fig <- paste(x@outdir,"/",x@prefix,"-peakSN.pdf",sep="")
    
    pdf(file = fig,width = 8,height = 5)
    
    ## sn distribution
    snStat <- table(cut(dat$sn,breaks = c(0:10,20,30,40,50,Inf)))
    snStat <- as.data.frame(snStat)
    names(snStat) <- c("SN","Freq")
    snStat$Ratio <- snStat$Freq/sum(snStat$Freq)
    ggobj <- ggplot(data=snStat,aes(x=SN,y=Ratio))+
        geom_bar(stat="identity",colour="white",fill="#56B4E9")+
        geom_text(aes(label=sprintf("%.3f%%",100*Ratio)),
                  vjust=-0.2,size=3,angle=60)
    print(ggobj)
    dev.off()
}
)



##' @title Plot the distribution of the peaks number
##' @description  Plot the distribution of the raw peaks number without 
##' post-processing. This function not only generates a figure, but also 
##' saves the information of peaks number into a file.
##' @rdname plotPeakNumber
##' @docType methods
##' @param x A metaXpara object
##' @param ... Additional parameter
##' @return The figure name
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' plotPeakNumber(para)
setGeneric("plotPeakNumber",function(x,...) standardGeneric("plotPeakNumber"))
##' @describeIn plotPeakNumber
setMethod("plotPeakNumber", signature(x = "metaXpara"), function(x,...){
    
    ## peak number 
    
    #nsample <- length(x@xcmsSetObj@filepaths)
    
    samList <- read.delim(x@sampleListFile)
    peaksData <- x@rawPeaks[, names(x@rawPeaks) %in% samList$sample]
    dat <- apply(peaksData,2,function(a){sum(a!=0 & !is.na(a))})
    dat <- as.data.frame(dat)
    names(dat) <- "npeaks"
    dat$sample <- row.names(dat)
    dat <- merge(dat,samList,by="sample")
    #dat$order <- factor(dat$order,levels = sort(unique(dat$order)))
    dat$batch <- as.character(dat$batch)
    dat$class <- as.character(dat$class)
    dat$class[is.na(dat$class)] <- "QC"
    dat$missPeaksN <- nrow(peaksData) - dat$npeaks
    
    ## define outlier 
    outliers.coef <- 1.5
    qs <- quantile(dat$npeaks,c(.25,.75),na.rm=TRUE)
    iqr <- qs[2] - qs[1]
    dat$outlier <- dat$npeaks < qs[1]-outliers.coef*iqr | 
        dat$npeaks > qs[2]+outliers.coef*iqr
    
    
    fig <- paste(x@outdir,"/",x@prefix,"-peakNumber.png",sep="")
    highfig <- sub(pattern = "png$",replacement = "pdf",x = fig)
    
    pdf(file = highfig,width = 8,height = 5)
    
    ggobj1 <- ggplot(data=dat,aes(x=order,y=npeaks,colour=class,shape=batch))+
        geom_point()+
        geom_text(aes(label=ifelse(outlier,order,"")),hjust=-0.2,size=4)+
        ylab("Peaks number")
    print(ggobj1)
    ggobj2 <- ggplot(data=dat,aes(x=order,y=missPeaksN,colour=class,shape=batch))+
        geom_point()+
        geom_text(aes(label=ifelse(outlier,order,"")),hjust=-0.2,size=4)+
        ylab("Missing peaks number")
    print(ggobj2)
    dev.off()
    
    png(filename = fig,width = 8,height = 5,units = "in",res=150)
    print(ggobj1)
    dev.off()
    # save the peaks number data to file
    peaksNumberFile <- paste(x@outdir,"/",x@prefix,"-peakNumber.txt",sep="")
    message("Save the peaks number information to file: ",peaksNumberFile)
    write.table(x = dat,file = peaksNumberFile,quote=FALSE,sep="\t",
                row.names=FALSE,col.names = TRUE)
    
    res <- list(fig=fig,highfig=highfig)
    return(res)
}
)


##' @title Plot the CV distribution of peaks in each group 
##' @description  Plot the CV distribution of peaks in each group.
##' @rdname plotCV
##' @docType methods
##' @param x A metaXpara object
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' para <- reSetPeaksData(para)
##' plotCV(para)
setGeneric("plotCV",function(x,...) standardGeneric("plotCV"))
##' @describeIn plotCV
setMethod("plotCV", signature(x = "metaXpara"), function(x,...){
    
    
    fig <- paste(x@outdir,"/",x@prefix,"-peakCV.png",sep="")
    highfig <- sub(pattern = "png$",replacement = "pdf",x = fig)
    
    pdf(file = highfig,width = 6,height = 5)
    
    peaksData <- x@peaksData
    peaksData$class <- as.character(peaksData$class) 
    peaksData$class[is.na(peaksData$class)] <- "QC"
    peaksData$value[peaksData$value<=0] <- NA
    cvTmp <- ddply(peaksData,.(ID,class),summarise,
                   cv=sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE))
    
    cvDat <- data.frame()
    for(i in unique(cvTmp$class)){
        tmp <- cvTmp[ cvTmp$class == i,]
        tmp <- tmp[!is.na(tmp$cv),]
        tmp <- tmp[order(tmp$cv),]
        tmp$n <- (1:nrow(tmp))/nrow(tmp)
        cvDat <- rbind(cvDat,tmp)
    }
    message("CV distribution (value):")
    cvOut <- ddply(cvDat,.(class),summarise,
                   n=length(cv),
                   n30=sum(cv<=0.3,na.rm = TRUE),
                   n20=sum(cv<=0.2,na.rm = TRUE),
                   n30ratio=n30/length(cv),
                   n20ratio=n20/length(cv))
    print(cvOut)
    
    ## whole range
    ggobj1 <- ggplot(data=cvDat,aes(x=cv,y=n,colour=class))+
        geom_line()+
        geom_vline(xintercept=c(0.2,0.3),linetype=2)+
        ylab("Percent of peaks")+
        ggtitle("The CV distribution of the intensity")+
        xlab("CV")
    print(ggobj1)
    ## only show cv between 0 and 1
    ggobj2 <- ggplot(data=cvDat,aes(x=cv,y=n,colour=class))+
        geom_line()+
        geom_vline(xintercept=c(0.2,0.3),linetype=2)+
        ylab("Percent of peaks")+
        xlab("CV")+
        ggtitle("The CV distribution of the intensity")+
        coord_cartesian(xlim=c(-0.05,1.05))
    print(ggobj2)
    
    ## for valueNorm
    if("valueNorm" %in% names(peaksData)){
        peaksData$valueNorm[peaksData$valueNorm<=0] <- NA
        cvTmp <- ddply(peaksData,.(ID,class),summarise,
                       cv=sd(valueNorm,na.rm = TRUE)/mean(valueNorm,na.rm = TRUE))
        
        cvDat <- data.frame()
        for(i in unique(cvTmp$class)){
            tmp <- cvTmp[ cvTmp$class == i,]
            tmp <- tmp[!is.na(tmp$cv),]
            tmp <- tmp[order(tmp$cv),]
            tmp$n <- (1:nrow(tmp))/nrow(tmp)
            cvDat <- rbind(cvDat,tmp)
        }
        
        message("CV distribution (valueNorm):")
        cvOut <- ddply(cvDat,.(class),summarise,
                       n30=sum(cv<=0.3,na.rm = TRUE),
                       n20=sum(cv<=0.2,na.rm = TRUE),
                       n30ratio=n30/length(cv),
                       n20ratio=n20/length(cv))
        print(cvOut)
        ## whole range
        ggobj <- ggplot(data=cvDat,aes(x=cv,y=n,colour=class))+
            geom_line()+
            geom_vline(xintercept=c(0.2,0.3),linetype=2)+
            ylab("Percent of peaks")+
            xlab("CV")+
            ggtitle("The CV distribution of the normalized intensity")
        print(ggobj)
        ## only show cv between 0 and 1
        ggobj <- ggplot(data=cvDat,aes(x=cv,y=n,colour=class))+
            geom_line()+
            geom_vline(xintercept=c(0.2,0.3),linetype=2)+
            ylab("Percent of peaks")+
            xlab("CV")+
            ggtitle("The CV distribution of the normalized intensity")+
            coord_cartesian(xlim=c(-0.05,1.05))
        print(ggobj)
        
    }
    
    dev.off()
    
    png(filename = fig,width = 6,height = 5,units = "in",res=150)
    print(ggobj2)
    dev.off()
    res <- list(fig=fig,highfig=highfig,stat=cvOut)
    return(res)
}
)


##' @title Plot Phylogenies for samples 
##' @description  This function plots phylogenetic trees for samples.
##' @rdname plotTreeMap
##' @docType methods
##' @param para A metaXpara object
##' @param valueID The name of the column that used for plot
##' @param log A logical indicating whether to log the data
##' @param rmQC A logical indicating whether to remove the QC samples
##' @param nc The number of clusters
##' @param treeType A character string specifying the type of phylogeny to be 
##' drawn; it must be one of "phylogram" (the default), "cladogram", "fan", 
##' "unrooted", "radial" or any unambiguous abbreviation of these.
##' @param width The width and height of the graphics region in inches. 
##' The default values are 8.
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' para <- reSetPeaksData(para)
##' plotTreeMap(para,valueID="value")
setGeneric("plotTreeMap",function(para,valueID="valueNorm",log=TRUE,rmQC=TRUE,
                                  nc=8,treeType="fan",width=8,...) 
    standardGeneric("plotTreeMap"))
##' @describeIn plotTreeMap
setMethod("plotTreeMap", signature(para = "metaXpara"), 
          function(para,valueID="valueNorm",log=TRUE,rmQC=TRUE,nc=8,
                   treeType="fan",width=8,...){
              
              fig <- paste(para@outdir,"/",para@prefix,"-TreeMap.pdf",sep="")
              
              pdf(file = fig,width = width,height = width)
              
              peaksData <- para@peaksData
              peaksData$class <- as.character(peaksData$class) 
              if(rmQC){
                  peaksData <- peaksData[ !is.na(peaksData$class),]
              }else{
                  peaksData$class[is.na(peaksData$class)] <- "QC"
              }
              peaksData[,valueID][ peaksData[,valueID]<=0] <- NA
              
              x<-dcast(peaksData,ID~sample,value.var = valueID)
              row.names(x) <- x$ID
              x$ID <- NULL
              x <- as.data.frame(t(x))
              x$sample <- row.names(x)
              samList <- read.delim(para@sampleListFile)
              samList$class <- as.character(samList$class) 
              samList$class[is.na(samList$class)] <- "QC"
              dat <- merge(x,samList,by="sample")
              if(nrow(dat) != nrow(x)){
                  warning("The nrow of the dat is ",nrow(dat),
                          ", the nrow of x is ",nrow(x),"\n")
              }
              row.names(dat) <- paste(dat$class,dat$order,sep="-")
              dat <- dat[ ,!names(dat) %in% names(samList)]
              if(log==TRUE){
                  dat <- log2(dat)
              }
              
              if(is.na(nc) | is.null(nc)){
                  n <- length(unique(samList$class))
              }else{
                  n <- nc
              }
              nclus <- n# number of clusters
              color <- 1:n# a vector of colors
              color_list=rep(color,nclus/length(color))
              cfit <- hclust(dist(dat))   
              clus=cutree(cfit,nclus)
              plot(as.phylo(cfit),type=treeType,label.offset=0.1,show.node.label=TRUE,
                   tip.color=color_list[clus],no.margin=TRUE)
              dev.off()
          }
)



##' @title Plot the correlation change of the QC samples.
##' @description  Plot the correlation change of the QC samples.
##' @rdname plotQC
##' @docType methods
##' @param para A metaXpara object
##' @param valueID The name of the column that used for plot
##' @param log A logical indicating whether to log the data
##' @param step The step value of calculate the cor of the samples. Default is 4.
##' @param width The width of the graphics region in inches. 
##' The default values are 8.
##' @param height The height of the graphics region in inches. 
##' The default values are 4.
##' @param ... Additional parameter
##' @return none
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' plotQC(para,valueID="value")
setGeneric("plotQC",function(para,valueID="valueNorm",step=4,log=TRUE,
                             width=8,height=4,...) 
    standardGeneric("plotQC"))
##' @describeIn plotQC
setMethod("plotQC", signature(para = "metaXpara"), 
          function(para,valueID="valueNorm",step=4,log=TRUE,
                   width=8,height=4,...){
              
              if(!any(is.na(para@peaksData$class))){
                  warning("Don't find QC sample in your data!")
                  return(NULL)
              }
              
              fig <- paste(para@outdir,"/",para@prefix,"-plotQC.pdf",sep="")
              
              pdf(file = fig,width = width,height = height)
              
              peaksData <- para@peaksData
              peaksData <- peaksData[is.na(peaksData$class),]
              peaksData$class <- as.character(peaksData$class) 
              peaksData$class[is.na(peaksData$class)] <- "QC"
              
              peaksData[,valueID][ peaksData[,valueID]<=0] <- NA
              if(log==TRUE){
                  peaksData[,valueID] <- log2(peaksData[,valueID])
              }
              peaksData <- peaksData[order(peaksData$order),]
              
              ## must sort
              x<-dcast(peaksData,ID~order,value.var = valueID)
              row.names(x) <- x$ID
              x$ID <- NULL
              orderName <- names(x)
              corDF <- data.frame()
              for(i in 1:(length(orderName)-step)){
                  valueCor <- cor(x[,i:(i+step)],use = "com")
                  valueCor <- data.frame(cor=valueCor[lower.tri(valueCor)],order=orderName[i])
                  corDF <- rbind(corDF,valueCor)
              }
              corDF$order <- as.numeric(as.character(corDF$order))
              corDF$order <- factor(corDF$order,levels=sort(unique(corDF$order)))
              ggobj <- ggplot(data=corDF,aes(x=order,y=cor))+
                  #geom_boxplot(outlier.size=0.4)+
                  geom_boxplot()+
                  theme(axis.text.x=element_text(angle=-90,vjust = 0.1))+
                  #ggtitle("Raw intensity")+
                  ylab("Correlation")
              print(ggobj)
              dev.off()
          }
)



##' @title Plot heatmap
##' @description  This function plots heatmap.
##' @rdname plotHeatMap
##' @docType methods
##' @param para A metaXpara object
##' @param valueID The name of the column that used for plot
##' @param log A logical indicating whether to log the data
##' @param rmQC A logical indicating whether to remove the QC samples
##' @param zero2na A logical indicating whether to convert the value <=0 to NA
##' @param colors Color for heatmap
##' @param width The width of the graphics region in inches. 
##' The default values are 12.
##' @param height The height of the graphics region in inches. 
##' The default values are 8.
##' @param saveRds Boolean, setting the argument to TRUE to save some objects to
##' disk for debug. Only useful for developer. Default is FALSE.
##' @param ... Additional parameter
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @return none
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' para <- reSetPeaksData(para)
##' para <- missingValueImpute(para)
##' plotHeatMap(para,valueID="value",width=6)
setGeneric("plotHeatMap",function(para,valueID="valueNorm",log=TRUE,rmQC=TRUE,
                                  zero2na=FALSE,colors="none",
                                  width=12,height=8,saveRds=FALSE,...) 
    standardGeneric("plotHeatMap"))
##' @describeIn plotHeatMap
setMethod("plotHeatMap", signature(para = "metaXpara"), 
          function(para,valueID="valueNorm",log=TRUE,rmQC=TRUE,
                   zero2na=FALSE,colors="none",width=12,height=8,
                   saveRds=FALSE,...){
              
              message("plot heatmap for ",valueID)
              
              peaksData <- para@peaksData
              peaksData$class <- as.character(peaksData$class) 
              if(rmQC){
                  peaksData <- peaksData[ !is.na(peaksData$class),]
              }else{
                  peaksData$class[is.na(peaksData$class)] <- "QC"
              }
              if(zero2na==TRUE){
                  peaksData[,valueID][ peaksData[,valueID]<=0] <- NA
              }
              
              x<-dcast(peaksData,class+order+sample~ID,value.var = valueID)
              row.names(x) <- paste(x$class,x$order,sep="_")
              classLabel <- x$class
              orderLabel <- x$order
              sampleLabel <- x$sample
              x$class <- NULL
              x$order <- NULL
              x$sample <- NULL
              
              x <- as.data.frame(t(x))
              if(log==TRUE){
                  x <- log2(x)  
              }
              annotationList <- data.frame(Class=classLabel)
              row.names(annotationList) <- names(x)
              
              
              ## set colour
              if(colors=="gbr"){
                  colors <- colorRampPalette(c("green", "black", "red"), space="rgb")(256);
              }else if(colors == "heat"){
                  colors <- heat.colors(256);
              }else if(colors == "topo"){
                  colors <- topo.colors(256);
              }else if(colors == "gray"){
                  colors <- colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
              }else{
                  colors <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(256));
              }
              
              
              res <- list()
              
              fig <- paste(para@outdir,"/",para@prefix,"-heatmap.pdf",sep="")
              resfile <- paste(para@outdir,"/",para@prefix,"-heatmap.rds",sep="")
              pdf(file = fig,width = width,height = height)
              res$heatmap <- pheatmap(x,annotation=annotationList,color=colors,
                                      show_rownames=FALSE,...)
              
              dev.off()
              
              fig_png <- str_replace(string = fig,pattern = ".pdf$",replacement = ".png")
              png(filename = fig_png,width = width, height = height,units = "in",res=120)
              pheatmap(x,annotation=annotationList,color=colors,show_rownames=FALSE,...)
              dev.off()
              
              res$fig <- fig_png
              res$highfig <- fig
              if(saveRds){
                saveRDS(res,file=resfile)
              }
              return(res)
          }
)




##' @title Filter peaks according to the RSD of peaks in QC samples
##' @description Filter peaks according to the RSD of peaks in QC samples. 
##' Usually used after missing value imputation.
##' @rdname filterQCPeaksByCV
##' @param para An object of metaXpara
##' @param cvFilter Filter peaks with the RSD in QC samples > cvFilter. 
##' @param valueID The name of the column which will be used.
##' @param ... Additional parameter
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' p <- filterQCPeaksByCV(para)
setGeneric("filterQCPeaksByCV",function(para,cvFilter=0.3,valueID="value",...) 
    standardGeneric("filterQCPeaksByCV"))
##' @describeIn filterQCPeaksByCV
setMethod("filterQCPeaksByCV", signature(para = "metaXpara"),
          function(para,cvFilter=0.3,valueID="value",...){
              
              peaksData <- para@peaksData
              if(sum(is.na(peaksData$class)) <=0 ){
                  message("Don't find QC sample in your data!")
                  return(para)
              }
              assign(x="valueID",value=valueID,envir=.GlobalEnv)
              qcData <- peaksData[is.na(peaksData$class),]
              cvData <- ddply(qcData,.(ID),summarise,
                              cv=abs(sd(get(valueID),na.rm = TRUE)/mean(get(valueID),
                                                                        na.rm = TRUE)))
              
              rmID <- cvData$ID[is.na(cvData$cv) | cvData$cv > cvFilter]
              para@peaksData <- peaksData[ !peaksData$ID %in% rmID,]
              para@peaksData <- merge(para@peaksData,cvData,by="ID")
              
              message("remove peaks by QC CV > ",cvFilter," : ",length(rmID))
              message("left peaks: ",length(cvData$ID)-length(rmID))
              return(para)
              
          })



##' @title Plot missing value distribution
##' @description  Plot missing value distribution.
##' @rdname plotMissValue
##' @docType methods
##' @param para A metaXpara object
##' @param width The width of the graphics region in inches. 
##' The default values are 8.
##' @param height The height of the graphics region in inches. 
##' The default values are 5.
##' @param ... Additional parameter
##' @return The figure name
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @exportMethod
##' @examples
##' library(faahKO)
##' xset <- group(faahko)
##' xset <- retcor(xset)
##' xset <- group(xset)
##' xset <- fillPeaks(xset)
##' peaksData <- as.data.frame(groupval(xset,"medret",value="into"))
##' peaksData$name <- row.names(peaksData)
##' para <- new("metaXpara")
##' rawPeaks(para) <- peaksData
##' sampleListFile(para) <- system.file("extdata/faahKO_sampleList.txt", 
##'     package = "metaX")
##' para <- reSetPeaksData(para)
##' plotMissValue(para)
setGeneric("plotMissValue",function(para,width=8,height=5,...) 
    standardGeneric("plotMissValue"))
##' @describeIn plotMissValue
setMethod("plotMissValue", signature(para = "metaXpara"), 
          function(para,width=8,height=5,...){
              
              fig <- paste(para@outdir,"/",para@prefix,"-missValueQC.png",sep="")
              
              highfig <- sub(pattern = "png$",replacement = "pdf",x = fig)
              
              
              pdf(file = highfig,width = width,height = height)
              
              peaksData <- para@peaksData
              peaksData[,"value"][ peaksData[,"value"]<=0] <- NA
              
              ## including qc sample
              nm <- ddply(peaksData,.(ID),summarise,nmiss=sum(is.na(value))/length(value))
              
              dat <- as.data.frame(table(cut(nm$nmiss,breaks = seq(0,1,by=0.1))))
              dat$breaks <- seq(0.1,1,by=0.1)
              dat$ratio <- sprintf("%.2f%%",100*dat$Freq/nrow(nm))
              
              ggobj1 <- ggplot(data=dat,aes(x=breaks,y=Freq))+
                  geom_bar(stat = "identity",width=0.07,fill="orangered")+
                  scale_x_continuous(breaks=dat$breaks)+
                  geom_text(aes(label = ratio),vjust=-0.2,size=4)+
                  ylab("Peaks number")+
                  xlab("Percent of missing value")+
                  ggtitle(label = paste("Total peaks:",nrow(nm),
                                        "; total samples:",
                                        length(unique(peaksData$sample))))
              
              print(ggobj1)
              
              #
              if(any(is.na(peaksData$class))){
                  # only true samples
                  if(any(!is.na(peaksData$class))){
                      nm <- ddply(peaksData[!is.na(peaksData$class),],.(ID),summarise,
                                  nmiss=sum(is.na(value))/length(value))
                      
                      dat <- as.data.frame(table(cut(nm$nmiss,breaks = seq(0,1,by=0.1))))
                      dat$breaks <- seq(0.1,1,by=0.1)
                      dat$ratio <- sprintf("%.2f%%",100*dat$Freq/nrow(nm))
                      
                      ggobj <- ggplot(data=dat,aes(x=breaks,y=Freq))+
                          geom_bar(stat = "identity",width=0.07,fill="orangered")+
                          scale_x_continuous(breaks=dat$breaks)+
                          geom_text(aes(label = ratio),vjust=-0.2,size=4)+
                          ylab("Peaks number")+
                          xlab("Percent of missing value")+
                          ggtitle(label = paste("Total peaks:",nrow(nm),
                                                "; total samples (non-QC):",
                                                length(unique(peaksData$sample[!is.na(peaksData$class)]))))
                      
                      print(ggobj)
                  }
                  
                  ## only qc sample
                  nm <- ddply(peaksData[is.na(peaksData$class),],.(ID),summarise,
                              nmiss=sum(is.na(value))/length(value))
                  
                  dat <- as.data.frame(table(cut(nm$nmiss,breaks = seq(0,1,by=0.1))))
                  dat$breaks <- seq(0.1,1,by=0.1)
                  dat$ratio <- sprintf("%.2f%%",100*dat$Freq/nrow(nm))
                  
                  ggobj <- ggplot(data=dat,aes(x=breaks,y=Freq))+
                      geom_bar(stat = "identity",width=0.07,fill="orangered")+
                      scale_x_continuous(breaks=dat$breaks)+
                      geom_text(aes(label = ratio),vjust=-0.2,size=4)+
                      ylab("Peaks number")+
                      xlab("Percent of missing value")+
                      ggtitle(label = paste("Total peaks:",nrow(nm),
                                            "; total samples (only QC):",
                                            length(unique(peaksData$sample[is.na(peaksData$class)]))))
                  
                  print(ggobj)
                  
              }
              
              dev.off()
              
              png(filename = fig,width = width,height = height,units = "in",res=150)
              print(ggobj1)
              dev.off()
              res <- list(fig=fig,highfig=highfig)
              return(res)
          }
)




##acc

##' @title dir.case
##' @description dir.case
##' @rdname dir.case
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' dir.case(para) <- "./"
setGeneric("dir.case<-",function(para,value) standardGeneric("dir.case<-"))
##' @describeIn metaXpara
setReplaceMethod("dir.case", signature(para = "metaXpara"), 
                 function(para,value){
                     para@dir.case <- value
                     para
                 }
)


##' @title dir.ctrl
##' @description dir.ctrl
##' @rdname dir.ctrl
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' dir.ctrl(para) <- "./"
setGeneric("dir.ctrl<-",function(para,value) standardGeneric("dir.ctrl<-"))
##' @describeIn metaXpara
setReplaceMethod("dir.ctrl", signature(para = "metaXpara"), 
                 function(para,value){
                     para@dir.ctrl <- value
                     para
                 }
)


##' @title sampleListFile
##' @description sampleListFile
##' @rdname sampleListFile
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' sampleListFile(para) <- "sample.txt"
setGeneric("sampleListFile<-",function(para,value) 
    standardGeneric("sampleListFile<-"))
##' @describeIn metaXpara
setReplaceMethod("sampleListFile", signature(para = "metaXpara"), 
                 function(para,value){
                     para@sampleListFile <- value
                     para
                 }
)

##' @title ratioPairs
##' @description ratioPairs
##' @rdname ratioPairs
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' ratioPairs(para) <- "1:2"
setGeneric("ratioPairs<-",function(para,value) standardGeneric("ratioPairs<-"))
##' @describeIn metaXpara
setReplaceMethod("ratioPairs", signature(para = "metaXpara"), 
                 function(para,value){
                     para@ratioPairs <- value
                     para
                 }
)


##' @title missValueImputeMethod
##' @description missValueImputeMethod
##' @rdname missValueImputeMethod
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' missValueImputeMethod(para) <- "knn"
setGeneric("missValueImputeMethod<-",function(para,value) 
    standardGeneric("missValueImputeMethod<-"))
##' @describeIn metaXpara
setReplaceMethod("missValueImputeMethod", signature(para = "metaXpara"), 
                 function(para,value){
                     para@missValueImputeMethod <- value
                     para
                 }
)


##' @title outdir
##' @description outdir
##' @rdname outdir
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' outdir(para) <- "outdir"
setGeneric("outdir<-",function(para,value) standardGeneric("outdir<-"))
##' @describeIn metaXpara
setReplaceMethod("outdir", signature(para = "metaXpara"), 
                 function(para,value){
                     para@outdir <- value
                     para
                 }
)


##' @title prefix
##' @description prefix
##' @rdname prefix
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' prefix(para) <- "test"
setGeneric("prefix<-",function(para,value) standardGeneric("prefix<-"))
##' @describeIn metaXpara
setReplaceMethod("prefix", signature(para = "metaXpara"), 
                 function(para,value){
                     para@prefix <- value
                     para
                 }
)


##' @title peaksData
##' @description peaksData
##' @rdname peaksData
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' peaksData(para) <- data.frame()
setGeneric("peaksData<-",function(para,value) standardGeneric("peaksData<-"))
##' @describeIn metaXpara
setReplaceMethod("peaksData", signature(para = "metaXpara"), 
                 function(para,value){
                     para@peaksData <- value
                     para
                 }
)


##' @title addValueNorm
##' @description addValueNorm
##' @rdname addValueNorm
##' @param para An object of metaXpara
##' @param value An object of metaXpara
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("metaXpara")
##' pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
##' sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
##' rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
##' sampleListFile(para) <- sfile
##' para <- reSetPeaksData(para)
##' addValueNorm(para) <- para
setGeneric("addValueNorm<-",function(para,value) 
    standardGeneric("addValueNorm<-"))
##' @describeIn metaXpara
setReplaceMethod("addValueNorm", 
                 signature(para = "metaXpara",value = "metaXpara"), 
                 function(para,value){
                     para@peaksData$valueNorm <- value@peaksData$value
                     para
                 }
)


##' @title rawPeaks
##' @description rawPeaks
##' @rdname rawPeaks
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' rawPeaks(para) <- data.frame()
setGeneric("rawPeaks<-",function(para,value) standardGeneric("rawPeaks<-"))
##' @describeIn metaXpara
setReplaceMethod("rawPeaks", signature(para = "metaXpara"), 
                 function(para,value){
                     para@rawPeaks <- value
                     para
                 }
)


##' @title idres
##' @description idres
##' @rdname idres
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' idres(para) <- data.frame()
setGeneric("idres<-",function(para,value) standardGeneric("idres<-"))
##' @describeIn metaXpara
setReplaceMethod("idres", signature(para = "metaXpara"), 
                 function(para,value){
                     para@idres <- value
                     para
                 }
)


##' @title xcmsSet.method
##' @description xcmsSet.method
##' @rdname xcmsSet.method
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.method(para) <- "centWave"
setGeneric("xcmsSet.method<-",function(para,value) 
    standardGeneric("xcmsSet.method<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.method", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.method <- value
                     para
                 }
)



##' @title xcmsSet.ppm
##' @description xcmsSet.ppm
##' @rdname xcmsSet.ppm
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.ppm(para) <- 10
setGeneric("xcmsSet.ppm<-",function(para,value) 
    standardGeneric("xcmsSet.ppm<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.ppm", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.ppm <- value
                     para
                 }
)


##' @title xcmsSet.peakwidth
##' @description xcmsSet.peakwidth
##' @rdname xcmsSet.peakwidth
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.peakwidth(para) <- 12
setGeneric("xcmsSet.peakwidth<-",function(para,value) 
    standardGeneric("xcmsSet.peakwidth<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.peakwidth", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.peakwidth <- value
                     para
                 }
)


##' @title xcmsSet.snthresh
##' @description xcmsSet.snthresh
##' @rdname xcmsSet.snthresh
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.snthresh(para) <- 5
setGeneric("xcmsSet.snthresh<-",function(para,value) 
    standardGeneric("xcmsSet.snthresh<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.snthresh", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.snthresh <- value
                     para
                 }
)


##' @title xcmsSet.prefilter
##' @description xcmsSet.prefilter
##' @rdname xcmsSet.prefilter
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.prefilter(para) <- c(1,5000)
setGeneric("xcmsSet.prefilter<-",function(para,value) 
    standardGeneric("xcmsSet.prefilter<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.prefilter", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.prefilter <- value
                     para
                 }
)


##' @title xcmsSet.mzCenterFun
##' @description xcmsSet.mzCenterFun
##' @rdname xcmsSet.mzCenterFun
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.mzCenterFun(para) <- "wMean"
setGeneric("xcmsSet.mzCenterFun<-",function(para,value) 
    standardGeneric("xcmsSet.mzCenterFun<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.mzCenterFun", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.mzCenterFun <- value
                     para
                 }
)


##' @title xcmsSet.integrate
##' @description xcmsSet.integrate
##' @rdname xcmsSet.integrate
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.integrate(para) <- 1
setGeneric("xcmsSet.integrate<-",function(para,value) 
    standardGeneric("xcmsSet.integrate<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.integrate", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.integrate <- value
                     para
                 }
)


##' @title xcmsSet.mzdiff
##' @description xcmsSet.mzdiff
##' @rdname xcmsSet.mzdiff
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.mzdiff(para) <- -0.001
setGeneric("xcmsSet.mzdiff<-",function(para,value) 
    standardGeneric("xcmsSet.mzdiff<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.mzdiff", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.mzdiff <- value
                     para
                 }
)


##' @title xcmsSet.noise
##' @description xcmsSet.noise
##' @rdname xcmsSet.noise
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.noise(para) <- 1000
setGeneric("xcmsSet.noise<-",function(para,value) 
    standardGeneric("xcmsSet.noise<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.noise", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.noise <- value
                     para
                 }
)


##' @title xcmsSet.verbose.columns
##' @description xcmsSet.verbose.columns
##' @rdname xcmsSet.verbose.columns
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.verbose.columns(para) <- FALSE
setGeneric("xcmsSet.verbose.columns<-",function(para,value) 
    standardGeneric("xcmsSet.verbose.columns<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.verbose.columns", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.verbose.columns <- value
                     para
                 }
)


##' @title xcmsSet.polarity
##' @description xcmsSet.polarity
##' @rdname xcmsSet.polarity
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.polarity(para) <- "positive"
setGeneric("xcmsSet.polarity<-",function(para,value) 
    standardGeneric("xcmsSet.polarity<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.polarity", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.polarity <- value
                     para
                 }
)

##' @title xcmsSet.profparam
##' @description xcmsSet.profparam
##' @rdname xcmsSet.profparam
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.profparam(para) <- list(step=0.005)
setGeneric("xcmsSet.profparam<-",function(para,value) 
    standardGeneric("xcmsSet.profparam<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.profparam", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.profparam <- value
                     para
                 }
)


##' @title xcmsSet.nSlaves
##' @description xcmsSet.nSlaves
##' @rdname xcmsSet.nSlaves
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.nSlaves(para) <- 8
setGeneric("xcmsSet.nSlaves<-",function(para,value) 
    standardGeneric("xcmsSet.nSlaves<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.nSlaves", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.nSlaves <- value
                     para
                 }
)

##' @title xcmsSet.fitgauss
##' @description xcmsSet.fitgauss
##' @rdname xcmsSet.fitgauss
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.fitgauss(para) <- FALSE
setGeneric("xcmsSet.fitgauss<-",function(para,value) 
    standardGeneric("xcmsSet.fitgauss<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.fitgauss", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.fitgauss <- value
                     para
                 }
)


##' @title xcmsSet.sleep
##' @description xcmsSet.sleep
##' @rdname xcmsSet.sleep
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.sleep(para) <- 0
setGeneric("xcmsSet.sleep<-",function(para,value) 
    standardGeneric("xcmsSet.sleep<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.sleep", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.sleep <- value
                     para
                 }
)


##' @title xcmsSet.fwhm
##' @description xcmsSet.fwhm
##' @rdname xcmsSet.fwhm
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.fwhm(para) <- 30
setGeneric("xcmsSet.fwhm<-",function(para,value) 
    standardGeneric("xcmsSet.fwhm<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.fwhm", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.fwhm <- value
                     para
                 }
)


##' @title xcmsSet.max
##' @description xcmsSet.max
##' @rdname xcmsSet.max
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.max(para) <- 5
setGeneric("xcmsSet.max<-",function(para,value) 
    standardGeneric("xcmsSet.max<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.max", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.max <- value
                     para
                 }
)


##' @title xcmsSet.step
##' @description xcmsSet.step
##' @rdname xcmsSet.step
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSet.step(para) <- 0.1
setGeneric("xcmsSet.step<-",function(para,value) 
    standardGeneric("xcmsSet.step<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSet.step", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSet.step <- value
                     para
                 }
)


##' @title group.bw0
##' @description group.bw0
##' @rdname group.bw0
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.bw0(para) <- 10
setGeneric("group.bw0<-",function(para,value) 
    standardGeneric("group.bw0<-"))
##' @describeIn metaXpara
setReplaceMethod("group.bw0", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.bw0 <- value
                     para
                 }
)


##' @title group.mzwid0
##' @description group.mzwid0
##' @rdname group.mzwid0
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.mzwid0(para) <- 0.015
setGeneric("group.mzwid0<-",function(para,value) 
    standardGeneric("group.mzwid0<-"))
##' @describeIn metaXpara
setReplaceMethod("group.mzwid0", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.mzwid0 <- value
                     para
                 }
)


##' @title group.bw
##' @description group.bw
##' @rdname group.bw
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.bw(para) <- 5
setGeneric("group.bw<-",function(para,value) 
    standardGeneric("group.bw<-"))
##' @describeIn metaXpara
setReplaceMethod("group.bw", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.bw <- value
                     para
                 }
)

##' @title group.mzwid
##' @description group.mzwid
##' @rdname group.mzwid
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.mzwid(para) <- 0.015
setGeneric("group.mzwid<-",function(para,value) 
    standardGeneric("group.mzwid<-"))
##' @describeIn metaXpara
setReplaceMethod("group.mzwid", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.mzwid <- value
                     para
                 }
)


##' @title group.minfrac
##' @description group.minfrac
##' @rdname group.minfrac
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.minfrac(para) <- 0.3
setGeneric("group.minfrac<-",function(para,value) 
    standardGeneric("group.minfrac<-"))
##' @describeIn metaXpara
setReplaceMethod("group.minfrac", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.minfrac <- value
                     para
                 }
)

##' @title group.minsamp
##' @description group.minsamp
##' @rdname group.minsamp
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.minsamp(para) <- 1
setGeneric("group.minsamp<-",function(para,value) 
    standardGeneric("group.minsamp<-"))
##' @describeIn metaXpara
setReplaceMethod("group.minsamp", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.minsamp <- value
                     para
                 }
)


##' @title group.max
##' @description group.max
##' @rdname group.max
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.max(para) <- 1000
setGeneric("group.max<-",function(para,value) 
    standardGeneric("group.max<-"))
##' @describeIn metaXpara
setReplaceMethod("group.max", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.max <- value
                     para
                 }
)


##' @title group.sleep
##' @description group.sleep
##' @rdname group.sleep
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' group.sleep(para) <- 0
setGeneric("group.sleep<-",function(para,value) 
    standardGeneric("group.sleep<-"))
##' @describeIn metaXpara
setReplaceMethod("group.sleep", signature(para = "metaXpara"), 
                 function(para,value){
                     para@group.sleep <- value
                     para
                 }
)


##' @title retcor.method
##' @description retcor.method
##' @rdname retcor.method
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' retcor.method(para) <- "obiwarp"
setGeneric("retcor.method<-",function(para,value) 
    standardGeneric("retcor.method<-"))
##' @describeIn metaXpara
setReplaceMethod("retcor.method", signature(para = "metaXpara"), 
                 function(para,value){
                     para@retcor.method <- value
                     para
                 }
)


##' @title retcor.profStep
##' @description retcor.profStep
##' @rdname retcor.profStep
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' retcor.profStep(para) <- 0.005
setGeneric("retcor.profStep<-",function(para,value) 
    standardGeneric("retcor.profStep<-"))
##' @describeIn metaXpara
setReplaceMethod("retcor.profStep", signature(para = "metaXpara"), 
                 function(para,value){
                     para@retcor.profStep <- value
                     para
                 }
)


##' @title retcor.plottype
##' @description retcor.plottype
##' @rdname retcor.plottype
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' retcor.plottype(para) <- "deviation"
setGeneric("retcor.plottype<-",function(para,value) 
    standardGeneric("retcor.plottype<-"))
##' @describeIn metaXpara
setReplaceMethod("retcor.plottype", signature(para = "metaXpara"), 
                 function(para,value){
                     para@retcor.plottype <- value
                     para
                 }
)


##' @title qcRlscSpan
##' @description qcRlscSpan
##' @rdname qcRlscSpan
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' qcRlscSpan(para) <- 0.4
setGeneric("qcRlscSpan<-",function(para,value) 
    standardGeneric("qcRlscSpan<-"))
##' @describeIn metaXpara
setReplaceMethod("qcRlscSpan", signature(para = "metaXpara"), 
                 function(para,value){
                     para@qcRlscSpan <- value
                     para
                 }
)


##' @title xcmsSetObj
##' @description xcmsSetObj
##' @rdname xcmsSetObj
##' @param para An object of metaXpara
##' @param value value
##' @return An object of metaXpara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' library(faahKO)
##' para <- new("metaXpara")
##' xcmsSetObj(para) <- faahko
setGeneric("xcmsSetObj<-",function(para,value) 
    standardGeneric("xcmsSetObj<-"))
##' @describeIn metaXpara
setReplaceMethod("xcmsSetObj", signature(para = "metaXpara"), 
                 function(para,value){
                     para@xcmsSetObj <- value
                     para
                 }
)


##' @title scale
##' @description scale
##' @rdname scale
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' scale(para) <- "uv"
setGeneric("scale<-",function(para,value) 
    standardGeneric("scale<-"))
##' @describeIn plsDAPara
setReplaceMethod("scale", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@scale <- value
                     para
                 }
)

##' @title center
##' @description center
##' @rdname center
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' center(para) <- TRUE
setGeneric("center<-",function(para,value) 
    standardGeneric("center<-"))
##' @describeIn plsDAPara
setReplaceMethod("center", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@center <- value
                     para
                 }
)


##' @title t
##' @description t
##' @rdname t
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' t(para) <- 1
setGeneric("t<-",function(para,value) 
    standardGeneric("t<-"))
##' @describeIn plsDAPara
setReplaceMethod("t", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@t <- value
                     para
                 }
)


##' @title validation
##' @description validation
##' @rdname validation
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' validation(para) <- "CV"
setGeneric("validation<-",function(para,value) 
    standardGeneric("validation<-"))
##' @describeIn plsDAPara
setReplaceMethod("validation", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@validation <- value
                     para
                 }
)


##' @title ncomp
##' @description ncomp
##' @rdname ncomp
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' ncomp(para) <- 5
setGeneric("ncomp<-",function(para,value) 
    standardGeneric("ncomp<-"))
##' @describeIn plsDAPara
setReplaceMethod("ncomp", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@ncomp <- value
                     para
                 }
)


##' @title nperm
##' @description nperm
##' @rdname nperm
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' nperm(para) <- 1000
setGeneric("nperm<-",function(para,value) 
    standardGeneric("nperm<-"))
##' @describeIn plsDAPara
setReplaceMethod("nperm", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@nperm <- value
                     para
                 }
)


##' @title kfold
##' @description kfold
##' @rdname kfold
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' kfold(para) <- 5
setGeneric("kfold<-",function(para,value) 
    standardGeneric("kfold<-"))
##' @describeIn plsDAPara
setReplaceMethod("kfold", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@kfold <- value
                     para
                 }
)


##' @title method
##' @description method
##' @rdname method
##' @param para An object of plsDAPara
##' @param value value
##' @return An object of plsDAPara
##' @docType methods
##' @exportMethod 
##' @author Bo Wen \email{wenbo@@genomics.cn}
##' @examples
##' para <- new("plsDAPara")
##' method(para) <- "oscorespls"
setGeneric("method<-",function(para,value) 
    standardGeneric("method<-"))
##' @describeIn plsDAPara
setReplaceMethod("method", signature(para = "plsDAPara"), 
                 function(para,value){
                     para@method <- value
                     para
                 }
)
