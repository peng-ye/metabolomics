


##' @title Correlation network analysis
##' @description Correlation network analysis
##' @param para A metaXpara object
##' @param valueID The name of the column that used for plot
##' @param group Samples used for plot
##' @param cor.method Method for correlation:"pearson","spearman" or "kendall"
##' @param threshold A threshold of significance levels of 
##' differential correlation
##' @param p.adjust.methods c("local", holm", "hochberg", "hommel", 
##' "bonferroni", "BH", "BY", "fdr", "none")
##' @param ... Additional parameter
##' @return The name of result file
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
##' resfile <- cor.network(para,group=c("S","C"))
cor.network=function(para,group,valueID="value",cor.method="spearman",
                     threshold=0.1,p.adjust.methods="BH"){
    if(length(group)!=2){
        print(group)
        stop("We only handle two classes network!\n")
    }
    #sampleList  <- read.delim(para@sampleListFile)
    
    peaksData <- para@peaksData
    peaksData <- dplyr::filter(peaksData,class %in% group)
    xyData <- dcast(peaksData,sample+class~ID,value.var = valueID)
    xyData <- dplyr::mutate(xyData,sample=NULL)
    
    cond1 <- dplyr::filter(xyData,class %in% group[1]) %>% 
        dplyr::mutate(class=NULL) %>% t() 
    cond2 <- dplyr::filter(xyData,class %in% group[2]) %>% 
        dplyr::mutate(class=NULL) %>% t()
    

    resfile <- paste(para@outdir,"/",para@prefix,"-correlation_network.txt",
                     sep="")
    resfig <- paste(para@outdir,"/",para@prefix,"-correlation_network.pdf",
                     sep="")
    if(!is.na(pmatch(p.adjust.methods,table = "local"))){
        pdf(resfig)
    }
    
    comp.2.cc.fdr(output.file=resfile, data1 = cond1,data2 = cond2, 
                  method=cor.method,threshold=threshold,
                  p.adjust.methods = p.adjust.methods)
    
    if(!is.na(pmatch(p.adjust.methods,table = "local"))){
        dev.off()
    }
    
    #res <- read.delim(file = resfile,check.names=FALSE)
    message("Write the result to ",resfile)
    return(resfile)
}


##' @title Plot correlation network map
##' @description  Plot correlation network map
##' @param para A metaXpara object
##' @param valueID The name of the column that used for plot
##' @param group Samples used for plot
##' @param cor.thr Threshold of correlation
##' @param degree.thr Threshold of degree of node
##' @param size.factor Node size factor for plot 
##' @param layout layout for plotting
##' @param ... Additional parameter
##' @return An object of igraph
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
##' gg <- plotNetwork(para,group=c("S","C"),degree.thr = 10,cor.thr = 0.8)
plotNetwork=function(para,group,valueID="value",cor.thr=0.95,degree.thr=10,
                     size.factor=0.5,layout=layout_in_circle,...){
    peaksData <- para@peaksData
    peaksData <- dplyr::filter(peaksData,class %in% group)
    xyData <- dcast(peaksData,sample+class~ID,value.var = valueID)
    xyData <- dplyr::mutate(xyData,sample=NULL,class=NULL) %>% t() 
    
    resfig <- paste(para@outdir,"/",para@prefix,"-network.pdf",sep="")
    pdf(resfig,width = 10,height = 10)
    #igraph.options(vertex.size=6)
    
    gg <- generate_g(xyData,cor.thr = cor.thr,edge.width = 1.5,node.size = 3)
    V(gg)$size=degree(gg)*size.factor
    ## check the degree of each node
    #identify those vertices part of less than three edges
    bad.vs<-V(gg)[degree(gg)<=degree.thr] 
    gg<-delete.vertices(gg, bad.vs) #exclude them from the graph
    message("V: ",vcount(gg))
    message("E: ",ecount(gg))
    plot(gg,layout=layout,vertex.label.cex=0.6,...)
    dev.off()
    message(resfig)
    return(gg)
    
}

##' @title Plot correlation heatmap
##' @description  This function plots correlation heatmap.
##' @param para A metaXpara object
##' @param valueID The name of the column that used for plot
##' @param samples Samples used for plot
##' @param label Label to show in figure
##' @param cor.method Method used for correlation
##' @param shownames A logical indicates whether show names when plot
##' @param width The width of the graphics region in inches. 
##' The default values are 6.
##' @param height The height of the graphics region in inches. 
##' The default values are 6.
##' @param anno A logical value indicates whether to plot heatmap with 
##' annotating class information
##' @param cluster A logical value indicates whether to do the cluster when 
##' anno is TRUE 
##' @param ... Additional parameter
##' @return The fig name
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
##' plotCorHeatmap(para,valueID="value",samples=NULL,width=6,anno=TRUE)
plotCorHeatmap=function(para,valueID="value",samples=NA,label="order",width=6,
                        cor.method="spearman",
                        height=6,anno=FALSE,cluster=FALSE,shownames=FALSE,...){
    peaksData <- para@peaksData
    samList <- read.delim(para@sampleListFile)
    
    if(is.null(samples)){
        message("use all the samples...")
    }else if(any(is.na(samples))){
        ## plot for QC samples
        peaksData <- dplyr::filter(peaksData,is.na(class))
        samList <- dplyr::filter(samList,is.na(class))
    }else{
        peaksData <- dplyr::filter(peaksData,class %in% samples)
        samList <- dplyr::filter(samList,class %in% samples)
    }
    
    if(label=="order"){
        xyData <- dcast(peaksData,ID~order,value.var = valueID)
        xyData <- dplyr::select(xyData,-ID)
    }else{
        xyData <- dcast(peaksData,ID~sample,value.var = valueID)
        xyData <- dplyr::select(xyData,-ID)
        
    }
    
    
    
    samList <- samList[order(samList$class,samList$order),]
    samList$order <- as.character(samList$order)
    if(label=="order"){
        diffs <- setdiff(samList$order,names(xyData))
        if(length(diffs)>=1){
            warning("The following samples are not found in the samples!")
            warning(paste(diffs,collapse = ","))
        }
        samList <- dplyr::filter(samList,!order %in% diffs)
        xyData <- xyData[,samList$order]
    }else{
        diffs <- setdiff(samList$sample,names(xyData))
        if(length(diffs)>=1){
            warning("The following samples are not found in the samples!")
            warning(paste(diffs,collapse = ","))
        }
        samList <- dplyr::filter(samList,!sample %in% diffs)
        xyData <- xyData[,samList$sample]
    }
    corres <- cor(xyData,method = cor.method,...)
    if(is.null(samples)){
        prefix = "ALL"
    }else{
        prefix <- ifelse(any(is.na(samples)),"QC",paste(samples,collapse = "_"))    
    }
    
    figpdf <- paste(para@outdir,"/",para@prefix,"-",prefix,"-corHeatmap.pdf",
                 sep="")
    figpng <- gsub(pattern = "pdf$",replacement = "png",x = figpdf)
    
    if(anno==FALSE){
        new.palette=colorRampPalette(c("black","red","yellow","white"),
                                     space="rgb")
        
        pdf(figpdf,width = width,height =height)
        gg <- levelplot(corres,col.regions=new.palette(40),cuts=30,
                        xlab="",ylab="",
                        scales=list(x=list(rot=90)))
        print(gg)
        dev.off()
        
        png(figpng,width = width,height = height,units = "in",res = 120)
        gg <- levelplot(corres,col.regions=new.palette(40),cuts=30,
                        xlab="",ylab="",
                        scales=list(x=list(rot=90)))
        print(gg)
        dev.off()
    }else{
        ## use pheatmap
        samList$class <- as.character(samList$class)
        samList$class[is.na(samList$class)] <- "QC"
        annotationList <- data.frame(Class=samList$class,
                                     Batch=as.character(samList$batch))
        if(label=="order"){
            row.names(annotationList) <- samList$order
        }else{
            row.names(annotationList) <- samList$sample
        }
        
        pdf(figpdf,width = width,height =height)
        pheatmap(corres,annotation=annotationList,border_color = NA,
                 cluster_rows=cluster,cluster_cols = cluster,
                 show_colnames = shownames,show_rownames=shownames,...)
        dev.off()
        
        png(figpng,width = width,height = height,units = "in",res = 120)
        pheatmap(corres,annotation=annotationList,border_color = NA,
                 cluster_rows=cluster,cluster_cols = cluster,
                 show_colnames = shownames,show_rownames=shownames,...)
        dev.off()
        
    }
    res <- list(fig=figpng,highfig=figpdf)
    return(res)
}
