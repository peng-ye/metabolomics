

##' @title Feature selection and modeling
##' @description Feature selection and modeling
##' @param para An object of \code{metaXpara}
##' @param group The sample class used
##' @param method Method for feature selection and modeling
##' @param valueID The column name used
##' @param fold k-fold
##' @param repeats The repeat number
##' @param verbose Whether output or not
##' @param ... Additional parameters
##' @return The result of feature selection and modeling
##' @export
featureSelection=function(para,group,method="rf",valueID="value",fold=5,
                          repeats=10,verbose=FALSE,...){
    
    peaksData <- para@peaksData
    peaksData <- dplyr::filter(peaksData,class %in% group)
    peaksData$class <- as.character(peaksData$class)
    dat <- dcast(peaksData,sample+class~ID,value.var = valueID)
    x <- dplyr::select(dat,-sample,-class)
    y <- dat$class
    if(method == "rf"){
        res <- rfSelection(x,y,fold = fold,repeats=repeats,...)
    }else if(method == "svmRadial"){
        res <- svmRadialSelection(x,y)
    }
    return(res)
    
}



rfSelection=function(x,y,method,sizes=NULL,fold=5,repeats=10,verbose=FALSE,...){
    
    if(nrow(x)!=length(y)){
        stop("The number of row of x must be equal to the length of y!")
    }
    
    
    if(!is.factor(y)){
        y <- as.factor(as.character(y))
    }
    
    if(is.null(sizes)){
        sizes <- seq(1,nrow(x),by=1)
    }
    trainctrl <- trainControl(method = "cv",
                              repeats = repeats,
                              number = fold,
                              classProbs = TRUE,
                              verboseIter=verbose,
                              summaryFunction = twoClassSummary)
    rfectrl <- rfeControl(functions=rfFuncs, 
                          method = "cv",
                          repeats = repeats,
                          number = fold,
                          returnResamp="final", verbose = verbose)
    rferes <- rfe(x, y, sizes = sizes, 
                  metric = "Accuracy", 
                  maximize = TRUE, 
                  tuneLength=10,
                  rfeControl = rfectrl, 
                  trControl = trainctrl)
    
    return(rferes)
    
}

svmRadialSelection=function(x,y,method,sizes=NULL,fold=5,repeats=10,
                            verbose=FALSE,...){
    
    if(!is.factor(y)){
        y <- as.factor(as.character(y))
    }
    
    if(is.null(sizes)){
        sizes <- seq(1,nrow(x),by=1)
    }
    trainctrl <- trainControl(method = "cv",
                              repeats = repeats,
                              number = fold,
                              #metric = " ROC", 
                              classProbs = TRUE,
                              verboseIter=verbose,
                              summaryFunction = twoClassSummary)
    rfectrl <- rfeControl(functions=caretFuncs, 
                          method = "cv",
                          repeats = repeats,
                          number = fold,
                          returnResamp="final", verbose = verbose)
    rferes <- rfe(x, y, sizes = sizes, 
                  #metric = "Accuracy", 
                  #maximize = TRUE,
                  method="svmRadial",
                  tuneLength=10,
                  rfeControl = rfectrl, 
                  trControl = trainctrl)
    
    return(rferes)
    
}