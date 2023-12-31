% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/metaX.R
\docType{methods}
\name{plotPCA}
\alias{plotPCA}
\alias{plotPCA,metaXpara-method}
\title{Plot PCA figure}
\usage{
plotPCA(para, pcaMethod = "svdImpute", valueID = "valueNorm",
  label = "order", rmQC = TRUE, batch = FALSE, scale = "none",
  center = FALSE, saveRds = FALSE, ...)

\S4method{plotPCA}{metaXpara}(para, pcaMethod = "svdImpute",
  valueID = "valueNorm", label = "order", rmQC = TRUE, batch = FALSE,
  scale = "none", center = FALSE, saveRds = FALSE, ...)
}
\arguments{
\item{para}{A \code{metaXpara} object}

\item{pcaMethod}{See \code{\link{pca}} in \pkg{pcaMethods}}

\item{valueID}{The name of the column which will be used}

\item{label}{The label used for plot PCA figure, default is "order"}

\item{rmQC}{A logical indicates whether remove QC data}

\item{batch}{A logical indicates whether output batch information}

\item{scale}{Scaling, see \code{\link{pca}} in \pkg{pcaMethods}}

\item{center}{Centering, see \code{\link{pca}} in \pkg{pcaMethods}}

\item{saveRds}{Boolean, setting the argument to TRUE to save some objects to
disk for debug. Only useful for developer. Default is FALSE.}

\item{...}{Additional parameter}
}
\value{
none
}
\description{
Plot PCA figure
}
\section{Methods (by class)}{
\itemize{
\item \code{metaXpara}: 
}}
\examples{
para <- new("metaXpara")
pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
sampleListFile(para) <- sfile
para <- reSetPeaksData(para)
para <- missingValueImpute(para)
para <- transformation(para,valueID = "value")
metaX::plotPCA(para,valueID="value",scale="uv",center=TRUE)
}
\author{
Bo Wen \email{wenbo@genomics.cn}
}

