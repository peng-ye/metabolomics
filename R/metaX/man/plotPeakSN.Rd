% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/metaX.R
\docType{methods}
\name{plotPeakSN}
\alias{plotPeakSN}
\alias{plotPeakSN,metaXpara-method}
\title{Plot the distribution of the peaks S/N}
\usage{
plotPeakSN(x, ...)

\S4method{plotPeakSN}{metaXpara}(x, ...)
}
\arguments{
\item{x}{A metaXpara object}

\item{...}{Additional parameter}
}
\value{
none
}
\description{
Plot the distribution of the peaks S/N, only suitable for
XCMS result. This function generates a figure.
}
\section{Methods (by class)}{
\itemize{
\item \code{metaXpara}: 
}}
\examples{
library(faahKO)
xset <- group(faahko)
xset <- retcor(xset)
xset <- group(xset)
xset <- fillPeaks(xset)
para <- new("metaXpara")
xcmsSetObj(para) <- xset
plotPeakSN(para)
}
\author{
Bo Wen \email{wenbo@genomics.cn}
}

