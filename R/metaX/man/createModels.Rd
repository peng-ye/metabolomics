% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/bootPLSDA.R
\name{createModels}
\alias{createModels}
\title{Create predictive models}
\usage{
createModels(para, method = "plsda", group = NA, valueID = "value", ...)
}
\arguments{
\item{para}{An object of \code{metaXpara}}

\item{method}{Method for model construction}

\item{group}{Sample class used}

\item{valueID}{The name of column used}

\item{...}{Additional arguments.}
}
\value{
A list object
}
\description{
Create predictive models
}
\examples{
para <- new("metaXpara")
pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
sampleListFile(para) <- sfile
para <- reSetPeaksData(para)
para <- missingValueImpute(para)
para <- transformation(para,method = 1,valueID = "value")
para <- metaX::preProcess(para,scale = "uv",center = TRUE,
                        valueID = "value")
rs <- createModels(para,method="plsda",group=c("S","C"),valueID="value")
}

