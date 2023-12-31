% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/prep.R
\name{transformation}
\alias{transformation}
\title{Data transformation}
\usage{
transformation(para, method = 1, valueID = "valueNorm", ...)
}
\arguments{
\item{para}{An metaX object}

\item{method}{The method for transformation, 1=log, 2=Cube root}

\item{valueID}{The name of column used for transformation}

\item{...}{Additional parameter}
}
\value{
An new metaX object
}
\description{
Data transformation
}
\examples{
para <- new("metaXpara")
pfile <- system.file("extdata/MTBLS79.txt",package = "metaX")
sfile <- system.file("extdata/MTBLS79_sampleList.txt",package = "metaX")
rawPeaks(para) <- read.delim(pfile,check.names = FALSE)
sampleListFile(para) <- sfile
para <- reSetPeaksData(para)
para <- missingValueImpute(para)
para <- transformation(para,valueID = "value")
}
\author{
Bo Wen \email{wenbo@genomics.cn}
}

