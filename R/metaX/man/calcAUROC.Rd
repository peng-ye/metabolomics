% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ROC.R
\docType{methods}
\name{calcAUROC}
\alias{calcAUROC}
\alias{calcAUROC,numeric-method}
\title{Classical univariate ROC analysis}
\usage{
calcAUROC(x, y, cgroup, plot, ...)

\S4method{calcAUROC}{numeric}(x, y, cgroup, plot, ...)
}
\arguments{
\item{x}{A numeric vector}

\item{y}{A response vector}

\item{cgroup}{Sample class used}

\item{plot}{A logical indicates whether plot}

\item{...}{Additional parameter}
}
\value{
A data.frame
}
\description{
Classical univariate ROC analysis
}
\section{Methods (by class)}{
\itemize{
\item \code{numeric}: 
}}
\examples{
\donttest{
x <- rnorm(50,2,2)
y<- rep(c("c","t"),25)
calcAUROC(x,y)
}
}
\author{
Bo Wen \email{wenbo@genomics.cn}
}

