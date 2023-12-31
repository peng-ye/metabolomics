% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/tool.R
\name{permutePLSDA}
\alias{permutePLSDA}
\title{permutePLSDA}
\usage{
permutePLSDA(x, y, n = 100, np = 2, outdir = "./", prefix = "metaX",
  tol = 0.001, cpu = 0, ...)
}
\arguments{
\item{x}{a matrix of observations.}

\item{y}{a vector or matrix of responses.}

\item{n}{number of permutations to compute the PLD-DA p-value based on R2
magnitude. Default n=100}

\item{np}{the number of components to be used in the modelling.}

\item{outdir}{output dir}

\item{prefix}{the prefix of output figure file}

\item{tol}{tolerance value based on maximum change of cumulative R-squared
coefficient for each additional PLS component. Default tol=0.001}

\item{cpu}{0}

\item{...}{additional arguments}
}
\value{
pvalue
}
\description{
Validation of the PLS-DA model by using permutation test
statistics
}

