% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/model.R
\name{runPLSDA}
\alias{runPLSDA}
\title{runPLSDA}
\usage{
runPLSDA(para, plsdaPara, auc = TRUE, sample = NULL,
  valueID = "valueNorm", cpu = 0, label = "order", ...)
}
\arguments{
\item{para}{An object of metaXpara}

\item{plsdaPara}{An object of plsDAPara}

\item{auc}{A logical indicates whether calculate the AUC}

\item{sample}{Sample class}

\item{valueID}{The name of column used}

\item{cpu}{The number of cpu used}

\item{label}{The label used for plot}

\item{...}{additional arguments}
}
\value{
pvalue
}
\description{
Validation of the PLS-DA model by using permutation test
statistics
}

