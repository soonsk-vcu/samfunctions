% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corapp.R
\name{cortesttable}
\alias{cortesttable}
\title{make a correlation p-value matrix}
\usage{
cortesttable(df, alternative = NULL, method = "Spearman")
}
\arguments{
\item{df}{purely numerical data frame}

\item{alternative}{parameter to be passed to cor.test}

\item{method}{parameter to be passed to cor.test}
}
\description{
returns a matrix of correlation test p-values from cor.test
}
\author{
Samuel Soon
}
