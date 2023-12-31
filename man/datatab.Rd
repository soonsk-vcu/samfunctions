% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatab.R
\name{datatab}
\alias{datatab}
\title{Construct a data dictionary}
\usage{
datatab(key, def)
}
\arguments{
\item{key}{Vector of column names}

\item{def}{Vector of descriptions for each column}
}
\description{
Concats column names and descriptions and outputs a kable table.
}
\examples{
convert(c("A","B"), c("Letter A", "Letter B"))
}
\author{
Samuel Soon
}
