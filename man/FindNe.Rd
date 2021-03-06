% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ne.R
\name{FindNe}
\alias{FindNe}
\title{Find number of breeding males and frmales for targeted effective population size.}
\usage{
FindNe(NeTarget, nM, nF, ...)
}
\arguments{
\item{NeTarget}{numeric, targeted effective population size.}

\item{nM}{numeric, vector of number of breeding males to evaluate over.}

\item{nF}{numeric, vector of number of breeding males to evaluate over.}

\item{...}{passed to \code{\link{Ne}}.}
}
\value{
Data frame with evaluated combinations ranked by distance from the target.
}
\description{
\code{FindNe} searches which number of breeding males and frmales give targeted
  effective population size.
}
\examples{
FindNe(NeTarget=100, nM=c(50, 100, 150), nF=c(50, 100, 150))
FindNe(NeTarget=100, nM=c(10, 20, 30, 50, 100), nF=c(50, 100, 150), VarProgM=0)
}

