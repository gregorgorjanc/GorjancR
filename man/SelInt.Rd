% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelInt.R
\name{SelInt}
\alias{SelInt}
\title{Selection intensity.}
\usage{
SelInt(x)
}
\arguments{
\item{x}{numeric, proportion of selected individuals.}
}
\value{
Selection intensity.
}
\description{
\code{SelInt} evaluates selection intensity given proportion of selected individuals.
}
\examples{
SelInt(x=1.00)
SelInt(x=0.99)
SelInt(x=0.50)
SelInt(x=0.05)
SelInt(x=0.01)
}

