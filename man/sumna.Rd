% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datautilities.R
\name{sumna}
\alias{sumna}
\title{sum a vector or numeric variable correctly accounting for NA}
\usage{
sumna(x)
}
\arguments{
\item{x}{A numeric vector}
}
\value{
returns NA only of all are NA, otherwise returns sum of non missing value
}
\description{
sum a vector or numeric variable correctly accounting for NA
}
\note{
compare to sum(x, na.rm=TRUE) which returns 0 if all values are NA
}
\examples{
x<-c(rep(NA, 5))
sum(x, na.rm=TRUE)
sumna(x)
y<-c(rep(NA, 5), 1, 2, 3)
sumna(y)
}
