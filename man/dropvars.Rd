% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datautilities.R
\name{dropvars}
\alias{dropvars}
\title{drops or keeps variables from a dataframe}
\usage{
dropvars(dat, x)
}
\arguments{
\item{dat}{A data frame}

\item{x}{A vector of variables to be dropped}
}
\value{
A dataframe
}
\description{
drops or keeps variables from a dataframe
}
\examples{
data(mtcars)
dvars<-c("am", "cyl", "wt")
newdat<-dropvars(mtcars, dvars)
}
