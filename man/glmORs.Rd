% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glmformat.R
\name{glmORs}
\alias{glmORs}
\title{Returns Odds Ratios, 95\% confidence intervals, and p-values}
\usage{
glmORs(x, dps = 3)
}
\arguments{
\item{x}{a \code{glm} object}

\item{dps}{integer, number of decimal places desired (default=3)}
}
\value{
dataframe
}
\description{
Returns Odds Ratios, or exponentiated coefficients,
95\% confidence intervals, and p-values from a \code{glm} object
}
\examples{
data(mtcars)
otemp<-with(mtcars, glm(vs~mpg+wt, family=binomial))
glmORs(otemp)
}
