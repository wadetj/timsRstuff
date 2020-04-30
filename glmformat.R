
#' Returns Odds Ratios, 95\% confidence intervals, and p-values
#'
#' Returns Odds Ratios, or exponentiated coefficients,
#' 95\% confidence intervals, and p-values from a \code{glm} object
#'
#' @param x a \code{glm} object
#' @param dps integer, number of decimal places desired (default=3)
#' @return dataframe
#' @examples
#' data(mtcars)
#' otemp<-with(mtcars, glm(vs~mpg+wt, family=binomial))
#' glmORs(otemp)

glmORs<-function(x, dps=3){
  ors<-exp(x$coefficients)
  ors<-na.omit(ors)
  coefs<-na.omit(x$coefficients)
   ses<-summary(x)$coefficients[, 2]
  pvals<-summary(x)$coefficients[, 4]
  orslo<-exp(coefs-(1.96*ses))
  orsup<-exp(coefs+(1.96*ses))
  N<-x$df.null+1
  dat<-cbind.data.frame(round(ors, dps), round(orslo, dps),  round(orsup, dps), format.pval(pvals, dps))
  dat<-dat[-1,]
  dat<-cbind.data.frame(dat, N)
  dat$N[1:nrow(dat)-1]=""
 colnames(dat)<-c("OR", "95LB", "95UB", "p", "N")
    dat
}


#' Returns coefficients,  95\% confidence intervals, and p-values
#'
#' Returns coefficients, #' 95\% confidence intervals,
#' and p-values from a \code{glm} object
#'
#' @param x glm object
#' @param dps integer, number of decimal places desired (default=3)
#' @return dataframe
#' @examples
#' data(mtcars)
#' otemp<-with(mtcars, glm(mpg~+wt+vs))
#' glmCIs(otemp)

 glmCIs<-function(x, dps=3){
  b<-x$coefficients
  b<-na.omit(b)
  coefs<-na.omit(x$coefficients)
   ses<-summary(x)$coefficients[, 2]
  pvals<-summary(x)$coefficients[, 4]
  blo<-coefs-(1.96*ses)
  bup<-coefs+(1.96*ses)

 # if(length((grep("g+lm", x$call))==0)) {
	#
	#}
	N<-summary(x)$df[1]+summary(x)$df[2]

	if(length((grep("g+lm", x$call))!=0)) {
		N<-x$df.null+1
	}
 dat<-cbind.data.frame(round(b, dps), round(blo, dps),  round(bup, dps), format.pval(pvals, dps))
 dat<-dat[-1,]
 dat<-cbind.data.frame(dat, N)
 dat$N[1:nrow(dat)-1]=""
 colnames(dat)<-c("Beta", "95LB", "95UB", "p", "N")
 dat
 }


