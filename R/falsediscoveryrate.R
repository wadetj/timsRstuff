#' p-value and confidence interval adjustment for multiple comparisons
#' 
#' takes numeric vector of p-values and adjusts for multiple comparisons using false discovery rate (benjamini and hochberg 1995) and bonferroni (method="bon")
#' optionally takes numeric vectors of estimates and standard errors and produces confidence intervals adjusted for multiple comparisons (benjamini et al. 2005)
#' @param pvalues numeric vector of p-values
#' @param qstar overall alpha level (default=0.05)
#' @param method "fdr" (default) or "bon"
#' @param ci FALSE (default) or TRUE to produce adjusted confidence intervals
#' @param ses numeric vector of standard errors, required if ci=TRUE 
#' @param estimates numeric vector of estimates, required if ci=TRUE
#' @return dataframe with starred p-values and optionally adjusted confidence intervals that meet adjusted significance level. 
#' @examples 
#' #from benjamini and hochberg 1995
#' pvals<-c(0.0001, 0.0004, 0.0019, 0.0095, 0.0201, 0.0278, 0.0298, 0.0344, 0.0459, 0.3240, 0.4262, 0.5719, 0.6528, 0.7590, 1.00)
#' falsediscovery(pvals)
#' falsediscovery(pvals, method="bon")
#' 
#' #from https://www.statisticshowto.com/benjamini-hochberg-procedure/
#' #note that p-values 0.039 and 0.041 are still considered significant even though higher than fdr
#' pvals<-c(0.205, 0.074, 0.060, 0.042, 0.041, 0.039, 0.008, 0.001, c(rep(0.3, 17)))
#' falsediscovery(pvals, qstar=0.25)
#' 
#' #example from regression model
#' library(datasets)
#' data(swiss)
#' otemp<-with(swiss, lm(Infant.Mortality~(Fertility+Agriculture+Examination+Education+Catholic)^2))
#' pvals<-summary(otemp)$coefficients[,4]
#' #remove intercept
#' pvals<-pvals[-1]
#' falsediscovery(pvals)
#' falsediscovery(pvals, qstar=0.2)
#' falsediscovery(pvals, method="bon")
#' #note need example with CIs
#' @export

falsediscovery<-function(pvalues, qstar=0.05, method="fdr", dp=5, ci=FALSE, ses=NULL, estimates=NULL){
  if(method=="fdr"){
    len<-c(1:length(pvalues))
    n<-length(pvalues)
    ord<-order(pvalues)
    pvals<-pvalues[ord]
    fd<-(len/n)*qstar
    #if no pvalues are less than fdr, assign padj to -1 to avoid error message
    if(length(pvals[pvals<fd])>0) {
      padj<-max(pvals[pvals<fd])
    } else {
      padj=-1
    }
    
    crit<-ifelse(pvals<=padj, "*", "")
    if(ci==FALSE) {	
      pvals<-round(pvals, dp)
      fd<-round(fd, dp)
      dat<-cbind.data.frame(pvals, fd, crit)
      return(dat)
      print(dat)
    }
    if(ci==TRUE) {
      if(is.null(ses) | is.null(estimates)) stop ("SEs and estimates needed for CI calculation")
      sesx<-round(ses[ord], dp)
      ests<-round(estimates[ord], dp)
      z<-ifelse(padj!=-1, qnorm(1-padj/2), NA)
      ci.lb<-round(ests-(z*sesx), dp)
      ci.ub<-round(ests+(z*sesx), dp)
      pvals<-round(pvals, dp)
      fd<-round(fd, dp)
      dat<-cbind.data.frame("estimates"=ests, ci.lb,ci.ub, pvals,fd, crit)
      return(dat)
      print(dat)
    }
  }
  
  if(method=="bon"){
    n<-length(pvalues)
    qadj<-qstar/n
    ord<-order(pvalues)
    pvals<-pvalues[ord]
    crit<-ifelse(pvals<=qadj, "*", "")
    
    if(ci=="FALSE") {
      round(pvals, dp)
      dat<-cbind.data.frame(pvals, crit)
      return(dat)
      print(dat)
    }
    if(ci=="TRUE"){
      if(is.null(ses) | is.null(estimates)) stop ("SEs and estimates needed for CI calculation")
      sesx<-round(ses[ord], dp)
      ests<-round(estimates[ord], dp)
      z<-qnorm(1-qstar/(2*n))
      ci.lb<-round(ests-(z*sesx), dp)
      ci.ub<-round(ests+(z*sesx), dp)
      dat<-cbind.data.frame("estimates"=ests, ci.lb,ci.ub, pvals, crit)
      return(dat)
      print(dat)
    }
  }
}

