#' provides adjustment for multiple comparisons based 
#' 
#' takes numeric vector of p-values and adjusts for multiple comparisons using false discovery rate (benjamini and hochberg 1995, default) and bonferroni (method="bon")
#' @param pvalues numeric vector of p-values
#' @param qstar overall alpha level (default=0.05)
#' @param method "fdr" (default) or "bon"
#' @return dataframe with starred p-values that meet adjusted significance level
#' @examples 
#' from benjamini and hochberg 1995
#' pvals<-c(0.0001, 0.0004, 0.0019, 0.0095, 0.0201, 0.0278, 0.0298, 0.0344, 0.0459, 0.3240, 0.4262, 0.5719, 0.6528, 0.7590, 1.00)
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
#' @export

falsediscovery<-function(pvalues, qstar=0.05, method="fdr"){
if(method=="fdr"){
  len<-c(1:length(pvalues))
  n<-length(pvalues)
  pvals<-sort(pvalues)
  fd<-(len/n)*qstar
  #if no pvalues are less than fdr, assign padj to -1 to avoid error message
  if(length(pvals[pvals<fd])>0) {
    padj<-max(pvals[pvals<fd])
  } else {
    padj=-1
  }
  crit<-ifelse(pvals<=padj, "*", "")
   #calculate adjusted p-values for fdr
  p_adj<-(n/len)*pvals
  for(i in seq(n-1, 1, -1)) {
	p_adj[i]<-ifelse(p_adj[i]<p_adj[i+1], p_adj[i], p_adj[i+1])
	}
  dat<-cbind.data.frame(pvals, fd, crit, p_adj)
  return(dat)
  print(dat)
}
  
if(method=="bon"){
	qadj<-qstar/length(pvalues)
	pvals<-sort(pvals)
	crit<-ifelse(pvals<=qadj, "*", "")
	#calculate adjusted p-values for bonferroni
	p_adj<-pvals*length(pvalues)
	p_adj<-ifelse(p_adj>1, 1, p_adj)
	dat<-cbind.data.frame(pvals, crit, p_adj)
	return(dat)
	print(dat)
  }
}

#' @examples
#' #example from benjamini and hochberg 1995
#' #bonferroni is more conservative
#' pvals<-c(0.0001, 0.0004, 0.0019, 0.0095, 0.0201, 0.0278, 0.0298, 0.0344, 0.0459, 0.3240, 0.4262, 0.5719, 0.6528, 0.7590, 1.00)
#' ran<-runif(15)
#' ord<-order(ran)
#' pvals<-pvals[ord]
#' falsediscovery(pvals)
#' falsediscovery(pvals, method="bon")

#' #example from https://www.statisticshowto.com/benjamini-hochberg-procedure/
#' #note that p-values 0.039 and 0.041 are still considered significant 
#' #even though higher than fdr
#' pvals<-c(0.205, 0.074, 0.060, 0.042, 0.041, 0.039, 0.008, 0.001, c(rep(0.3, 17)))
#' falsediscovery(pvals, qstar=0.25)

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



  
