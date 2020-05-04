
#' produces summary statistics for a numeric vector
#' @param x A numeric, complex or logical vector
#' @examples
#' x<-rnorm(100)
#' describe(x)
#' @export

describe<-function(x) {
	n <- length(na.omit(x))
	xbar <- mean(x, na.rm=T)
	md <- median(x, na.rm=T)
	s2 <- var(x, na.rm=T)
	s <- sqrt(var(x, na.rm=T))
	se <- s/sqrt(n)
	xmin <- min(x, na.rm=T)
	xmax <- max(x, na.rm=T)
	rng <- xmax - xmin
	cbind(n, xbar, md, s2, s, se, xmin, xmax, rng)
}



#' drops or keeps variables from a dataframe
#' @param dat A data frame
#' @param x A vector of variables to be dropped
#' @return A dataframe
#' @examples
#' data(mtcars)
#' dvars<-c("am", "cyl", "wt")
#' newdat<-dropvars(mtcars, dvars)
#' @export

dropvars<-function(dat, x) {
	xtmp <- names(dat) %in% c(x)
	dtmp<-dat[!xtmp]
	return(dtmp)
}

keepvars<-function(dat, x) {
  xtmp <- names(dat) %in% c(x)
  dtmp<-dat[xtmp]
  return(dtmp)
}



#' extract subtrings in a character vector from right
#' @param x A character vector
#' @param n integer, first element from right to be extracted
#' @return A character vector
#' @examples
#' substrRight("testing", 3)
#' @export

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#' global replace of values from a data frame
#' @param dat A data frame
#' @param from value to replace
#' @param to new value
#' @return A dataframe
#' @note All occurences of "from" or replaced with "to"
#' even partial strings or individual numbers within a numeric value
#' @examples
#' data(iris)
#' newdat<-replacedf(iris, "setosa", "set")
#' newdat<-replacedf(iris, 1.2, 1.25)
#' df<-data.frame(a=c(-99, 1, 2, 3, -99), b=c(1, 2, 3, -99, 4))
#' replacedf(df, -99, NA)
#' @export


replacedf<-function(dat, from, to) {
	data.frame(lapply(dat, function(x) {gsub(from, to, x)}))
	}



#' simple moving average
#' @param dat A numeric vector
#' @param lagdat number of lags for moving average
#' @param fill if TRUE fills in NAs to create of vector of original length (default FALSE)
#' @return A numeric vector of length length(dat)-(lagdat+1), or length(dat)
#' @note presently cannot handle NA or missing data 
#' @examples
#' x<-c(1:100)
#' mav2(x, 5)
#' mav2(x, 10, fill=TRUE)
#' @export

 mav2<-function(dat, lagdat, fill=FALSE) {
  mn<-NULL
  ln<-length(dat)
  
  for(i in lagdat:ln) {
    
    end<-i
    start<-i-(lagdat-1)
    mn[start]<-mean(c(dat[start:end]), na.rm=T)
    
  }
   if(fill==FALSE) {
    return(mn)
  }
  if(fill==TRUE) {
    mn<-c(rep(NA, lagdat-1), mn)
    return(mn)  
  }

}
 
   
 
 
