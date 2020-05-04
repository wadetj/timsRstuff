#' Tables with percentages
#'
#' Produces one, two or multi-way tables with percentages
#'
#' @param row factor or numeric vector which will appear in rows
#' @param col factor or numeric vector which will appear in columns
#' @param collab character vector of column names
#' @param tex output in tex format (TRUE/FALSE)
#' @param rowlab character vector of row names
#' @return r x c matrix
#' @examples
#' data(iris)
#' tabpct2(iris$Species)
#' iris$hilo<-cut(iris$Sepal.Width, breaks=2)
#' tabpct2(iris$Species, iris$hilo)
#' tabpct2(iris$Species, iris$hilo, collab=c("low", "Pct.", "hi", "Pct."))
#' tabpct2(iris$hilo, iris$Species, rowlab=c("low", "hi", "Total"), collab=c("set", "%", "vers", "%", "virg", "%"))
#' @export


tabpct2=function(row, col=NULL, collab="N", tex=FALSE, rowlab=NULL) {

  if(!is.null(col)) {

    t1<-table(row, col)
    ptot1<-round((apply(t1, 2, sum))/sum(apply(t1, 2, sum)), 4)*100
    pt1<-round(prop.table(t1, 1), 4)*100
    count1<-apply(t1, 2, sum)
    #Total=c(count1[1], ptot1[1], count1[2], ptot1[2])
    Total<-c((gdata::interleave(t(count1), t(ptot1))))
    #tab1=cbind(t1[,1], pt1[,1], t1[,2], pt1[, 2])
    tab1<-t(gdata::interleave(t(t1), t(pt1)))
    tab1<-rbind(tab1, Total)
    xcols<-dim(tab1)[2]/2
    #pval=chisq.test(row, col)$p.value


    if(length(collab)==1) {
      ns<-c(rep(collab, xcols))
      if(tex==FALSE){pcts=c(rep("%", xcols))}
      if(tex==TRUE) {pcts=c(rep("\\%", xcols))}
      xnames<-c(gdata::interleave(t(ns), t(pcts)))
      colnames(tab1)<-xnames
      }

    if(length(collab)>1) {colnames(tab1)=collab}


    #if(tex=="False"){colnames(tab1)=c(rowlab1, "%", rowlab2, "%")}

    #if(tex=="True") {colnames(tab1)=c(rowlab1, "\\%", rowlab2, "\\%")}

    if(!is.null(rowlab)){rownames(tab1)=rowlab}
    tab1
    return(tab1)
  }

    if(is.null(col)) {
      t1<-table(row)
    count1<-sum(t1)
    pt1<-round(prop.table(t1), 4)*100
    Total<-c(count1[1], 100)
    tab1<-cbind(t1, pt1)
    tab1<-rbind(tab1, Total)
    if(tex==FALSE) {colnames(tab1)=c(collab, "%")}
    if(tex==TRUE) {colnames(tab1)=c(collab, "\\%")}
    if(!is.null(rowlab)){rownames(tab1)=rowlab}
    tab1
    return(tab1)
  }
}
