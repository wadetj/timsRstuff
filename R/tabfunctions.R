#' Tables with percentages
#'
#' Produces one, two or multi-way tables with percentages. Requires gdata::interleave
#' See tabpct3 for additional flexibility
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


#' Tables with percentages
#'
#' Produces one, two or multi-way tables with percentages. Requires gdata::interleave.
#' For latex, html output, object can be formatted with knitr::kable
#' @param row factor or numeric vector which will appear in rows
#' @param col factor or numeric vector which will appear in columns
#' @param collab character vector of column names
#' @param rowlab character vector of row names
#' @param pcts character string 'both', 'row', or 'col' indicating row, column or both percents (default='col')
#' @param dp integer indicating number of decimal places for percents (default=2)
#' @param total Logical indicating whether row and column totals are shown (default= TRUE)
#' @param pval character string indicating 'exact', 'chisq' or 'none' (default='exact') 
#' @return r x c matrix
#' @examples
#' library(knitr)
#' yn<-sample(c("Y", "N"), 100, replace=TRUE)
#' sex<-sample(c("M", "F"), 100, replace=TRUE)
#' tabpct3(yn)
#' tabpct3(yn, sex)
#' tabpct3(yn, sex, dp=1, pcts="row", rowlab=c("No", "Yes"), collab=c("Female", "Male"))
#' tabpct3(yn, sex, dp=1, pcts="col", rowlab=c("No", "Yes"), collab=c("Female", "Male"))
#' xx<-tabpct3(yn, sex, dp=1, pcts="col", rowlab=c("No", "Yes"), collab=c("Female", "Male"))
#' kable(xx, format="latex", caption="My Table")
#' kable(xx, format="simple", caption="My Table")
#' ynm<-sample(c("y", "n", "m"), 312, replace=TRUE)
#' dnum<-sample(c(1:5), 312, replace=TRUE)
#' tabpct3(dnum, ynm, collab=c("Maybe", "No", "Yes"), pval="chisq")
#' @export


tabpct3=function(row, col=NULL, collab=NULL,  rowlab=NULL, pcts="col", dp=2, total=TRUE, pval="exact") {
  if(!is.null(col)) {
    t1<-table(row, col)
    rowtot<-apply(t1, 1, sum)
    #coltot<-apply(t1, 2, sum)
    #rowptot<-round((coltot/tot)*100, dp)
    #fill in 100 %s
    #colptot<-round((rowtot/rowtot)*100, dp)
    tot<-sum(rowtot) 
    if(!is.null(collab)) colnames(t1)<-c(collab)
    if(!is.null(rowlab)) rownames(t1)<-c(rowlab)
   #reverse table for interleave
    rt1<-t(t1)
  #cols and rows are backwards from what is wanted  
    rowp<-round(prop.table(rt1, 2)*100, dp)
    colp<-round(prop.table(rt1, 1)*100, dp)
    if(pcts=="both") {
      rownames(rowp)<-c(rep("Row %", dim(rowp)[1]))
      rownames(colp)<-c(rep("Col %", dim(colp)[1]))
      tx<-gdata::interleave(rt1, rowp, colp)
      #txt<-kable(t(tx), format="simple")
      txt<-t(tx)
    
      if(total==TRUE){
        txt<-cbind(txt, rowtot)
        coltot<-apply(txt, 2, sum)
        txt<-rbind(txt, coltot)
        ncols<-dim(txt)[2]
        nrows<-dim(txt)[1]
        #fill in correct row pctsa
        for(i in seq(2, (ncols-2), by=3)) {
          txt[nrows, i]<-round(100*(txt[nrows, i-1]/txt[nrows, ncols]), dp)
        }
        
        colnames(txt)[ncols]<-"Total"
        rownames(txt)[nrows]<-"Total"
        #correct total row
        
        
        }

    }
    if(pcts=="row") {
      rownames(rowp)<-c(rep("%", dim(rowp)[1]))
      tx<-gdata::interleave(rt1, rowp)
      #txt<-kable(t(tx), format="simple")
      txt<-t(tx)
      
      if(total==TRUE){
        #txt<-cbind(txt, rowtot)
        coltot<-apply(txt, 2, sum)
        txt<-rbind(txt, coltot)
        #colnames(txt)[dim(txt)[2]]<-"Total"
        #fill in correct row pcts
        ncols<-dim(txt)[2]
        nrows<-dim(txt)[1]
        for(i in seq(2, (ncols), by=2)) {
          txt[nrows, i]<-round(100*((txt[nrows, i-1])/tot), dp)
        }
        
        rownames(txt)[nrows]<-"Total"
        totcol<-c(rowtot, tot)
        txt<-cbind(txt, totcol)
        colnames(txt)[dim(txt)[2]]<-"Total"
      }
    }
    if(pcts=="col") {
      rownames(colp)<-c(rep("%", dim(colp)[1]))
      tx<-gdata::interleave(rt1, colp)
      #txt<-kable(t(tx), format="simple")
      txt<-t(tx)
        #txt<-kable(t(tx), format="simple")
        txt<-t(tx)
        
        if(total==TRUE){
          txt<-cbind(txt, rowtot)
          coltot<-apply(txt, 2, sum)
          txt<-rbind(txt, coltot)
          colnames(txt)[dim(txt)[2]]<-"Total"
          rownames(txt)[dim(txt)[1]]<-"Total"
          
        }
        
    }
  if(pval=="exact") {
    epval<-round(fisher.test(t1)$p.value, 4)
    prow<-c(rep("", ncol(txt)-1), epval)
    txt_df<-rbind.data.frame(txt, prow)
    row.names(txt_df)[nrow(qq)]<-"exact p-value"
    return(txt_df)
  }
    if(pval=="chisq") {
      cpval<-round(chisq.test(t1)$p.value, 4)
      prow<-c(rep("", ncol(txt)-1), cpval)
      txt_df<-rbind.data.frame(txt, prow)
      row.names(txt_df)[nrow(qq)]<-"chisq p-value"
    return(txt_df)  
    }   
    
  if(pval=="none") {
    return(txt)
  }
}
  

  if(is.null(col)) {
      t1<-table(row)
      count1<-sum(t1)
      pt1<-round((prop.table(t1)*100), dp)
      Total<-c(count1[1], 100)
      tab1<-cbind(t1, pt1)
      tab1<-rbind(tab1, Total)
      if(is.null(collab)) colnames(tab1)=c("N", "%")
      if(!is.null(collab)) colnames(tab1)=c(collab, "%")
      if(!is.null(rowlab)) rownames(tab1)=rowlab
      tab1
  }
}

