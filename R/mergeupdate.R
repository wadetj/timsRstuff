   
#goal- merge that updates values from dataset2 to dataset1
#if NA should keep existing value in dataset1
#variables not in dataset1 but in dataset2 should be added to dataset1
#seems to work- does not handle many to 1 merge
#need to add checks if ids are unique
#add merge type for 1:1 and 1:m (df1- single observation, df2, multiple obs)
#1 to many merges expands original dataset(df1), creates new id and duplicates
#values for which there are no values in second dataset
#also 1:m resorts dataset based on id

mergeupdate<-function(df1, df2, id, id1, id2, mergetype="1:1"){
 newdf<-df1
 
  if(!is.na(id)){
    id1=id
    id2=id
  }

  if(mergetype=="1:1") {
   if(anyDuplicated(df1[, id1])>0 | anyDuplicated(df2[, id2])>0) stop("ids do not uniquely identify an observation. Use mergetype='1:m' for many to one merge")
  }
 
 id1x<-paste0("^", id1, "$")
 id2x<-paste0("^", id1, "$")
 
  df1.vars<-names(df1)
  df2.vars<-names(df2)
  
  xx<-grep(id1x, df1.vars)
  df1.vars<-df1.vars[-xx]
  xx<-grep(id2x, df2.vars)
  df2.vars<-df2.vars[-xx]
  update.vars<-df1.vars[df1.vars %in% df2.vars]
  df1only<-df1.vars[!df1.vars %in% df2.vars]
  df2only<-df2.vars[!df2.vars %in% df1.vars]
 
  #creates new id for multiple observations
  #expands original dataset 
  
  if(mergetype=="1:m") {
    ord2<-order(df2[,id2])
    df2<-df2[ord2,]
    ord1<-order(newdf[,id1])
    newdf<-newdf[ord1, ]
    xtemp<-rle(df2[,id2])
    times<-xtemp$lengths
    newid<-sequence(xtemp$lengths)
    df2newid<-paste0(df2[,id2], ".",  newid)
    newdf<-as.data.frame(lapply(newdf, rep, times))
    df2[,id2]<-df2newid
    newdf[,id1]<-df2newid
  }
  
if(length(update.vars)>0 ) {   
  for(i in 1:length(update.vars)) {
    newdf[,update.vars[i]][match(newdf[,id1], df2[,id2])]<-df2[,update.vars[i]]
    newdf[, update.vars[i]][match(newdf[,id1], df2[,id2]) & is.na(df2[, update.vars[i]])]<-df1[, update.vars[i]][match(newdf[,id1], df2[,id2]) & is.na(df2[, update.vars[i]])]
    
  }
}

if(length(df2only)>0){    
  for(i in 1:length(df2only)) {
    varname<-paste(df2only[i])
    newdf[,varname]<-NA
    newdf[,varname][match(newdf[,id1], df2[,id2])]<-df2[,varname][match(newdf[,id1], df2[,id2])]
  }
}

  return(newdf)
  
}

# 
# data(iris)
# iris2<-iris
# iris$id<-1:dim(iris)[1]
# iris2<-iris
# iris2$Sepal.Length<-rnorm(150)
# iris2$Petal.Width[25:75]<-rnorm(51)
# iris2$color="red"
# iris$height="tall"
# xx<-mergeupdate(iris, iris2, id="id")
# 
# data1<-data.frame(id=rep(1:10), x=1, y=10*runif(10), z=10)
# data3<-data.frame(id=rep(1:10, 3), z=rnorm(30), y=100*runif(30))
# mergeupdate(data1, data3, id="id", mergetype="1:m")
# 

