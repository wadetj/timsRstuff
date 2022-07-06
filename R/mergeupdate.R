   
#goal- merge that updates values from dataset2 to dataset1
#if NA should keep existing value in dataset1
#variables not in dataset1 but in dataset2 should be added to dataset1
#seems to work- does not handle many to 1 merge
#need to add checks if ids are unique
#add merge type for 1:1 and 1:m




mergeupdate<-function(df1, df2, id, id1, id2, mergetype="1:1"){
 newdf<-df1
  
  if(!is.na(id)){
    id1=id
    id2=id
  }

 #does not seem to work
  #if(mergetype=="1:1") {
   if(anyDuplicated(id1)>0 | anyDuplicated(id2)>0) {
     stop("ids do not uniquely identify an observation")
   }
# }
 
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
 
if(length(update.vars)>0 ) {   
  for(i in 1:length(update.vars)) {
    #varname<-paste(update.vars[i])
    #newdf[,varname][match(newdf[,id1], df2[,id2])]<-df2[,varname]
    newdf[,update.vars[i]][match(newdf[,id1], df2[,id2])]<-df2[,update.vars[i]]
    newdf[, update.vars[i]][match(newdf[,id1], df2[,id2]) & is.na(df2[, update.vars[i]])]<-df1[, update.vars[i]][match(newdf[,id1], df2[,id2]) & is.na(df2[, update.vars[i]])]
    
  }
}
  
  
  
 
  #  for(i in 1:length(df1only)) {
  #   varname<-paste(df1only[i])
  #   newdf[,varname]<-df1[,varname]
  #   
  # }

  
if(length(df2only)>0){    
  for(i in 1:length(df2only)) {
    varname<-paste(df2only[i])
    newdf[,varname]<-NA
    newdf[,varname][match(newdf[,id1], df2[,id2])]<-df2[,varname][match(newdf[,id1], df2[,id2])]
  }
}

  return(newdf)
  
}


data(iris)
iris2<-iris
iris$id<-1:dim(iris)[1]
iris2<-iris
iris2$Sepal.Length<-rnorm(150)
iris2$Petal.Width[25:75]<-rnorm(51)
iris2$color="red"
iris$height="tall"
xx<-mergeupdate(iris, iris2, id="id")

# 
# dumb<-function(a, b, c) {
#   if(a!=10) stop("a must equal 10")
#   print("yes a=10")               
# }
# 
#   
