determination.label <- function(df, id, authority, det, year.det){
  rbind(
    cl <- 11,
    l<-matrix(nrow=(ceiling(length(df[,1])/cl))*2,ncol=cl),
    k<-1,
    for (j in ((c(1:(length(l[,1])/2))*2)-1)){
      for (i in c(1:cl)) {
        l[j,i] <- paste (df[k,id], paste("(", df[k, authority], ")", sep="")) #genus and species (and authority)

        l[j+1,i] <- paste ("det.",df[k,det]," ",df[k,year.det]) #det. [name of expert who identified] [year identified]
        
        
        if (k < length(df[,1])) k<-k+1
        
      }
    },
    return(l)
  )
}
