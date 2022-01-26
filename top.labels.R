top.labels <- function(df){
  rbind(
    cl <- 11,
    l<-matrix(nrow=(ceiling(length(df[,1])/cl))*2,ncol=cl),
    k<-1,
    for (j in ((c(1:(length(l[,1])/2))*2)-1)){
      for (i in c(1:cl)) {
        l[j,i] <- paste ("RMBL",df[k,1],"#",df[k,8]) #RMBL Year #specimen number
        l[j+1,i] <- paste ("") #blank row for formatting
        
        if ( df[k,8] == "") l[j,i] <- paste ("RMBL",df[k,1])
        
        if (k < length(df[,1])) k<-k+1
        
      }
    },
    
    write.table(l, file = "gradients_top_labels.csv", sep = ",", col.names = F, row.names = F, append=FALSE)
  )
}