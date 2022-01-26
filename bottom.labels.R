bottom.labels <- function(df){
  rbind(
    cl<-10,
    l<-matrix(nrow=(ceiling(length(df[,1])/cl))*4,ncol=cl,byrow=FALSE,dimnames=NULL),
    k<-1,
    for (j in ((c(1:(length(l[,1])/4))*4)-3)){
      for (i in c(1:cl)) {
        l[j,i] <- paste (df[k,5],df[k,3],df[k,4],df[k,11]) #site, (Transect Segment), plant
        l[j+1,i] <- paste (df[k,6],"N",",",df[k,7],"W","Elev:",df[k,10],"m") #coordinates, elevation
        l[j+2,i] <- paste ("RMBL","Gothic, CO USA") #RMBL, County Info
        l[j+3,i] <- paste (df[k,2],"col.:",df[k,12],df[k,8]) #date project collector specimen number
        
        if (df[k,3] == "") l[j,i] <- paste (df[k,5],",",df[k,8])
        
        if (df[k, 12] == "") l[j+3,i] <- paste(df[k,2],",",df[k,11],",","col.: B. Brosi")
        
        if (df[k,6] == "") l[j+1,i] <- ("ENTER GPS DATA")
        
        if (k < length(df[,1])) k<-k+1
        
      }
    },
    
    write.table(l, file = "gradients_bottom_labels.csv", sep = ",", col.names = F, row.names = F, append=FALSE)
  )
}