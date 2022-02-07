locality.label <- function(df, spec.num, country, state, county, geo.general, geo.spec, date, col){
  # first have the specimens ordered by specimen #
  df = df %>% 
    arrange(spec.num)
  
  rbind(
    cl<-10,
    l<-matrix(nrow=(ceiling(length(df[,1])/cl))*4,ncol=cl,byrow=FALSE,dimnames=NULL),
    k<-1,
    for (j in ((c(1:(length(l[,1])/4))*4)-3)){
      for (i in c(1:cl)) {
        
        l[j,i] <- paste (df[k,country],df[k,state],df[k,county]) ## Country, state (territory, province), and county (township) 
        l[j+1,i] <-paste(df[k,geo.general])
        l[j+2,i] <-  paste (df[k,geo.spec]) #coordinates or other specific geographic location
        l[j+3,i] <- paste (df[k,date], "col.",df[k,col]) #date project collector specimen number
        
        
        if (k < length(df[,1])) k<-k+1
        
      }
    },
    return(l),
    write.table(l, file = "locality_label.csv", sep = ",", col.names = F, row.names = F, append=FALSE)
  )
}