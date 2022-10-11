drugsfunction<-function(address){
 
 
  datalist = NULL
  
  list_of_files <- list.files(path = address,
                              pattern = "doc",full.names = T)
 
  for (i in 1:length(list_of_files)) {
    datet<-tools::file_path_sans_ext(basename(list_of_files[i]))
   
   
    z<-readtext(list_of_files[i])
    z<-z[,2]
    x<-medExtractR(z,druglist1,90,'mg',0)
   
    ts<- as.data.frame(x)
 
     if(nrow(ts)>2){
      ts$a1<- 1
      for(b in 2:nrow(ts)){
       
        if(ts$entity[b] == 'DrugName'){
          ts$a1[b]<-ts$a1[b-1]+1
        }
        else{
          ts$a1[b]<-ts$a1[b-1]
        }
      }
      t<-ts[,c(1,2,4)]
     
      xt<-t%>%pivot_wider(names_from = entity , values_from = expr)
      xt$Date<-datet
     
     
    }
   
  datalist[[i]]<-xt
  }
 
 
 
  return(datalist)
 
}






cols <- unique(unlist(sapply(datalist, names)))

Y<-do.call(rbind, lapply(datalist, function(tis) {
 
  tis[setdiff(cols, names(tis))] <- NA
  tis <- data.frame(tis)
}))

return(Y)

}
