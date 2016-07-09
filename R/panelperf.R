panelperf <-function(data){
  p.value <- c()
  for (i in 1:ncol(data[,-c(1:3)])) {
    
    res.aovsummary<- summary(aov(data[,3+i]~Product*Session,data = data))
    
    p.value[i] <- round(res.aovsummary[[1]][[5]][1],2)
    
  }
  p.value
  
  colnames(data[,-c(1:3)])
  
  res.p <- data.frame(cbind(colnames(data[,-c(1:3)]),p.value))
  
  

}
