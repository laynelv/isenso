panelperf <-function(data){
  p.value <- c()
  for (i in 1:ncol(data[,-c(1:3)])) {
    
    res.aovsummary<- summary(aov(data[,3+i]~Product*Session,data = data))
    
    p.value[i] <- round(res.aovsummary[[1]][[5]][1],2)
    
  }
  p.value
  
  notsig <- p.value>=0.05
  attribute <- colnames(data[,-c(1:3)])

  
  res.p <- data.frame(cbind(attribute,p.value,notsig))
   order <- as.integer(rownames(res.p))
  res.p2 <- data.frame(cbind(res.p,order))
  #res.p$order <- as.numeric(res.p$order)
  require(ggplot2)
  g <- ggplot(res.p2,aes(x=order,y=p.value))
  
  g1 <- g+geom_bar(stat = "identity",aes(fill=notsig),width = 0.8)+geom_text(aes(label =p.value, vjust = 1.1, hjust = 0.5),color="grey30",size = 3.5)+theme(axis.text.x = element_text(size = 10, color = "grey20", angle = 45))+scale_fill_manual(values = c('lightseagreen','lightcoral'))+scale_x_continuous(breaks = res.p2$order,labels =res.p2$attribute)+labs(list(title = "Panel Performance (P.value<0.05)", y = "P.value", x = " "))+ guides(fill = "none")

  return(list(g1,res.p))
}
