tasterperf2 <- function(data=NULL,ntaster){
  
  taster <- data.frame(table(data[,2]))
  attribute <- colnames(data[,-c(1:3)])
  ##judge if ntaster is a number,then subset the data
  if (is.character(ntaster)==TRUE) {
    indvidual <- subset(data,data[,2]==ntaster)
    others <- subset(data,data[,2]!=ntaster)
  } else{
    indvidual <- subset(data,data[,2]==taster$Var1[ntaster])
    others <- subset(chocolate,data[,2]!=taster$Var1[ntaster])
  }
  res.aov <- apply(indvidual[,-c(1:3)],2, function(v) aov(v~Product,data=indvidual))
  
  res.summaryaov <- sapply(res.aov, summary)
  ##extract F-values
  F.value <- c()
  for (i in 1:length(attribute)) {
        F.value[i] <- round(res.summaryaov[[i]][4][[1]][1],2)
  }
  ##discrimination is OK,if F-value >=2
  dis<- F.value>=2
  res.F <- data.frame(cbind(attribute,F.value,dis))
  
  ##individual consistency
  res.others <- aggregate(.~others[,1],data=others[,-c(2,3)],mean)
  res.others <- res.others[,-2]
  
  res.ind <- aggregate(.~indvidual[,1],data=indvidual[,-c(2,3)],mean)
  res.ind <- res.ind[,-2]
  
  res.rvalue <- c()
  for (i in 1:length(attribute)) {
    res.rvalue[i] <- cor(res.ind[,i+1],res.others[,i+1],method = "pearson")
  }
  isOK <- res.rvalue>=0.8
  res.cor <- data.frame(cbind(attribute,round(res.rvalue,2),isOK))
  
  
  method <- c(rep("F.value",length(attribute)),rep("r.correlation",length(attribute)))

  require(ggplot2)
  
  if (is.character(ntaster)==TRUE) {
  colnames(res.F) <- c("attribute","value",ntaster)
  
  colnames(res.cor) <- c("attribute","value",ntaster)
  
  res.tasterperf <- data.frame(cbind(rbind(res.F,res.cor),method))
  
  res.tasterperf$value <- as.numeric(as.character(res.tasterperf$value))
  
  res.tasterperf$order <- as.numeric(as.character(c(1:length(attribute),1:length(attribute))))
  
  
  p <- ggplot(res.tasterperf,aes(order,value))
  p1 <- p+geom_bar(stat = "identity",aes(fill=res.tasterperf[,3]),width = 0.8)+geom_text(aes(label =value, vjust = 1.1, hjust = 0.5),color="grey30",size = 3.5)+theme(axis.text.x = element_text(size = 10, color = "grey30", angle = 45))+scale_x_continuous(breaks=res.tasterperf$order,labels =res.tasterperf$attribute)+labs(list(title = "Individual discrimination ability & agreement (F >=2,r >=0.8)", y = " ", x = " "))+facet_grid(method ~ ., scales = "free_y",labeller = label_parsed,switch="y")+ scale_fill_discrete(name = ntaster)
  
  }else{
    colnames(res.F) <- c("attribute","value",as.character(taster$Var1)[ntaster])
    
    colnames(res.cor) <- c("attribute","value",as.character(taster$Var1)[ntaster])
    
    res.tasterperf <- data.frame(cbind(rbind(res.F,res.cor),method))
    
    res.tasterperf$value <- as.numeric(as.character(res.tasterperf$value))
    res.tasterperf$order <- as.numeric(as.character(c(1:length(attribute),1:length(attribute))))
    
    p <- ggplot(res.tasterperf,aes(order,value))
    p1 <- p+geom_bar(stat = "identity",aes(fill=res.tasterperf[,3]),width = 0.8)+geom_text(aes(label =value, vjust = 1.1, hjust = 0.5),color="grey30",size = 3.5)+theme(axis.text.x = element_text(size = 10.5, color = "grey30", angle = 45))+scale_x_continuous(breaks=res.tasterperf$order,labels =res.tasterperf$attribute)+labs(list(title = "Individual discrimination ability & agreement (F >=2,r >=0.8)", y = " ", x = " "))+facet_grid(method ~ ., scales = "free_y",labeller = label_parsed,switch="y")+ scale_fill_discrete(name = as.character(taster$Var1)[ntaster])
    
  }
  
  return(list(p1,res.tasterperf))
}