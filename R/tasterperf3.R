tasterperf <- function(data=NULL,ntaster){

taster <- data.frame(table(data[,2]))
attribute <- colnames(data[,-c(1:3)])
##judge if ntaster is a number
if (is.character(ntaster)==TRUE) {
  indvidual <- subset(data,Panelist==ntaster)
} else{
  indvidual <- subset(data,Panelist==taster$Var1[ntaster])
}

res.aov <- apply(indvidual[,-c(1:3)],2, function(v) aov(v~Product,data=indvidual))

res.F <- sapply(res.aov, summary)

##extract F-values
F.value <- c()
for (i in 1:length(attribute)) {
  
  F.value[i] <- round(res.F[[i]][4][[1]][1],2)
}
##discrimination is OK,if F-value >=2
dis<- F.value>=2

res.F2 <- data.frame(cbind(attribute,F.value,dis))
order <- as.integer(rownames(res.F2))
res.F3 <- data.frame(cbind(res.F2,order))
##continious or discrete
res.F3$F.value <- as.numeric(as.character(res.F3$F.value))

if (is.character(ntaster)==TRUE) {
  colnames(res.F2) <- c("attribute","F.value",ntaster)
  
  g <- ggplot(res.F3,aes(x=order,y=F.value))
  
  g1 <- g+geom_bar(stat = "identity",aes(fill=res.F2[,3]),width = 0.8)+geom_text(aes(label =F.value, vjust = 1.1, hjust = 0.5),color="grey30",size = 3.5)+ theme(axis.text.x = element_text(size = 10, color = "grey20", angle = 45))+scale_x_continuous(breaks = res.F3$order,labels =res.F3$attribute) +labs(list(title = "Individual discrimination ability (F.value>=2) ", y = "F.value", x = " "))+ scale_fill_discrete(name = ntaster)
} else{
  colnames(res.F2) <- c("attribute","F.value",as.character(taster$Var1)[ntaster])
  
  g <- ggplot(res.F3,aes(x=order,y=F.value))
  
  g1 <- g+geom_bar(stat = "identity",aes(fill=res.F2[,3]),width = 0.8)+geom_text(aes(label =F.value, vjust = 1.1, hjust = 0.5),color="grey30",size = 3.5)+ theme(axis.text.x = element_text(size = 10, color = "grey20", angle = 45))+scale_x_continuous(breaks = res.F3$order,labels =res.F3$attribute) +labs(list(title = "Individual discrimination ability (F.value>=2) ", y = "F.value", x = " "))+ scale_fill_discrete(name = as.character(taster$Var1)[ntaster])
}

return(list(g1,res.F2))



}
