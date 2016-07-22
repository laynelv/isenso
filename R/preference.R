
#' @export
preference <- function (data=NULL,ncluster=3){
  require(ggplot2)
  require(factoextra)
 ## k-means cluster and biplot
   data <- na.omit(data)
   rownames(data) <- data[,1]
   data <- data[,-1]
   kmeans <- kmeans(data,ncluster)
   cluster <- kmeans$cluster
   data2 <- data.frame(cbind(data,cluster))

   res.pca <- prcomp(data2[,-ncol(data2)],scale. = T)

res.biplot <- fviz_pca_biplot(res.pca, axes = c(1, 2), geom = c( "point"),label = c("ind", "var" ), invisible = "none", labelsize = 4, pointsize = 2, habillage = data2$cluster, addEllipses = F, ellipse.level = 0.95, col.ind = "black", col.ind.sup = "blue",alpha.ind = 1, col.var = "blue", alpha.var = 1, col.quanti.sup = "blue", col.circle = "grey70", repel = FALSE, axes.linetype = "dashed", select.var = list(name = NULL, cos2 = NULL, contrib = NULL), select.ind = list(name = NULL, cos2 = NULL,  contrib = NULL), jitter = list(what = "label", width = NULL, height = NULL))+theme_bw()

## means score and LSD comparation, barplot

scores <- data.frame(stack(data.frame(na.omit(data))))
model<-aov(values~ind, data=scores)
out <- LSD.test(model,"ind", p.adj="bonferroni")

order <- rownames(out$groups)
res.lsd <- cbind(out$groups,order)

g <- ggplot(res.lsd,aes(order,means))+geom_bar(aes(fill=M),stat = "identity",width =0.6)+geom_text(aes(label = round(out$groups$means,2), vjust = 1.1, hjust = 0.5),color="black",size = 4)+scale_y_continuous(breaks=seq(min(round(out$groups$means,2)),max(round(out$groups$means,2)),round(out$statistics$LSD,2)))+scale_x_discrete(labels = res.lsd$trt)+ guides(fill = "none")+theme_bw()+labs(title=paste("Mean scores and multiple comparation. LSD=",round(out$statistics$LSD,2)),y=" ",x=" ")
return(list(res.biplot,g,round(out$statistics,2)))
}
