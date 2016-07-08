
#' @export
fprofile <- function(data=NULL){

  rownames(data) <- data[,1]
  data <- data[,-1]
  res.pca <- prcomp(data[,-1],scale. = T)

  require(factoextra)

  res.var <- fviz_pca_var(res.pca, geom = c("arrow", "text"), label = "all",invisible = "none", repel = FALSE, labelsize = 4, col.var = "steelblue",alpha.var = 1, col.quanti.sup = "blue", col.circle = "grey70")+theme_bw()

  res.biplot <- fviz_pca_biplot(res.pca, axes = c(1, 2), geom = c( "text"),label = c("ind", "var" ), invisible = "none", labelsize = 4, pointsize = 3, habillage = data$panelist, addEllipses = F, ellipse.level = 0.95, col.ind = "black", col.ind.sup = "blue",alpha.ind = 1, col.var = "steelblue", alpha.var = 1, col.quanti.sup = "blue", col.circle = "grey70", repel = FALSE, axes.linetype = "dashed", select.var = list(name = NULL, cos2 = NULL, contrib = NULL), select.ind = list(name = NULL, cos2 = NULL,  contrib = NULL), jitter = list(what = "label", width = NULL, height = NULL))+theme_bw()
 return (list(res.var,res.biplot))
}
