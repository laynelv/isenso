
#' @export
sorting <- function(data=NULL){

  rownames(data) <- data[,1]
  data <- data[,-1]
  res.pca <- prcomp(data[,-1],scale. = TRUE)

  require(factoextra)
  res.biplot <- fviz_pca_biplot(res.pca, axes = c(1, 2), geom = c( "text"),label = "all", invisible = "none", labelsize = 4, pointsize = 3, habillage = "none", addEllipses = F, ellipse.level = 0.95, col.ind = "dodgerblue3", col.ind.sup = "blue",alpha.ind = 1, col.var = "firebrick1", alpha.var = 1, col.quanti.sup = "blue", col.circle = "grey70", repel = FALSE, axes.linetype = "dashed", select.var = list(name = NULL, cos2 = NULL, contrib = NULL), select.ind = list(name = NULL, cos2 = NULL,  contrib = NULL), jitter = list(what = "label", width = NULL, height = NULL))
  return (res.biplot+theme_bw())
}
