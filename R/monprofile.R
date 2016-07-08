
#' @export
monprofile <- function(data = NULL, prod = 1, firsta = 4,ncluster=3,graph="text") {

    if (prod == 0) {
        product <- rownames(data)
    } else {
        product <- data.frame(table(data[, prod]))
        product <- as.character(product$Var1)
    }

    taster <- data.frame(table(data[, 2]))
    taster <- as.character(taster$Var1)
    ## line plot of products'mean and attributes

    if (firsta == 1) {
        attribute <- colnames(data)
    } else {
        attribute <- colnames(data[, -c(1:firsta - 1)])
    }

    res.mean <- list()
    for (i in 1:length(product)) {
        datalist <- split(data, data[, prod] == product[i])
        mean <- apply(datalist[[2]][, -c(1:firsta - 1)], 2, function(v) mean(v, na.rm = T))
        order <- as.integer(c(1:length(attribute)))
        res.mean[[i]] <- cbind(rep(product[i], length(attribute)), attribute, data.frame(mean),order)
    }
    res.mean <- do.call("rbind", res.mean)  ##堆叠for loop's list数据,生成新数据框
    colnames(res.mean) <- c("product", "attribute", "mean", "order")
    rownames(res.mean) <- c(1:nrow(res.mean))

    if (any(is.na(res.mean))) {
        res.mean$mean[which(is.na(res.mean$mean))] <- 0
    }
    require(ggplot2)
    p <- ggplot(res.mean, aes(x = order, y = mean, group = product, color = product))
    p1 <- p + geom_line(stat = "identity", size = 1.2)
    p2 <- p1 + scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) + scale_x_continuous(breaks = res.mean$order,labels = res.mean$attribute) + theme_bw()
    # scale_fill_manual(values = c('dodgerblue1','firebrick1'))
    p3 <- p2 + theme(axis.text.x = element_text(size = 9, color = "grey20", angle = 45)) +labs(list(title = "Monadic Profile", y = " ", x = " "))
    ## PCA biplot of products and attributes

    require(factoextra)

    if (any(is.na(data[, -c(1:firsta - 1)]))) {
        return(list(p3, res.mean))
        print("There are NAs in the dataframe!")
    } else {
        res.mean2 <- data.frame(matrix(NA, length(attribute), length(product)))
        for (i in 1:length(product)) {
            datalist <- split(data, data[, prod] == product[i])
            mean2 <- apply(datalist[[2]][, -c(1:firsta - 1)], 2, function(v) mean(v,na.rm = T))
            res.mean2[, i] <- data.frame(mean2)$mean2
        }
        res.mean2 <- data.frame(res.mean2)
        colnames(res.mean2) <- product
        rownames(res.mean2) <- attribute
        res.pca <- prcomp(res.mean2,scale. = T)
        km.res <- kmeans(res.mean2,ncluster)
        res.mean2$cluster <- data.frame(km.res[1])$cluster
        res.biplot <- fviz_pca_biplot(res.pca, axes = c(1, 2), geom = graph,
            label = c("ind","var"), invisible = "none", labelsize = 4, pointsize = 3, habillage = res.mean2$cluster,addEllipses = F, ellipse.level = 0.95, col.ind = "black", col.ind.sup = "blue",alpha.ind = 1, col.var = "dodgerblue3", alpha.var = 1, col.quanti.sup = "blue", col.circle = "grey70", repel = FALSE, axes.linetype = "dashed", select.var = list(name = NULL,cos2 = NULL, contrib = NULL), select.ind = list(name = NULL, cos2 = NULL,contrib = NULL), jitter = list(what = "label", width = NULL, height = NULL))
        return(list( res.biplot + theme_bw(), p3,res.mean2))
    }
}
