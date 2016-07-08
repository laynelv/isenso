#' @export
comprofile <- function(data = NULL, firsta = 4) {
    data <- data[, -c(1:firsta - 1)]
    N <- c()
    mean <- c()
    sd <- c()
    for (i in 1:ncol(data)) {
        N[i] <- length(data[, i])
        mean[i] <- mean(data[, i])
        sd[i] <- sd(data[, i])
    }
    if (sum(is.na(data) == 0)) {
        dat2 <- rbind(N, mean)
        dat3 <- rbind(dat2, sd)
    } else {
        print("there are NAs, check your data")
    }
    ci <- 2 * sd/sqrt(N)  # calculate 95% confidence interval ci,

    dat4 <- rbind(dat3, ci)
    ## add a column of attributes
    dat4 <- round(dat4, 3)
    attribute <- as.character(colnames(data))
    dat5 <- rbind(attribute, dat4)
    cp2 <- data.frame(t(dat5))
    cp2[, 2:ncol(cp2)] <- lapply(cp2[, 2:ncol(cp2)], as.character)
    cp2[, 2:ncol(cp2)] <- lapply(cp2[, 2:ncol(cp2)], as.numeric)
    require(ggplot2)

    cp2 <- transform(cp2, Significant = ifelse(mean - ci <= 0 & mean + ci >= 0, no = "Yes",
        yes = "No"))  #分开正负,判断是否显著差异

    cp2$order <- as.integer(rownames(cp2))
    p <- ggplot(cp2, mapping = aes(x = order, y = mean, fill = Significant))
    p1 <- p + geom_bar(stat = "identity", position = "identity", width = 0.4)
    p2 <- p1 + geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.9),
        color = "grey30")

    p3 <- p2 + scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) + scale_x_reverse(breaks = cp2$order,labels = cp2$attribute) + scale_fill_manual(values = c("dodgerblue1", "firebrick1"))
    p4 <- p3 + theme(axis.text.y = element_text(size = 8, color = "black")) + labs(list(title = "Comparative Profile (Sample vs REF)", y = " ", x = " "))
    p5 <- p4 + coord_flip() + theme_bw()
    p6 <- p5 + theme(legend.justification = "right", legend.position = "right", legend.key.size = unit(5, "mm"))

    return(list(p6, cp2))
}
