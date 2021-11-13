#' title: plots.R
#' comments: plotting functions for various data types
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(ggplot2)
library(gridExtra)

#' source
source("cluster.R")

#' functions
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_cluster_series <- function(x, series, preproc = FALSE, centroids = FALSE, ...)
{
    #' Given the series, will plot then based on the orders of the cluster
    #' object.

    if (preproc) {
        #' Preprocess the series data with the cluster function
        series <- clus_preproc(series, x)
    }

    if (centroids) {
        #' Force the centroids of the cluster to be mv
        x@centroids <- lapply(x@centroids, function(x) cbind(x, numeric(length(x))))
    }

    plot(x, type = "sc", series = series, ...)
}


plot_mult_xts <- function(..., main = NULL, xlab = NULL, ylab = NULL, legend.names = NULL, col = NULL)
{
    #' Plots multiple xts series together
    if (length(list(...)) == 1 && is.list(...)) {
        data <- Reduce(merge, ...)
    } else {
        data <- merge(...)
    }

    plot(data, main = main, xlab = xlab, ylab = ylab)
    addLegend(legend.names = legend.names, col = col, lty = 1, bty = "o")
}


plot.ErrorModel <- function(mod, main = NULL, xlab = NULL, ylab = NULL, q_opt = TRUE)
{
    ## Creating the necessary data structures
    agg_level <- seq_along(mod$SMA$error)
    sma_data <- data.frame(y = mod$SMA$error, x = agg_level, size = 1)
    sma_data$size[[mod$SMA$opt]] <- 2  # The size doesn't actually matter since it's relative
    ema_data <- data.frame(y = mod$EMA$error, x = agg_level, size = 1)
    ema_data$size[[mod$EMA$opt]] <- 2

    annotations <- data.frame(text = c(mod$SMA$opt, mod$EMA$op),
                              x = c(mod$SMA$opt, mod$EMA$opt),
                              y = c(mod$SMA$error[[mod$SMA$opt]], mod$EMA$error[[mod$EMA$opt]]))


    if (q_opt) {
        sma_data$size[[mod$SMA$q_opt]] <- 2
        ema_data$size[[mod$EMA$q_opt]] <- 2
        foo <- data.frame(text = c(mod$SMA$q_opt, mod$EMA$q_opt),
                          x = c(mod$SMA$q_opt, mod$EMA$q_opt),
                          y = c(mod$SMA$error[[mod$SMA$q_opt]], mod$EMA$error[[mod$EMA$q_opt]]))
        annotations <- rbind(annotations, foo)
    }

    ggplot(NULL) +
        geom_point(data = sma_data, aes(y = y, x = x, color = "SMA", size = size), show.legend = F) +
        geom_point(data = ema_data, aes(y = y, x = x, color = "EMA", size = size), show.legend = F) +
                                        #geom_line(aes(y = mod$Croston, x = agg_level, color = "Croston"), size = 1.5) +
        geom_line(aes(y = mod$AGG, x = agg_level, color = "AGG"), size = 1.5) +
        geom_line(aes(y = mod$ASACT, x = agg_level, color = "ASACT"), size = 1.5, linetype = "longdash") +
        geom_text(data = annotations, aes(x = x, y = y, label = text)) +
        scale_color_manual(values = cbPalette) +
        guides(color=guide_legend(title=NULL)) +
        labs(title = main, x = xlab, y = ylab)
}


plot_series_error <- function(error_model, main = NULL, xlab = NULL, ylab = NULL, legend.names = NULL, col = NULL)
{
    con <- error_model$con
    sma <- get_error_model(error_model, "ADIDA - SMA")
    crost <- get_error_model(error_model, "Croston")
    agg <- get_error_model(error_model, "AGG")
    plot_mult_xts(con, sma, crost, agg, main = main, xlab = xlab, ylab = ylab, legend.names = legend.names, col = col)
}


plot_pred_error <- function(model, ind)
{
    #' Plot predictions  model for the ind customer from the model object
    preds <- model$pred[[ind]]

    asact_err <- model$pred[[ind]]$asact$err
    clus_err <- model$pred[[ind]]$clus$err
    cum_err <- model$pred[[ind]]$cum$err

    cat(sprintf("RMSE Errors \r
ASACT = %s\r
Clus = %s\r
ADIDA = %s\n",
asact_err, clus_err, cum_err))

    plot_mult_xts(preds$real, preds$asact$pred, preds$clus$pred, preds$cum$pred,
                  legend.names = c("Real", "ASACT", "Clus", "Cum"))
}


plot_vertical_clus <- function(x, series = NULL, grob = FALSE) {
    #' Plot the multivariate series of a cluter object stacked vertically
    clus <- seq_len(x@k)
    centroids <- x@centroids

    if (!is.null(series)) {
        data <- tslist(series)
    } else {
        if (length(x@datalist) < 1L)
            stop("Provided object has no data. Please provide the data manually.") # nocov
        data <- x@datalist
    }

                                        # helper values (lengths() here, see issue #18 in GitHub)
    L1 <- lengths(data)
    L2 <- lengths(centroids)
                                        # timestamp consistency
                                        # transform to data frames
    dfm <- reshape2::melt(data)
    dfcm <- reshape2::melt(centroids)

                                        # time, cluster and colour indices
    color_ids <- integer(x@k)

    dfm_tcc <- mapply(x@cluster, L1, USE.NAMES = FALSE, SIMPLIFY = FALSE,
                      FUN = function(clus, len) {
                          t <- rep(seq_len(len/2), 2)
                          cl <- rep(clus, len)
                          color <- rep(color_ids[clus], len)
                          color_ids[clus] <<- color_ids[clus] + 1L
                          data.frame(t = t, cl = cl, color = color)
                      })
    dfcm_tc <- mapply(1L:x@k, L2, USE.NAMES = FALSE, SIMPLIFY = FALSE,
                      FUN = function(clus, len) {
                          t <- rep(seq_len(len/2), 2)
                          cl <- rep(clus, len)
                          data.frame(t = t, cl = cl)
                      })

                                        # bind
    dfm <- data.frame(dfm, do.call(rbind, dfm_tcc, TRUE))
    dfcm <- data.frame(dfcm, do.call(rbind, dfcm_tc, TRUE))
                                        # make factor
    dfm$cl <- factor(dfm$cl)
    dfcm$cl <- factor(dfcm$cl)
    dfm$color <- factor(dfm$color)

    cluster_members_labeller <- function(cl) {
        members <- percent(sum(x@cluster == cl)/length(x@cluster))
        return(paste("Cluster", cl, "-", members, "mem.", sep = " "))
    }

    single_plot <- function(cl) {
        gg <- ggplot2::ggplot(data.frame(t = integer(),
                                         variable = factor(),
                                         value = numeric(),
                                         cl = factor(),
                                         color = factor()),
                              ggplot2::aes_string(x = "t",
                                                  y = "value",
                                                  group = "L1"))

                                        # add series appropriate

        gg <- gg + ggplot2::geom_line(data = dfm[dfm$cl == cl, ], aes_string(colour = "color")) +
            ggplot2::scale_color_hue(h.start = 180)

        ## Random colors
        ## n  <- length(levels(dfm$color))
        ## hues <- hue_pal()(n)[order(sample(1:n, n))]

        ## gg <- gg + ggplot2::scale_color_manual(values = hues)

        ## Centroid
        gg <- gg + ggplot2::geom_line(data = dfcm[dfcm$cl == cl, ],
                                      linetype = "solid",
                                      size = 1.5,
                                      colour = "black",
                                      alpha = 0.5)

        ## Label
        foo <- cluster_members_labeller(cl)
        names(foo) <- cl


        gg <- gg +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "none",
                           axis.title.x = ggplot2::element_blank(),
                           axis.title.y = ggplot2::element_blank(),
                           panel.spacing.y = ggplot2::unit(0, "lines")) +
            ggplot2::facet_grid(Var2 ~ cl, labeller = ggplot2::labeller(cl = foo, Var2 = ggplot2::label_value)) +
            ggplot2::guides(colour = FALSE)

        return(gg)
    }

    plot_list <- lapply(clus, single_plot)

    if (grob) {
        return(gridExtra::arrangeGrob(grobs = plot_list, ncol = 3, nrow = 3, bottom = "t", left = "Normalized value"))
    } else {
        gridExtra::grid.arrange(grobs = plot_list, ncol = 3, nrow = 3, bottom = "t", left = "Normalized value")
    }
}

plot_vertical_cluster_series <- function(x, series, preproc = FALSE, grob = FALSE) {
    #' Given the series, will plot then based on the orders of the cluster
    #' object.

    if (preproc) {
        #' Preprocess the series data with the cluster function
        series <- clus_preproc(series, x)
    }

    plot_vertical_clus(x, series = series, grob = grob)
}


plot_mv_centroids <- function(x) {
    #' Superimpose the multivariate centroids for a cluster object
    clus <- seq_len(x@k)

    centroids <- x@centroids

    L2 <- lengths(centroids)
                                        # timestamp consistency
                                        # transform to data frames

    dfcm <- reshape2::melt(centroids)

                                        # time, cluster and colour indices
    dfcm_tc <- mapply(1L:x@k, L2, USE.NAMES = FALSE, SIMPLIFY = FALSE,
                      FUN = function(clus, len) {
                          t <- rep(seq_len(len/2), 2)
                          cl <- rep(clus, len)
                          data.frame(t = t, cl = cl)
                      })

                                        # bind
    dfcm <- data.frame(dfcm, do.call(rbind, dfcm_tc, TRUE))
                                        # make factor
    dfcm$cl <- factor(dfcm$cl)

    ## Create a labeller which returns the percentage of members in each cluster
    cluster_members_labeller <- function(cl) {
        members <- percent(sum(x@cluster == cl)/length(x@cluster))
        paste("Cluster", cl, "-", members, "mem.", sep = " ")
    }

    foo <- sapply(seq_len(x@k), cluster_members_labeller)
    names(foo) <- seq_len(x@k)

    gg <- ggplot2::ggplot(dfcm, ggplot2::aes(x = t, y = value, group = interaction(Var2, cl), colour = Var2)) +
        ggplot2::geom_line() +
        ggplot2::theme(legend.position="top") +
        ggplot2::labs(y = "Normalised Value", colour = "") +
        ggplot2::scale_color_hue(labels = c("consumption", "delivery")) +
        ggplot2::facet_wrap(~cl, scales = "free_y", labeller = ggplot2::labeller(cl = foo))

    plot(gg)
}


pareto_plot <- function(data, binsize, xlab = NULL, ylab = NULL) {
    #' Return a pareto plot while binning the x data togther by binsize
    res <- data[order(data, decreasing = TRUE)]
    myDF <- data.frame(count = sapply(split(res, ceiling(seq_along(res)/52)), sum))
    myDF$cust_group <- factor(seq(from = 1, to = length(n_dels)/52))
    myDF$cumulative <- cumsum(myDF$count)

    ggplot2::ggplot(myDF, ggplot2::aes(x=myDF$cust_group)) +
        ggplot2::geom_bar(ggplot2::aes(y=myDF$count), stat="identity") +
        ggplot2::geom_point(ggplot2::aes(y=myDF$cumulative), pch = 16, size = 3) +
        ggplot2::geom_path(ggplot2::aes(y=myDF$cumulative, group = 1), lty = 3, size = 0.9) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = xlab, y = ylab)
}


pareto_curve <- function(data, xlab = NULL, ylab = NULL) {
    res <- data[order(data, decreasing = TRUE)]
    myDF <- data.frame(res = res)
    myDF$cumulativey <- cumsum(myDF$res)/sum(data)
    myDF$cumulativex <- index(data)/length(data)

    ggplot2::ggplot(myDF, ggplot2::aes(x=myDF$cumulativex)) +
        ggplot2::geom_path(ggplot2::aes(y=myDF$cumulativey, group = 1), linetype = "solid", size = 1) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = xlab, y = ylab) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_x_continuous(labels = scales::percent)
}
