#' title: plots.R
#' comments: plotting functions for various data types
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(ggplot2)

#' source

#' functions
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_cluster_series <- function(clus, series)
{
    #' Given the series, will plot then based on the orders of the cluster
    #' object.
    plot(clus, type = "series", series = series)
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
