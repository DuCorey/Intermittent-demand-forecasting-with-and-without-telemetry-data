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


plot_mult_xts <- function(...)
{
    #' Plots multiple xts series together
    plot(merge(...))
}


plot_error_model <- function(mod)
{
    ## Creating the necessary data structures
    agg_level <- seq_along(mod$SMA$error)
    sma_data <- data.frame(y = mod$SMA$error, x = agg_level, size = 1)
    sma_data$size[[mod$SMA$opt]] <- 2  # The size doesn't actually matter since it's relative
    ema_data <- data.frame(y = mod$EMA$error, x = agg_level, size = 1)
    ema_data$size[[mod$EMA$opt]] <- 2
    annotations <- data.frame(text = c(mod$SMA$opt, mod$EMA$opt),
                              x = c(mod$SMA$opt, mod$EMA$opt),
                              y = c(mod$SMA$error[[mod$SMA$opt]], mod$EMA$error[[mod$EMA$opt]]))


    ggplot(NULL) +
        geom_point(data = sma_data, aes(y = y, x = x, color = "SMA", size = size), show.legend = F) +
        geom_point(data = ema_data, aes(y = y, x = x, color = "EMA", size = size), show.legend = F) +
        geom_line(aes(y = mod$Croston, x = agg_level, color = "Croston"), size = 1.5) +
        geom_line(aes(y = mod$ASACT, x = agg_level, color = "ASACT"), size = 1.5, linetype = "longdash") +
        geom_text(data = annotations, aes(x = x, y = y, label = text)) +
        scale_color_manual(values = cbPalette) +
        guides(color=guide_legend(title=NULL))

}
