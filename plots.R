plot_cluster_series <- function(clus, series)
{
    #' Given the series, will plot then based on the orders of the cluster
    #' object.
    plot(clus, type = "series", series = series)
}
