#' title: smooth.R
#' comments: smoothing functions for different data types
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(forecast)

#' imports

#' functions
ksmooth_xts <- function(data, ...)
{
    #' Smooth using ksmooth and return an xts
    res <- ksmooth(index(data), data, ...)$y

    res <- xts(res, order.by = index(data))
    attr(res, 'frequency') <- frequency(data)

    return(res)
}

ets_smooth_xts <- function(data, ...)
{
    #' automatic Exponential Smoothing
    if (nrow(data) < 1) {
        warning("ets requires a minimum of 1 value in the time series. Returning NULL.")
        return(NULL)
    }

    ## Convert data to numeric since the method don't like to take xts input
    smooth <- fitted(forecast::ets(as.numeric(data), ...))

        xts(., order.by = index(data))

    attr(res, 'frequency') <- frequency(data)

    return(res)
}
