#' title: smooth.R
#' comments: smoothing functions for different data types
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages

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
