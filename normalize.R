#' title: normalize.R
#' comments: normalizing functions for different data types
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages

#' imports

#' functions
z_score <- function(ts) {
    if (is.null(ts)) {
        warning("Null value passed returning NULL.")
        return(NULL)
    } else {
        return((ts - mean(ts))/sd(ts))
    }
}


un_z_score <- function(ts, mean, sd) {
    return(ts*sd + mean)
}


scale <- function(ts) {
    #' Scale the data between 0 and 1
    if (is.null(ts)) {
        warning("Null value passed returning NULL.")
        return(NULL)
    } else {
        return((ts-min(ts))/(max(ts)-min(ts)))
    }
}


unscale <- function(ts, max, min) {
    return(ts * (max - min) + min)
}


mean_normalization <- function(ts) {
    return((ts - mean(ts))/max(ts) - min(ts))
}


un_mean_normalization <- function(ts, mean, max, min) {
    return(ts * (max - min) + mean)
}


scale_ab <- function(ts, a, b) {
    if (is.null(ts)) {
        warning("Null value passed returning NULL.")
        return(NULL)
    } else {
        return(a + (ts - min(ts)) * (b - a) / (max(ts) - min(ts)))
    }
}


unscale_ab <- function(ts, a, b, max, min) {
    return((ts - a) * (max - min) / (b - a) + min)
}


normalize_relative <- function(x, y) {
    #' Normalize serie x relative to serie y
    return(x * sum(y)/sum(x))
}


scale_mult <- function(...) {
    #' Scale multiple time series based on the global max and min
    my_max <- max(c(...))
    my_min <- min(c(...))
    return(lapply(list(...), function(x) (x - my_min) / (my_max - my_min)))
}
