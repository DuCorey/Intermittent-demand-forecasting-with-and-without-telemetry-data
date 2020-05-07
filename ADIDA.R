#' title: ADIDA.R
#' comments: Aggregatte Dissagregate Intermittent Demand approach functions
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' pacakges
library("zoo")
library("forecast")

#' imports
source("error.R")
source("series.R")
source("dtwclust.R")

#'functions
aggregate_temp <- function(data, binsize, FUN = sum) {
    #' Temporal aggreqation of time series
    #' The series are temporaly agregated starting from the end as such some starting can be loss

    ## Cut down the length so that we capture all of the end
    rem <- length(data) %% binsize
    if (rem != 0) {
        data <- trim_ts(data, rem, "start")
    }

    agg <- zoo::rollapply(data, binsize, FUN, by=binsize)
    agg <- agg[!is.na(agg)]
    return(agg)
}


disaggregate_weighted <-  function(data, weights) {
    #' Implementation of weighted disaggregation of time series

    cur_agg_lvl <- unique(diff(index(data)))

    ## if (length(cur_agg_lvl) != 1) {
    ##     stop("Aggregation level is inconsistent accross the serie.")
    ## } else if (cur_agg_lvl = 1) {
    ##     warning("Cannot disagregate timeseries of 1 aggregation. Returning input serie.")
    ##     return(data)
    ## }

    res  <- as.vector(t(data %*% weights))
    return(res)
}


disaggregate_sma <- function(data, binsize) {
    weights <- simpleweights(binsize)
    res <- disaggregate_weighted(data, weights)
   return(res)
}


disaggregate_ema <- function(data, binsize) {
    weights <- emaweights(binsize)
    res <- disaggregate_weighted(data, weights)
    return(res)
}


emaweights <- function(m) {
    alpha <- 2/(m+1)
    i <- 1:m
    sm <- sum((alpha*(1-alpha)^(1-i)))
    weights <- (alpha*(1-alpha)^(1-i))/sm
    return(rev(weights))
}


wmaweights <- function(m) {
    weights <- (1:m)/sum(1:m)
    return(rev(weights))
}


simpleweights <- function(m) {
    return(rep(1/m, m))
}


ADIDA <- function(data, binsize, type) {
    #' Implementation of ADIDA framework
    type <- match.arg(type, c("SMA", "EMA"))

    rem <- length(data) %% binsize

    agg <- aggregate_temp(data, binsize)
    res <- switch(type,
                  SMA = disaggregate_sma(agg, binsize),
                  EMA = disaggregate_ema(agg, binsize))

    if (is.xts(data)) {
        res <- xts(res, order.by = index(data)[(rem + 1):length(data)])
    }

    return(res)
}


series_agg <- function(series, func = mean) {
    #' Aggregate the series based on the function.
    #' For time series each time period will be have the func applied to each
    #' Also works for multivariate series where each variable will be aggregated seperately
    if (is_multivariate(series)) {
        L <- sapply(series, NCOL)[[1]]
        res <- vector("list", L)

        for (i in seq_len(L)) {
            series_sub <- lapply(series, function(x) x[,i])
            df <- as.data.frame(series_sub, col.names = 1:length(series_sub))
            res[[i]] <- apply(df, 1, func)
        }

        res <- Reduce(cbind, res)
    } else {
        #' Univariate series
        df <- as.data.frame(series, col.names = 1:length(series))
        res <- apply(df, 1, func)
    }

    colnames(res) <- colnames(series[[1]])

    ## If the series as xts, we reformat the output to be xts
    if (is.xts(series[[1]])) {
        res <- as.xts(res, order.by = index(series[[1]]))
    }

    return(res)
}


mean_series <- function(series) {
    return(series_agg(series, func = mean))
}


cum_ADIDA <- function(serie, max_bin, type = "SMA", agg_func = mean) {
    #' Return the cumulative ADIDA model for the serie
    #' Results are cumulated using the agg_func and the ADIDA model is done
    #' with following the max_bin and the type
    bins <- 1:max_bin
    series <- lapply(bins, function(x) ADIDA(serie, x, type = type))
    min_length <- min(lengths(series))
    series %<>% lapply(., function(x) trim_ts(x, length(x) - min_length, "start"))
    res <- series_agg(series, agg_func)

    return(res)
}


opt_agg <- function(con, del, model_f) {
    #' Determine the optinal aggregation level (bin) for an ADIDA model
    #' If we knew the form of the original series that we are converting (AR, MA)
    #' the optimal aggregation level can be found analytically refer to paper ...

    foo <- match_ends(con, del, "end")
    con <- foo[[1]]
    del <- foo[[2]]

    max_bin <- floor(length(del) / 2)
    bins <- 1:max_bin
    series <- lapply(bins, function(x) model_f(del, x))

    ## Pretty sure you can determine the minimum using only modular arithmetic
    ## but I can't figure it out and the time saved is negligeable
    min_length <- min(lengths(series))

    ## Trim all the ADIDA time series so we can measure the same error on all of them
    series <- lapply(series, function(x) trim_ts(x, length(x) - min_length, "start"))

    ## Calculate the error for all the series
    foo <- match_ends(con, series[[1]], "start")
    con <- foo[[1]]
    errors <- sapply(series, error_calc, b = con, type = "mae")

    opt <- which.min(errors)


    structure(
        list(
            opt = opt,
            series = series,
            errors = errors
        )
    )
}

if (FALSE) {
    #' Series Aggregation
    a <- generate_xts(5)
    b <- generate_xts(5)
    series_agg(list(a, b))
}
