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

    ## cur_agg_lvl <- unique(diff(index(data)))

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


ADIDA <- function(data, binsize, ffun, h = binsize, disagfun = disaggregate_sma) {
    #' Implementation of ADIDA model
    #' A) Aggregate
    #' B) Forecast
    #' C) Disaggregate
    #' data : the data for the forecast
    #' binsize : the aggregation binsize
    #' ffun : the forecasting function
    #' h : the forecasting horizon
    #' disagfun: the disaggregation function

    ## A) Aggregate
    agg <- aggregate_temp(data, binsize)

    ## B) Forecast
    ## We forecast on the aggregated serie, but we want the horizon of the
    ## forecast to be on the disaggregated series
    ## The ceiling determines the minimum amount of forecasting we need on the
    ## aggregated series so that we have enough after disaggregating
    fmodel <- ffun(agg)
    fcast <- result_forecast(forecast(fmodel, ceiling(h/binsize)))

    ## C) Disaggregate
    disag <- disagfun(as.matrix(fcast), binsize)

    ## Final result
    res <- head(disag, n = h)

    structure(
        list(
            frc.out = res,
            disag = disag,
            fitted = data,
            binsize = binsize,
            h = h,
            fmodel = fmodel,
            ffun = ffun,
            disagfun = disagfun
        ),
        class = "ADIDA"
    )
}


## Final match up of the lengths
## rem <- length(data) %% binsize  to match up length
## if (is.xts(data)) {
##     res <- xts(res, order.by = index(data)[(rem + 1):length(data)])
## }


forecast.ADIDA <- function(obj, h, ...) {
    #' Forecast the ADIDA object
    if (h == obj$h) {
        res <- obj$frc.out
    } else if (h <= length(obj$disag)) {
        res <- head(obj$disag, h)
    } else {
        ## Continue the forecast using the save forecasting model
        fcast <- result_forecast(forecast(obj$fmodel, ceiling(h/obj$binsize)))
        ## Disaggregate
        disag <- obj$disagfun(as.matrix(fcast), obj$binsize)
        ## Final result
        res <- head(disag, n = h)
    }

    structure(
        list(
            out = res
        ),
        class = "ADIDAforecast"
    )
}


update.ADIDA <- function(obj, newdata, ...) {
    #' How we want to update an ADIDA object with new data
    #' We have to recalculate the whole thing
    #' Technically there is some edge cases where if the new data is identical
    #' to the old data on a subset and the new total length mod the binsize is 0
    #' we could not have to recalculate all the aggregation again, but that part is fast
    #' so I'm not going to do it.
    return(ADIDA(newdata, obj$binsize, obj$ffun, obj$h, obj$disagfun))
}


compact_forecast.ADIDA <- function(obj) {
    #' Compact representation of the ADIDA object.
    #' This is all we need to recreate the original object given the same data
    structure(
        list(
            binsize = obj$binsize,
            disagfun = obj$disagfun,
            fmodel = compact_forecast(obj$fmodel)
        ),
        class = "ADIDAcompact"
    )
}


forecast.ADIDAcompact <- function(obj, x, h) {
    #' How we forecast a comapcted ADIDA object as fast as we can
    ## Aggregate the data, as this is the data being sent to the compacted
    ## forecasting model
    agg <- aggregate_temp(x, obj$binsize)

    ## Use the compacted forecasting model to speed up
    fcast <- result_forecast(forecast(obj$fmodel, agg, ceiling(h/obj$binsize)))

    ## Disaggregate
    disag <- obj$disagfun(as.matrix(fcast), obj$binsize)

    ## Final result
    res <- head(disag, n = h)
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

    foo <- generate_xts(50)
    bar <- ADIDA(foo, 10, ffun = partial(croston, f.type="SBA.opt"))
    forecast(bar, h = 8)
    system.time(forecast(bar, h = 20))

    cux <- compact_forecast(bar)
    system.time(forecast(cux, foo, 20))

    forecast(bar, h = 20)
    update(bar, generate_xts(60))

}
