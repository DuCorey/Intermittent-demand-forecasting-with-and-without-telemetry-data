#' title: ADIDA.R
#' comments: Aggregatte Dissagregate Intermittent Demand approach functions
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' pacakges

#' imports
source("smooth.R")
source("error.R")
source("series.R")

#'functions
aggregate <- function(data, binsize, FUN = sum, ...)
{
    rem <- length(data) %% binsize
    if (rem != 0) {
        warning("Data length is not a multiple of binsize. You will lose some starting data when rolling.")
        data <- trim_ts(data, rem, "start")
    }

    agg <- rollapply(data, binsize, FUN, by=binsize)
    agg <- agg[!is.na(agg)]
    return(agg)
}


disaggregate_weighted <-  function(data, weights, ...)
{
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


disaggregate_sma <- function(data, binsize)
{
    weights <- simpleweights(binsize)
    res <- disaggregate_weighted(data, weights)
    return(res)
}


disaggregate_ema <- function(data, binsize)
{
    weights <- emaweights(binsize)
    res <- disaggregate_weighted(data, weights)
    return(res)
}


emaweights <- function(m)
{
    alpha <- 2/(m+1)
    i <- 1:m
    sm <- sum((alpha*(1-alpha)^(1-i)))
    weights <- (alpha*(1-alpha)^(1-i))/sm
    return(rev(weights))
}


wmaweights <- function(m)
{
    weights <- (1:m)/sum(1:m)
    return(rev(weights))
}


simpleweights <- function(m)
{
    return(rep(1/m, m))
}


ADIDA <- function(data, binsize, type)
{
    #' Implementation of ADIDA framework
    type <- match.arg(type, c("SMA", "EMA"))

    rem <- length(data) %% binsize

    res <- aggregate(data, binsize)
    res <- switch(type,
                  SMA = disaggregate_sma(res, binsize),
                  EMA = disaggregate_ema(res, binsize))
    res <- xts(res, order.by = index(data)[(rem + 1):length(data)])
    return(res)
}


match_ends <- function(a, b, how)
{
    #' Takes two time series and returns them so that both ends match
    #' It cuts off the extra in the larger time series.
    how <- match.arg(how, c("start", "end"))

    if (how == "end") {
        min_end <- min(end(a), end(b))
        a <- a[xts_range(start(a), min_end)]
        b <- b[xts_range(start(b), min_end)]
    } else if (how == "start") {
        max_start <- max(start(a), start(b))
        a <- a[xts_range(max_start, end(a))]
        b <- b[xts_range(max_start, end(b))]
    }

    return(list(a,b))
}


xts_range <- function(start, end)
{
    #' Create an xts range from start and end dates
    return(paste(start, end, sep = "::"))
}


croston_conversion <- function(con, del)
{
    foo <- match_ends(con, del, "end")
    con <- foo[[1]]
    del <- foo[[2]]

    pred_con <- my_croston(del)

    foo <- match_ends(pred_con, con, "start")
    pred_con <- foo[[1]]
    con <- foo[[2]]

    err <- error_calc(con, pred_con, type = "mae")

    structure(
        list(
            serie = pred_con,
            error = err
        )
    )
}


opt_agg <- function(con, del, model_f)
{
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


ASACT <- function(serie, agg, ...)
{
    agg <- match.arg(agg, c("daily", "weekly", "monthly"))

    serie %<>% my_croston(.)

    switch(agg, weekly = {
        serie <- convert_xts_weekly(serie)
    }, daily = {
        serie <- convert_xts_daily(serie)
    }, monthly = {
        serie <- convert_xts_monthly(serie)
    })

    ## if (trim) {
    ##     serie %<>% trim_ts(., n = 1)
    ## }

    return(serie)
}


all_models <- function(con, del, asact_time, asact_init_serie, error_type = "mae")
{
    ## Match the ends
    foo <- match_ends(con, del, "end")
    con <- foo[[1]]
    del <- foo[[2]]

    ## Model 1 - SMA ADIDA
    max_bin <- floor(length(del) / 2)
    bins <- 1:max_bin
    series_1 <- lapply(bins, function(x) ADIDA(del, x, type = "SMA"))

    ## Model 2 - EMA ADIDA
    series_2 <- lapply(bins, function(x) ADIDA(del, x, type = "EMA"))

    ## Model 3 - Croston
    serie_3 <- my_croston(del)

    ## Model 4 - ASACT
    serie_4 <- ASACT(asact_init_serie, asact_time)
    foo <- match_ends(con, serie_4, "end")
    serie_4 <- foo[[2]]

    ## Match the lengths of the series for all the models
    min_length <- min(lengths(series_1), lengths(series_2), length(serie_3),
                      length(serie_4))
    series_1 <- lapply(series_1, function(x) trim_ts(x, length(x) - min_length, "start"))
    series_2 <- lapply(series_2, function(x) trim_ts(x, length(x) - min_length, "start"))
    serie_3 <- trim_ts(serie_3, length(serie_3) - min_length, "start")
    serie_4 <- trim_ts(serie_4, length(serie_4) - min_length, "start")
    con <- trim_ts(con, length(con) - min_length, "start")

    ## Calculate the errors
    errors_1 <- sapply(series_1, error_calc, b = con, error_type)
    errors_2 <- sapply(series_2, error_calc, b = con, error_type)
    errors_3 <- sapply(serie_3, error_calc, b = con, error_type)
    errors_4 <- sapply(serie_4, error_calc, b = con, error_type)

    opt <- which.min(c(min(errors_1), min(errors_2), errors_3, errors_4))
    opt <- switch(as.character(opt),
                  "1" = "ADIDA - SMA",
                  "2" = "ADIDA - EMA",
                  "3" = "Croston",
                  "4" = "ASACT")

    ## We try to find an quasi optimal value. The first value close to the true minimum.
    sma_min <- which(abs(errors_1 - min(errors_1)) < 0.05 * (max(errors_1) - min(errors_1)))[[1]]
    ema_min <- which(abs(errors_2 - min(errors_2)) < 0.05 * (max(errors_2) - min(errors_2)))[[1]]
    q_opt <- which.min(c(errors_1[[sma_min]], errors_2[[ema_min]], errors_3, errors_4))
    q_opt <- switch(as.character(q_opt),
                    "1" = "ADIDA - SMA",
                    "2" = "ADIDA - EMA",
                    "3" = "Croston",
                    "4" = "ASACT")


    structure(
        list(
            con = con,
            SMA = list(error = errors_1, opt = which.min(errors_1), q_opt = sma_min),
            EMA = list(error = errors_2, opt = which.min(errors_2), q_opt = sma_min),
            Croston = errors_3,
            ASACT = errors_4,
            opt = opt,
            q_opt = q_opt,
            error_type = error_type
        )
    )
}


best_model_from_error <- function(error, con, del, asact_time, asact_init_serie)
{
    ## Match the ends
    foo <- match_ends(con, del, "end")
    con <- foo[[1]]
    del <- foo[[2]]

    switch(error$opt,
           "ADIDA - SMA" = {
               agg_level <- error$SMA$opt
               serie <- ADIDA(del, agg_level, "SMA")
               err <- error$SMA$error[agg_level]
           }, "ADIDA - EMA" = {
               agg_level  <- error$EMA$opt
               serie <- ADIDA(del, agg_level, "EMA")
               err <- error$EMA$error[agg_level]
           }, "Croston" = {
               serie <- my_croston(del)
               err <- error$Croston
           }, "ASACT" = {
               serie <- ASACT(asact_init_serie, asact_time)
               err <- error$ASACT
           })

    serie <- trim_ts(serie, length(serie) - length(error$con), "start")

    ## Test to make sure the conversion is correct
    check_err <- error_calc(error$con, serie, error$error_type)
    if (check_err != err) {
        stop("Error producing best serie from model.")
    }

    return(serie)
}

#' main
if (FALSE) {
    ## Static consumption
    del <- clus_tel_day$del$orig[[1141]]
    con <- clus_tel_day$con$orig[[1141]]

    ## Dynamic consumption
    del <- clus_tel_day$del$orig[[5]]
    con <- clus_tel_day$con$orig[[5]]

    asact_init_serie  <- clus_tel_day$del$orig[[5]]


    ## Daily model
    foo_rmse <- all_models(con, del, asact_time = "daily", asact_init_serie, error_type = "rmse")
    foo_mae <- all_models(con, del, asact_time = "daily", asact_init_serie, error_type = "mae")

    bar <- best_model_from_error(foo_1, con, del, "daily", del)

    plot_error_model(foo_mae)
    plot_mult_xts(bar)

    ## Weekly model
    all_models(clus_tel_week$con$orig[[5]], clus_tel_week$del$orig[[5]],
               asact_time = "weekly", asact_init_serie = clus_tel_day$del$orig[[5]])

    con <- clus_tel_week$con$orig[[5]]
    del <- clus_tel_week$del$orig[[5]]

    asact_time = "weekly"
    asact_init_serie = clus_tel_day$del$orig[[5]]


    foo <- match_ends(con, del, "end")
    con <- foo[[1]]
    del <- foo[[2]]

    ## Model 1 - SMA ADIDA
    max_bin <- floor(length(del) / 2)
    bins <- 1:max_bin
    series_1 <- lapply(bins, function(x) ADIDA(del, x, type = "SMA"))

    ## Model 2 - EMA ADIDA
    series_2 <- lapply(bins, function(x) ADIDA(del, x, type = "EMA"))

    ## Model 3 - Croston
    serie_3 <- my_croston(del)

    ## Model 4 - ASACT
    serie_4 <- ASACT(asact_init_serie, asact_time)
    foo <- match_ends(con, serie_4, "end")
    serie_4 <- foo[[2]]

    min_length <- min(lengths(series_1), lengths(series_2), length(serie_3),
                      length(serie_4), length(con))
    series_1 <- lapply(series_1, function(x) trim_ts(x, length(x) - min_length, "start"))
    series_2 <- lapply(series_2, function(x) trim_ts(x, length(x) - min_length, "start"))
    serie_3 <- trim_ts(serie_3, length(serie_3) - min_length, "start")
    serie_4 <- trim_ts(serie_4, length(serie_4) - min_length, "start")

    foo <- match_ends(con, series_1[[1]], "start")
    con <- foo[[1]]

    errors_1 <- sapply(series_1, error_calc, b = con, type = "mae")
    errors_2 <- sapply(series_2, error_calc, b = con, type = "mae")
    errors_3 <- sapply(serie_3, error_calc, b = con, type = "mae")
    errors_4 <- sapply(serie_4, error_calc, b = con, type = "mae")

    ## Monthly model
    all_models(clus_tel_month$con$orig[[2]], clus_tel_month$del$orig[[2]],
               asact_time = "monthly", asact_init_serie = clus_tel_day$del$orig[[2]])

    plot_mult_xts(del, con)
    plot_mult_xts(foo$SMA$serie[[150]], foo$con)
}
