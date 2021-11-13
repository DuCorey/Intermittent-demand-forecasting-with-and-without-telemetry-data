#' title: intermittent.R
#' comments: functions working on intermittent time series
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(tsintermittent)
library(xts)
library(MAPA)

#' imports
source("ADIDA.R")

#' functions
forward_propagation <- function(x) {
    #' Forward propagation of non zero values as if they were fluxes
    spread <- x
    flux <- NA
    start <- NA
    j <- 1

    while (j < length(x)) {
        if (x[[j]] != 0) {
            start <- x[[j]]
            count <- 1
            while (x[[j+1]] == 0) {
                count <- count + 1
                j <- j + 1
                if (j >= length(x)){
                    break
                }
            }
            flux <- start/count
            spread[(j-count+1):j] <- flux
        }
        j <- j+1
    }
    return(spread)
}


simple_croston <- function(data, alpha) {
    #' Simplest implementation of Croston's method to smooth intermittent time series
    n <- length(data)
    nzd <- which(data != 0)
    k <- length(nzd)
    ## Demand interval
    x <- c(nzd[1], nzd[2:k] - nzd[1:(k-1)])
    ## Non zero demand periods
    z <- data[nzd]
    init <- c(z[1], x[1])

    zfit <- vector("numeric", k)
    xfit <- vector("numeric", k)

    zfit[1] <- init[1]
    xfit[1] <- init[2]
    for (i in 2:k) {
        zfit[i] <- zfit[i - 1] + alpha * (z[i] - zfit[i - 1])
        xfit[i] <- xfit[i - 1] + alpha * (x[i] - xfit[i - 1])
    }
    cc <- zfit/xfit

    frc.in <- rep(NA, n)
    tv <- c(nzd + 1, n)

    for (i in 1:k) {
        frc.in[tv[i]:tv[i+1]] <- cc[i]
    }
    return(frc.in)
}


croston_smooth <- function(object, ...) {
    UseMethod("croston_smooth")
}


croston_smooth.xts <- function(obj, ...) {
    #' Smooth the data using croston
    return(croston_smooth(croston(obj, ...)))
}


croston_smooth.croston <- function(obj, ...) {
    #' Smooth the data using croston
    #' Pass it a croston object with fitted data
    smoothed <- obj$frc.in
    ## Refit the time series so the times add match up again.
    ## Croston being an exponential smoothing method will lose an observation
    ## Smoothing always loses the first point of data
    if (is.xts(obj$fitted)) {
        smoothed <- smoothed[!is.na(smoothed)] %>%
            xts(., order.by = tail(index(obj$fitted), -sum(is.na(smoothed))))

        attr(smoothed, 'frequency') <- frequency(obj$fitted)
    } else {
        smoothed <- tail(smoothed, -1)
    }

    return(smoothed)
}


croston_smooth.crostoncompact <- function(obj, x) {
    #' Since the object passed is the compact croston, we resmooth the data
    #' using the computed parameters
    return(croston_smooth.croston(forecast.crostoncompact(obj, x, 0)))
}


croston <- function(data, f.type = c("SBA.base", "SBA.opt"), ...) {
    f.type <- match.arg(f.type, c("SBA.base", "SBA.opt"))
    x <- as.numeric(data)

    model <- switch(f.type,
                    SBA.base = tsintermittent::crost(x, h = 0, w = 0.05,
                                                     type = "sba",
                                                     init.opt = FALSE, ...),
                    SBA.opt = tsintermittent::crost(x, h = 0, type = "sba", ...))
    model$x <- x
    model$type <- f.type
    model$fitted <- data

    structure(
        model,
        class = "croston"
    )
}


forecast.croston <- function(obj, h, ...) {
    #' Since we already have the croston object, we don't need to redo any of
    #' the optimisations in the crosotn method
    out <- tsintermittent::crost(obj$x, h = h, w = obj$weights, init = obj$initial,
                                 type = obj$model, init.opt = FALSE, ...)
    out$h <- h

    structure(
        out,
        class = "crostonforecast"
    )
}


update.croston <- function(obj, newdata, ...) {
    #' How we want to update a croston object with new data
    #' We have to recalculate all the parameters
    return(croston(newdata, obj$type, ...))
}


compact_forecast.croston <- function(obj) {
    #' Compact representation of the croston object.
    #' This is all we need to recreate the original object given the same data
    structure(
        list(
            weights = obj$weights,
            initial = obj$initial,
            model = obj$model
        ),
        class = "crostoncompact"
    )
}


compact_forecast.crostoncompact <- function(obj) {
    return(obj)
}


forecast.crostoncompact <- function(obj, x, h) {
    obj$x <- x
    return(forecast.croston(obj, h))
}


result_forecast.crostonforecast <- function(fcast) {
    #' Return the numerical forecast result only
    return(as.numeric(fcast$frc.out))
}


ASACT <- function(data, agg.time, ffun, h = agg.time) {
    #' Implementation of ASACT forecast by (Murray et al. 2018)
    #' Initial data is the low level time series
    #' A) Smooth
    #' B) Aggregate
    #' C) Forecast
    #' D) Disaggreate to return to the original time scale
    #' Methods B), C) and D) are essentially the ADIDA method

    ## A) Smooth with croston opt
    smooth_model <- croston(data, f.type = "SBA.opt")
    ## Use the passed compacted smooth model
    smooth <- croston_smooth(smooth_model, data)

    ## B), C), D) using ADIDA
    res <- ADIDA(smooth, agg.time, ffun, h, disaggregate_sma)


    structure(
        list(
            frc.out = res$frc.out,
            smooth = smooth,
            smooth.model = smooth_model,
            disag = res$disag,
            fitted = data,
            agg.time = agg.time,
            h = h,
            ffun = ffun,
            fmodel = res$fmodel
        ),
        class = "ASACT"
    )
}


update.ASACT <- function(obj, newdata, ...) {
    return(ASACT(newdata, obj$agg.time, obj$ffun, obj$h, ...))
}


compact_forecast.ASACT <- function(obj) {
    #' Compact representation of the ADIDA object.
    #' This is all we need to recreate the original object given the same data
    structure(
        list(
            agg.time = obj$agg.time,
            smooth.model = compact_forecast(obj$smooth.model),
            fmodel = compact_forecast(obj$fmodel)
        ),
        class = "ASACTcompact"
    )
}


forecast.ASACTcompact <- function(obj, x, h) {
    #' How we forecast a comapcted ASACT object as fast as we can
    #' Equivalent to doing the ADDIAcompact forecast after smoothing the data
    ## Smooth the data using the saved values in the smooth.model
    smooth <- croston_smooth(obj$smooth.model, x)

    ## Need to send set these attributes for the ADIDAcompact forecast method
    obj$binsize <- obj$agg.time
    obj$disagfun <- disaggregate_sma

    res <- forecast.ADIDAcompact(obj, smooth, h)

    return(res)
}


result_forecast.ASACTforecast <- function(fcast) {
    return(as.numeric(fcast$out))
}


ASACT2 <- function(data, agg.time, ffun, h = 1) {
    #' Implementation of ASACT forecast by (Murray et al. 2018)
    #' Initial data is the low level time series with proper months support
    #' A) Smooth
    #' B) Aggregate
    #' C) Forecast
    #' D) Disaggreate to return to the original time scale
    #' Methods B), C) and D) are essentially the ADIDA method

    agg.time <- match.arg(agg.time, c("week", "month"))

    ## A) Smooth with croston opt
    smooth_model <- croston(data, "SBA.opt")
    ## Use the passed compacted smooth model
    smooth <- croston_smooth(smooth_model, data)

    if (agg.time == "week") {
        ## The ASACT method is essentially the ADIDA method using 7 for the agg
        res <- ADIDA(smooth, 7, ffun, h, disaggregate_sma)
        frc.out  <- res$frc.out
        disag <- res$disag
        fmodel <- res$fmodel
    } else {
        ## B) Aggregation on months
        agg <- apply.monthly(smooth, sum)
        attr(agg, "frequency") <- 12

        ## C) Forecast
        h_month <- number_of_months_for_horizon(h, tail(agg, 1))
        fmodel <- ffun(agg)
        fcast <- result_forecast(forecast(fmodel, h_month))

        ## D) Disagregate each forward step one at a time
        disag <- c()
        cur_month <- month(tail(agg, 1))
        for (i in seq(h_month)) {
            next_month <- mod(cur_month + i, 12)
            days <- as.numeric(days_in_month(next_month))
            sma <- disaggregate_sma(fcast[[i]], days)
            disag <- c(disag, sma)
        }
        frc.out <- head(disag, h)
    }


    structure(
        list(
            frc.out = frc.out,
            smooth = smooth,
            smooth.model = smooth_model,
            agg.time = agg.time,
            disag = disag,
            fitted = data,
            agg.time = agg.time,
            h = h,
            ffun = ffun,
            fmodel = fmodel
        ),
        class = "ASACT2"
    )
}


forecast.ASACT2 <- function(obj, h, ...) {
    #' Forecast of the ASACT 2 method
    if (h == obj$h) {
        res <- obj$frc.out
    } else if (h <= length(obj$disag)) {
        res <- head(obj$disag, h)
    } else {
        res <- forecast(ASACT2(obj$fitted, obj$agg.time, obj$ffun, h = h), h)
    }


    structure(
        list(
            out = res
        ),
        class = "ASACT2forecast"
    )
}


update.ASACT2 <- function(obj, newdata, ...) {
    return(ASACT2(newdata, obj$agg.time, obj$ffun, obj$h, ...))
}


compact_forecast.ASACT2 <- function(obj) {
    #' Compact representation of the ASACT2 object
    #' This is all we need to recreate the original object given the same data
    structure(
        list(
            agg.time = obj$agg.time,
            smooth.model = compact_forecast(obj$smooth.model),
            fmodel = compact_forecast(obj$fmodel)
        ),
        class = "ASACT2compact"
    )
}

## TODO
## forecast.ASACT2compact <- function(obj, x, h) {

## }


result_forecast.ASACT2forecast <- function(fcast) {
    return(as.numeric(fcast$out))
}


## ASACT_smooth <- function(x, time, weight = "EQW", f.type = "ets") {
##     time <- match.arg(time, c("weekly", "monthly"))
##     weight <- match.arg(weight, c("EQW"))
##     f.type <- match.arg(f.type, c("naive", "SBA.base", "SBA.opt", "ets"))
##     ## Aggregate using ASACT
##     agg <- ASACT(x, time)

##     ## Forecast the aggregated serie 1 step ahead
##     f1 <- switch(f.type,
##                  naive = forecast::naive(as.ts(agg), 1)$mean,
##                  ets = predict(forecast::ets(as.ts(agg)), 1)$mean,
##                  SBA.base = croston_predict(agg, 1, w = 0.05, init.opt = FALSE)$frc.out,
##                  SBA.opt = croston_predict(agg, 1, init.opt = TRUE, nop = 2)$frc.out)

##     ## Disaggregate
##     ## The disagergation amount depends on the length of the object 30 or 31 days for months
##     ## 7 days for a week
##     if (time == "weekly") {
##         disag <- disaggregate_sma(f1, 7)
##     } else {
##         ## Determine the number of days in the forecasted month for the disag
##         days <- lubridate::days_in_month(end(apply.monthly(bar, sum)) + 1)
##         disag <- disaggregate_sma(f1, days)
##     }

##     structure(
##         list(
##             model = paste("ASACT", f.type, weight, sep = " - "),
##             time = time,
##             out = disag
##         ),
##         class = "MyForecast"
##     )
## }


MAPA <- function(data, agg, ...) {
    mapafit <- MAPA::mapaest(as.ts(data), ppy = agg, type = "ets", model = "ZZZ", ...)


    structure(
        list(
            x = as.ts(data),
            mapafit = mapafit,
            agg = agg
        ),
        class = "MAPA"
    )
}


fitted.MAPA <- function(obj) {
    return(obj$mapafit[, "fitted"])
}


forecast.MAPA <- function(obj, h, ifh = 0, ...) {
    fcast <- mapafor(obj$x, obj$mapafit, fh = h, ifh = ifh, comb = "w.mean")

    structure(
        fcast,
        class = "MAPAforecast"
    )
}


update.MAPA <- function(obj, newdata, ...) {
    return(MAPA(newdata, obj$agg))
}


compact_forecast.MAPA <- function(obj) {
    structure(
        list(
            mapafit = obj$mapafit
        ),
        class = "MAPAcompact"
    )
}


forecast.MAPAcompact <- function(obj, x, h, ...) {
    #' How we forecast a compacted MAPA object
    fcast <- mapafor(x, obj$mapafit, fh = h, ifh = 0, comb = "w.mean")

    structure(
        fcast,
        class = "MAPAforecast"
    )
}


result_forecast.MAPAforecast <- function(obj) {
    return(as.numeric(obj$outfor))
}


## my_mapa <- function(data, agg, h, ...) {
##     #' These results do not match up perfectly to MAPA
##     #' Although it is about twice as fast as the MAPA::mapa
##     fors <- lapply(1:7, function(x) {
##         result_forecast(forecast(ADIDA(data, x, ets, h), h))
##     })

##     return(series_agg(fors))
## }


crost_decomp <- function(x) {
    #' Perform the croston decomposition of the time series
    decomp <- tsintermittent::crost.decomp(x, init = "naive")
    decomp$demand %<>% as.numeric %>%
        head(., -1)
    decomp$interval %<>% tail(., -1)
    decomp$flux <- decomp$demand/decomp$interval
    return(decomp)
}


tukey_decomp_table <- function(x) {
    decomps <- lapply(x, crost_decomp)
    tukey_demand <- lapply(decomps, function(x) fivenum(as.numeric(x$demand)))
    tukey_int <- lapply(decomps, function(x) fivenum(as.numeric(x$interval)))
    tukey_flux <- lapply(decomps, function(x) fivenum(as.numeric(x$flux)))

    d_min_mean <- mean(sapply(tukey_demand, function(x) x[[1]]))
    d_min_sd <- sd(sapply(tukey_demand, function(x) x[[1]]))

    d_25_mean <- mean(sapply(tukey_demand, function(x) x[[2]]))
    d_25_sd <- sd(sapply(tukey_demand, function(x) x[[2]]))

    d_med_mean <- mean(sapply(tukey_demand, function(x) x[[3]]))
    d_med_sd <- sd(sapply(tukey_demand, function(x) x[[3]]))

    d_75_mean <- mean(sapply(tukey_demand, function(x) x[[4]]))
    d_75_sd <- sd(sapply(tukey_demand, function(x) x[[4]]))

    d_max_mean <- mean(sapply(tukey_demand, function(x) x[[5]]))
    d_max_sd <- sd(sapply(tukey_demand, function(x) x[[5]]))

    int_min_mean <- mean(sapply(tukey_int, function(x) x[[1]]))
    int_min_sd <- sd(sapply(tukey_int, function(x) x[[1]]))

    int_25_mean <- mean(sapply(tukey_int, function(x) x[[2]]))
    int_25_sd <- sd(sapply(tukey_int, function(x) x[[2]]))

    int_med_mean <- mean(sapply(tukey_int, function(x) x[[3]]))
    int_med_sd <- sd(sapply(tukey_int, function(x) x[[3]]))

    int_75_mean <- mean(sapply(tukey_int, function(x) x[[4]]))
    int_75_sd <- sd(sapply(tukey_int, function(x) x[[4]]))

    int_max_mean <- mean(sapply(tukey_int, function(x) x[[5]]))
    int_max_sd <- sd(sapply(tukey_int, function(x) x[[5]]))

    flux_min_mean <- mean(sapply(tukey_flux, function(x) x[[1]]))
    flux_min_sd <- sd(sapply(tukey_flux, function(x) x[[1]]))

    flux_25_mean <- mean(sapply(tukey_flux, function(x) x[[2]]))
    flux_25_sd <- sd(sapply(tukey_flux, function(x) x[[2]]))

    flux_med_mean <- mean(sapply(tukey_flux, function(x) x[[3]]))
    flux_med_sd <- sd(sapply(tukey_flux, function(x) x[[3]]))

    flux_75_mean <- mean(sapply(tukey_flux, function(x) x[[4]]))
    flux_75_sd <- sd(sapply(tukey_flux, function(x) x[[4]]))

    flux_max_mean <- mean(sapply(tukey_flux, function(x) x[[5]]))
    flux_max_sd <- sd(sapply(tukey_flux, function(x) x[[5]]))

    mat <- matrix(c(d_min_mean, d_min_sd, int_min_mean, int_min_sd, flux_min_mean, flux_min_sd,
                    d_25_mean, d_25_sd, int_25_mean, int_25_sd, flux_25_mean, flux_25_sd,
                    d_med_mean, d_med_sd, int_med_mean, int_med_sd, flux_med_mean, flux_med_sd,
                    d_75_mean, d_75_sd, int_75_mean, int_75_sd, flux_75_mean, flux_75_sd,
                    d_max_mean, d_max_sd, int_max_mean, int_max_sd, flux_max_mean, flux_max_sd),
                  nrow = 5, ncol = 6, byrow = TRUE)

    dimnames(mat) = list(c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max."),
                         c("Demand Mean", "Demand sd", "Interval Mean", "Interval sd", "Flux Mean", "Flux sd"))
    print(mat)
}


intermittent_categorisation <- function(data, ...) {
    #' Intermittent time series categorisation

    ## Convert the incoming data into a dataframe for the function. Fill with NA's if they aren't the same size
    if (is.list(data)) {
        data <- do.call(partial(rowr::cbind.fill, fill = NA), data)
    }

    res <- tsintermittent::idclass(data, ...)
    return(res)
}

if (FALSE) {
    foo <- generate_xts(50)
    bar <- croston(foo)
    result_forecast(forecast(bar, 10))
    result_forecast(forecast(compact_forecast(bar), foo, 10))

    ## ASACT model
    data <- generate_xts(50)

    foo <- ASACT(data, 7, ets)
    forecast(foo, 10)

    cux <- croston(data, "SBA.opt")
    bar <- ASACT(data, 7, ets, smooth_model = cux)
    forecast(bar, 10)

    bar <- ADIDA(croston_smooth(croston(data, "SBA.opt")), 7, ets)
    forecast(bar, 10)

    cux <- compact_forecast(foo)
    forecast(cux, x = data, h = 10)

    ## MAPA
    foo <- MAPA(data, 7)
    forecast(foo, 7)
    result_forecast(forecast(foo, 7))
    compact_forecast(foo)
    forecast(compact_forecast(foo), data, 7)
    result_forecast(forecast(compact_forecast(foo), data, 7))

}
