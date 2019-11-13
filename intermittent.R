#' title: intermittent.R
#' comments: functions working on intermittent time series
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(tsintermittent)
library(xts)
library(MAPA)

#' imports

#' functions
forward_propagation <- function(x)
{
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


simple_croston <- function(data, alpha)
{
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


my_croston <- function(data, ...)
{
    #' Wrapper function over tsintermittent::crost
    if (sum(data != 0) < 2) {
        warning("Croston requires a minimum of 2 non-zero values. Returning NULL.")
        return(NULL)
    }
    ## Issue with Croston is that it has to begin with a non 0 value

    ## Convert data to numeric since the methods don't like to take xts input
    smoothed <- tsintermittent::crost(as.numeric(data), h = 0, init = "mean",
                                      init.opt = TRUE, type = "sba", ...)$frc.in

    ## Refit the time series so the times add match up again.
    ## Croston being an exponential smoothing method will lose an observation
    smoothed_xts <- smoothed[!is.na(smoothed)] %>%
        xts(., order.by = tail(index(data), -1))

    attr(smoothed_xts, 'frequency') <- frequency(data)

    return(smoothed_xts)
}


ASACT <- function(serie, agg, ...)
{
    #' Implementation of ASACT forecast by (Murray et al. 2018)
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


my_mapa <- function(data, agg, ...)
{
    #' Wrapper over mapa
    #' Don't use the parallel implementation of the function it is very poorly done
    smoothed <- MAPA::mapa(ts(data), ppy = agg, fh = 0, display = 0, conf.lvl=NULL, type = "ets", ...)$infor

    smoothed_xts <- smoothed[!is.na(smoothed)] %>%
        xts(., order.by = tail(index(data), -agg))

    attr(smoothed_xts, 'frequency') <- frequency(data)

    return(smoothed_xts)
}


crost_decomp <- function(x)
{
    #' Perform the croston decomposition of the time series
    decomp <- tsintermittent::crost.decomp(x, init = "naive")
    decomp$demand %<>% as.numeric %>%
        head(., -1)
    decomp$interval %<>% tail(., -1)
    decomp$flux <- decomp$demand/decomp$interval
    return(decomp)
}


tukey_decomp_table <- function(x)
{
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


intermittent_categorisation <- function(data, ...)
{
    #' Intermittent time series categorisation

    ## Convert the incoming data into a dataframe for the function. Fill with NA's if they aren't the same size
    if (is.list(data)) {
        data <- do.call(partial(rowr::cbind.fill, fill = NA), data)
    }

    res <- tsintermittent::idclass(data, ...)
    return(res)
}
