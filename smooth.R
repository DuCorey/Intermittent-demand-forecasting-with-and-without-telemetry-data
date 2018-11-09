library("tsintermittent")


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


my_croston <- function(data, ...) {
    ## Wrapper function over tsintermittent::crost

    if (sum(data != 0) < 2) {
        warning("Croston requires a minimum of 2 non-zero values. Returning NULL.")
        return(NULL)
    }

    ## Convert data to numeric since the methods don't like to take xts input
    smoothed <- tsintermittent::crost(as.numeric(data), h = 0, init = "mean",
                                      init.opt = FALSE, type = "sba", ...)$frc.in

    ## Refit the time series so the times add match up again.
    ## Croston being an exponential smoothing method will lose a time date

    smoothed_xts <- smoothed[!is.na(smoothed)] %>%
        xts(., order.by = index(data)[-1])

    attr(smoothed_xts, 'frequency') <- frequency(data)

    return(smoothed_xts)
}


my_ets <- function(data, ...) {
    ## Wrapper function over forecast::ets

    if (nrow(data) < 1) {
        warning("ets requires a minimum of 1 value in the time series. Returning NULL.")
        return(NULL)
    }


    ## Convert data to numeric since the methods don't like to take xts input
    res <- fitted(forecast::ets(as.numeric(data), model = "ZNN", ...)) %>%
        xts(., order.by = index(data))

    attr(res, 'frequency') <- frequency(data)

    return(res)
}
