#' title: forecast.R
#' comments: In sample and out sample forecasting functions as well as overwrites
#'           of the forecast package.
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(forecast)
library(lubridate)

#' imports
source("ADIDA.R")
source("intermittent.R")

#' functions
compact_forecast <- function(object, ...) {
    UseMethod("compact_forecast")
}


result_forecast <- function(object, ...) {
    UseMethod("result_forecast")
}


ets <- function(x, ...) {
    ## Do not allow the use of the lambda in the Box-Cox transformation since
    ## this forecasting will be used for intermittent data which are not
    ## normally distributed and thus the Box-Cox transformation is unsuited for it
    return(forecast::ets(as.ts(x), lambda = NULL, ...))
}


forecast.ets <- function(obj, h, ...) {
    fcast <- forecast::forecast.ets(object = obj, h = h, ...)
    fcast$h <- h
    return(fcast)
}


update.ets <- function(model, newdata, ...) {
    #' How to update an ets forcast update with new data
    #' Reuse the component type from the initial model but reestimate the coefficients
    modelcomponents <- paste(model$components[1], model$components[2],
                             model$components[3], sep = "")
    damped <- (model$components[4] == "TRUE")

    return(ets(newdata, model = modelcomponents, damped = damped, ...))
}


compact_forecast.ets <- function(x) {
    #' A compact form for the model which contains it's parameters so that one
    #' use it for other forecasts in the future. This avoids having to save
    #' the heavy time series data in the object call used to make the object.
    #' We save the parameters which are optimised for later use.

    #' Unfortunately these are the only parameters that can be kept.
    #' Any other parameters when passed back to the model will change the output.

    structure(
        list(
            components = x$components
        ),
        class = "etscompact"
    )
}


forecast.etscompact <- function(obj, x, h) {
    #' Forecast a compact ets this requires the original data to be sent again
    #' obj : compactets object
    #' x : original data
    #' h : forecast horizon
    modeltype <- paste(obj$components[1], obj$components[2],
                       obj$components[3], sep = "")
    damped <- as.logical(obj$components[4])

    model <- ets(x, model = modeltype, damped = damped)
    fcast <- forecast(model, h)

    return(fcast)
}


ses <- function(x, ...) {
    fcast <- forecast::ets(as.ts(x), "ANN", opt.crit = "mse", ...)
    fcast$method <- fcast$model$method <- "Simple exponential smoothing"
    fcast$model$call <- match.call()
    fcast$series <- deparse(substitute(x))

    structure(
        fcast,
        class = "ses"
    )
}


forecast.ses <- function(obj, h, ...) {
    out <- forecast::forecast.ets(object = obj, h = h, ...)
    out$h <- h
    return(out)
}


update.ses <- function(obj, newdata) {
    #' How we want to update a ses object with new data
    #' We can't since the ses already has a bunch of parameters already set in the ets call
    return(ses(newdata))
}


compact_forecast.ses <- function(obj) {
    structure(
        NA,
        class = "sescompact"
    )
}


forecast.sescompact <- function(obj, x, h) {
    #' Forecast a compact ses this requires the original data to be sent again
    #' obj : compactets object
    #' x : original data
    #' h : forecast horizon
    model <- ses(x)
    fcast <- forecast(model, h)

    return(fcast)
}


theta <- function(x, h = 1, ...) {
    #' The object returned by a thetaf is a forecast object. This means that
    #' there isn't a forecasting function that can be called on it. This object
    #' allows us to then call a forecast on it.
    #' Default h = 1 because we don't care about the forecast yet. This is fast.
    model <- forecast::thetaf(as.ts(x), h = h, ...)

    structure(
        model,
        class = "Theta"
    )
}


forecast.Theta <- function(obj, h, ...) {
    if (length(obj$mean) == h) {
        out <- x
    } else {
        ## Create and return the theta object with the correct forecast horizon
        return(forecast(theta(obj$x, h = h), h = h))
    }
    out$h <- h

    structure(
        out,
        class = "forecast"  # Manually set the class back to forecast
    )
}


update.Theta <- function(obj, newdata) {
    #' How we want to update a Theta object with newdata
    #' We can't since Theta needs to recalculate all the parameteres
    return(theta(newdata))
}


ARIMA <- function(x, ...) {
    #' ARIMA forecast
    return(forecast::auto.arima(as.ts(x), ...))
}


forecast.ARIMA <- function(obj, h, ...) {
    fcast <- forecast::forecast(object = obj, h = h, ...)
    fcast$h <- h
    return(fcast)
}


update.ARIMA <- function(model, newdata, ...) {
    #' Needs to reupdate everything
    #' Unfortunately there's no easy way to get the values of
    #' p,d,q from an existing model
    return(ARIMA(newdata))
}


compact_forecast.ARIMA <- function(obj) {
    structure(
        list(
            NA
        ),
        class = "ARIMAcompact"
    )

}


snaive <- function(x, ...) {
    #' snaive forecast
    fcast <- forecast::snaive(as.ts(x), ...)
    fcast$series <- as.ts(x)

    structure(
        fcast,
        class = "snaive"
    )
}


forecast.snaive <- function(obj, h, ...) {
    out <- forecast::snaive(y = obj$series, h = h)
    out$h <- h
    return(out)
}


update.snaive <- function(model, newdata, ...) {
    return(snaive(newdata))
}


compact_forecast.snaive <- function(obj) {
    structure(
        NA,
        class = "sescompact"
    )
}


## cum_ADIDA_forecast <- function(x, h, f.type = "ets") {
##     f.type <- match.arg(f.type, c("ets", "naive", "SBA.base", "SBA.opt"))

##     ##Create cumulative ADIDA of the series
##     cum <- cum_ADIDA(x, 182)

##     Forecast
##     f1 <- switch(f.type,
##                  naive = forecast::naive(ts(cum, frequency = 7), h)$mean,
##                  ets = predict(forecast::ets(ts(cum, frequency = 7)), h)$mean,
##                  SBA.base = croston_predict(cum, h, w = 0.05, init.opt = FALSE)$frc.out,
##                  SBA.opt = croston_predict(cum, h, init.opt = TRUE, nop = 2)$frc.out)

##     structure(
##         list(
##             model = paste("cumADIDA", f.type, sep = " - "),
##             h = h,
##             out = f1
##         ),
##         class = "MyForecast"
##     )
## }


result_forecast.forecast <- function(fcast) {
    #' Return the numerical forecast result only
    return(as.numeric(fcast$mean))
}


if (FALSE) {

    x <- generate_xts(20)
    y <- x

    foo <- theta(x)
    bar <- theta(y)
    cux <- update(foo, y)
    cux$fitted == bar$fitted
    forecast(foo, 10)


    foo <- croston(x, "SBA.base")
    bar <- croston(y, "SBA.base")
    cux <- update(foo, y)
    cux$frc.in == bar$frc.in
    forecast(foo, 10)
    forecast(foo, 20)

    foo <- croston(x, "SBA.opt")
    bar <- croston(y, "SBA.opt")
    cux <- update(foo, y)
    cux$frc.in == bar$frc.in
    forecast(foo, 10)
    forecast(foo, 20)

    foo <- ets(x)
    bar <- ets(y)
    cux <- update(foo, y)
    cux$fitted == bar$fitted
    forecast(foo, 20)

    foo <- ses(x)
    bar <- ses(y)
    cux <- update(foo, y)
    cux$fitted == bar$fitted
    forecast(foo, 10)
}
