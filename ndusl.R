#' title: ndusl.R
#' comments: calculating the next day under safety level
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages

#' imports

#' functions
next_day_under_safety_level <- function(x, ...) {
    UseMethod("next_day_under_safety_level", x)
}


next_day_under_safety_level.numeric <- function(con, start_tel, safety_level) {
    #' Given a consumption series and a starting telemetry amount, how long
    #' until we reach the safety_level
    #' con: a numeric vector containing the consumption
    amount <- 0
    a <- 0
    safety_stock <- start_tel - safety_level

    while(amount <= safety_stock) {
        a <- a + 1
        amount <- amount + con[[a]]

    }

    return(a)
}


next_day_under_safety_level.MyForecast <- function(x, ...) {
    #' Determine the safety level of a MyForecast
    return(next_day_under_safety_level(as.numeric(x$out), ...))
}


next_day_under_safety_level.MyForecastsList <- function(x, ...) {
    return(lapply(x, next_day_under_safety_level, ...))
}


ndusl_metric <- function(table) {
    #' Return a single value to evaluate the accuracy of the next day under
    #' safety level performance metric
    return(as.numeric(abs(as.numeric(names(table))) %*% as.numeric(table)))
}
