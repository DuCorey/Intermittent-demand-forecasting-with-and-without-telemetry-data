#' title: ndusl.R
#' comments: calculating the next day under safety level
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages

#' imports

#' functions
next_day_under_safety_level <- function(x, ...) {
    UseMethod("next_day_under_safety_level", x)
}


day_before_safety_level <- function(x, ...) {
     UseMethod("day_before_safety_level", x)
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


day_before_safety_level.numeric <- function(con, start_tel, safety_level) {
    res <- next_day_under_safety_level.numeric(con, start_tel, safety_level)
    if (is.na(res)) {
        return(NA)
    } else {
        ## We want the day and the amount just before we reached safety level
        a <- res - 1
        amount <- sum(head(con, a))

        return(c(a, amount))
    }
}


next_day_under_safety_level.xts <- function(con, del, start_tel, safety_level) {
    #' Given an xts series for the consumption, a delivery, a start_tel, and a safety_level
    #' calculate the time until we reach the safety_level starting after the del
    tryCatch({
        amount <- 0
        a <- 0
        safety_stock <- start_tel - safety_level
        while (amount <= safety_stock) {
            a <- a + 1
            amount <- amount + as.numeric(con[del + a])
        }
        return(a)
    },
    ## If the loop goes out-of-bounds we catch and return NA
    ## The loop goes out of bounds if there isn't any consumption
    ## data available at that date
    error = function(cond) {
        return(NA)
    })
}


day_before_safety_level.xts <- function(con, date, start_tel, safety_level) {
    res <- next_day_under_safety_level.xts(con, date, start_tel, safety_level)

    if (is.na(res)) {
        return(NA)
    } else {
        ## We want the day and the amount just before we reached safety level
        a <- res - 1
        amount <- sum(con[xts_range(date+1, date+a)])

        return(structure(
            list(
                date = date + a,
                amount = amount
            )
        ))
    }
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
