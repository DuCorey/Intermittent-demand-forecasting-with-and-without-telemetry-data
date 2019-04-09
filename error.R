#' title: error.R
#' comments: error related calculation functions for the project
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(smooth)

#' imports

#' functions
my_mase <- function(x, f)
{
    Q <- sum(abs(diff(x)), na.rm = TRUE) / (length(x) - 1)
    res <- smooth::MASE(x, f, Q)
    return(res)
}


error_calc <- function(x, f, type)
{
    type <- match.arg(type, c("mase", "mape", "rmse", "mae"))

    x %<>% as.numeric
    f %<>% as.numeric
    err <- switch(type,
                  mase = my_mase(x, f),
                  mape = smooth::MAPE(x, f),
                  rmse = sqrt(smooth::MSE(x, f)),
                  mae = smooth::MAE(x, f))
    return(err)
}


next_day_under_safety_level <- function(del, con, peak, safety_level) {
    amount <- 0
    con_start <- which(index(con) == date(del))
    a <- 0
    safety_stock <- peak - safety_level

    while(amount <= peak) {
        amount <- amount + con[[con_start + a]]
        a <- a + 1
    }

    ## We return the date difference
    return(as.numeric(index(con[con_start + a - 1]) - del))
}


try_next_day_under_safety_level <- function(del, con, peak, safety_level) {
    res <- tryCatch({
        next_day_under_safety_level(del, con, peak, safety_level)
    }, error = function(cond) {
        return(NA)
    })
    return(res)
}


ndusl_metric <- function(table)
{
    #' Return a single value to evaluate the accuracy of the next day under
    #' safety level performance metric
    return(as.numeric(abs(as.numeric(names(table))) %*% as.numeric(table)))
}
