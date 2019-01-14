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

    err <- switch(type,
                  mase = my_mase(x, f),
                  mape = smooth::MAPE(x, f),
                  rmse = sqrt(smooth::MSE(x, f)),
                  mae = smooth::MAE(x, f))
    return(err)
}
