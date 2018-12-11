#' title: error.R
#' comments: error related calculation functions for the project
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(smooth)

#' imports

#' functions
my_mase <- function(a, f)
{
    Q <- sum(abs(diff(a)), na.rm = TRUE) / (length(a) - 1)
    res <- smooth::MASE(a, f, Q)
    return(res)
}


error_calc <- function(a, b, type = "mase")
{
    type <- match.arg(type, c("mase", "mape", "rmse", "mae"))

    err <- switch(type,
                  mase = my_mase(a, b),
                  mape = smooth::MAPE(a, b),
                  rmse = sqrt(smooth::MSE(a, b)),
                  mae = smooth::MAE(a,b))
    return(err)
}
