#' title: error.R
#' comments: error related calculation functions for the project
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(smooth)

#' imports

#' functions
my_MASE <- function(x, f) {
    #' Mean Absolute Scaled Error
    Q <- sum(abs(diff(x)), na.rm = TRUE) / (length(x) - 1)
    res <- greybox::MASE(x, f, Q)
    return(res)
}


my_sME <- function(x, f, scale) {
    #' scaled Mean Error
    res <- mean(x - f, na.rm = TRUE)/scale
    return(res)
}


my_sMAE <- function(x, f, scale) {
    #' scaled Mean Absolute Error
    return(greybox::MAE(x, f)/scale)
}


my_sMSE <- function(x, f, scale) {
    #' scaled Mean Squared Error
    res <- mean(((x - f)/scale)**2)
    return(res)
}


my_PIS <- function(x, f) {
    #' Period In Stock
    return(sum(cumsum(f - x)))
}


my_sPIS <- function(x, f, scale) {
    #' scaled Period In Stock
    return(my_PIS(x,f)/scale)
}


my_sAPIS <- function(x, f, scale) {
    #' scaled Absolute Period In Stock
    return(abs(my_PIS(x,f))/scale)
}


error_calc <- function(x, f, type, scale = NULL) {
    type <- match.arg(type, c("MAE", "RMSE", "MASE", "sME", "sMAE", "sMSE", "PIS", "sPIS", "sAPIS"))

    x %<>% as.numeric
    f %<>% as.numeric
    err <- switch(type,
                  MAE = greybox::MAE(x, f),
                  RMSE = sqrt(greybox::MSE(x, f)),
                  MASE = my_MASE(x, f),
                  sME = my_sME(x, f, scale),
                  sMAE = my_sMAE(x, f, scale),
                  sMSE = greybox::sMSE(x, f, scale),
                  PIS = my_PIS,
                  sPIS = greybox::sPIS(x, f, scale),
                  sAPIS = my_sAPIS)

    return(err)
}
