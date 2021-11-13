#' title: error.R
#' comments: error related calculation functions for the project
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(greybox)

#' imports

#' functions
my_MASE <- function(forecast, train, test, period = 1) {
    #' Mean Absolute Scaled Error
    ## forecast - forecasted values
    ## train - data used for forecasting .. used to find scaling factor
    ## test - actual data used for finding MASE.. same length as forecast
    ## period - in case of seasonal data.. if not, use 1

    forecast <- as.vector(forecast)
    train <- as.vector(train)
    test <- as.vector(test)

    n <- length(train)
    scalingFactor <- sum(abs(train[(period+1):n] - train[1:(n-period)])) / (n-period)

    et <- abs(test-forecast)
    qt <- et/scalingFactor
    MASE <- mean(qt)
    return(MASE)
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


my_MAAPE <- function(x, f) {
    #' Mean Arctangent Absolute Percentage Error
    #' Kim, S., & Kim, H. (2016). A new metric of absolute percentage error for intermittent demand forecasts. International Journal of Forecasting, 32(3), 669-679.
    return(mean(atan(abs((x - f)/x)), na.rm = TRUE))
}


my_mMAPE <- function(x, f) {
    #'  Mean-based error measures for intermittent demand forecasting
    #' Prestwich, S., Rossi, R., Armagan Tarim, S., Hnich, B., 2014. Mean-based error measures for intermittent demand forecasting. International Journal of Production Research 52, 6782-6791.
    x_mean <- mean(x)
    return(mean(abs((x_mean - f)/x_mean), na.rm = TRUE))
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
