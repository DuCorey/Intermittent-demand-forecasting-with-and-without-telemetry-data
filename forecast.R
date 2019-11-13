#' title: forecast.R
#' comments: In sample and out sample forecasting functions
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(forecast)


#' imports
source("series.R")

#' functions
my_ets <- function(data, ...)
{
    #' Wrapper function over forecast::ets
    if (nrow(data) < 1) {
        warning("ets requires a minimum of 1 value in the time series. Returning NULL.")
        return(NULL)
    }

    ## Convert data to numeric since the method don't like to take xts input
    res <- fitted(forecast::ets(as.numeric(data), ...)) %>%
        xts(., order.by = index(data))

    attr(res, 'frequency') <- frequency(data)

    return(res)
}
