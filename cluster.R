#' title: data.R
#' comments: this file creates the data objects necessary for
#' author: Corey Ducharme / corey.ducharme@polymtl.ca
#' input: the excel files which contain the deliveries stored in the report folder
#' output: a csv which contains the deliveries for all clients for all years

#' packages
library(dtw)
library(dtwclust)
library(xts)

source("data.R")

#' parallel if required
## Create parallel workers
## library(doParallel)
## cl <- makeCluster(detectCores() - 1)
## invisible(clusterEvalQ(cl, library(dtwclust)))
## registerDoParallel(cl)


#' functions
z_score <- function(ts)
{
    return((ts - mean(ts))/sd(ts))
}


normalize <- function(ts)
{
    return((ts-min(ts))/(max(ts)-min(ts)))
}


## distance_matrix <- function(data, fun)
## {
##     series <- lapply(data, z_transform %o% fun)
##     ## Since the time series are in days the lag.max is 14
##     dm <- TSdist::TSDatabaseDistances(series, distance = "dtw", lag.max = 14)
##     return(dm)
## }


convert_xts_weekly <- function(serie)
{
    res <- as.xts(serie[[2]], order.by = as.Date(serie[[1]])) %>%
        xts::apply.weekly(., sum)
    attr(res, 'frequency') <- 365.25/7
    return(res)
}


convert_xts_daily <- function(serie)
{
    res <- as.xts(serie[[2]], order.by = as.Date(serie[[1]])) %>%
        xts::apply.daily(., sum)
    attr(res, 'frequency') <- c(7, 365.25)
    return(res)
}


convert_xts_monthly <- function(serie)
{
    res <- as.xts(serie[[2]], order.by = as.Date(serie[[1]])) %>%
        xts::apply.monthly(., sum)
    attr(res, 'frequency') <- 12
    return(res)
}


client_cluster_con_serie <- function(client, time_scale)
{
    time_scale <- match.arg(time_scale, c("days", "weeks", "months"))

    serie <- client_consumption_ts(client)

    switch(time_scale, weeks = {
        serie <- convert_xts_weekly(serie)
    }, days = {
        serie <- convert_xts_daily(serie)
    }, months = {
        serie <- convert_xts_monthly(serie)
    })

    serie %<>% trim_ts(., n = 1) %>%
        z_score
    return(serie)
}


get_con_series <- function(cvd, time_scale)
{
    series <- lapply(cvd, client_cluster_con_serie,
                     time_scale = time_scale)
    return(series)
}


client_cluster_del_serie <- function(client, source, time_scale)
{
    source <- match.arg(source, c("raw", "tel"))
    time_scale <- match.arg(time_scale, c("days", "weeks", "months"))

    serie <- client_delivery_ts(client, source = source) %>%
        add_missing_days

    switch(time_scale, weeks = {
        serie <- convert_xts_weekly(serie)
    }, days = {
        serie <- convert_xts_daily(serie)
    }, months = {
        serie <- convert_xts_monthly(serie)
    })

    ##serie %<>% z_score
    return(serie)
}


get_del_series <- function(cvd, source, time_scale)
{
    source <- match.arg(source, c("raw", "tel"))
    series <- lapply(cvd, client_cluster_del_serie,
                     source = source, time_scale = time_scale)
    return(series)
}


filter_year <- function(serie, time_scale, year = "2016")
{
    time_scale <- match.arg(time_scale, c("weeks", "days", "months"))

    #' Filter time series for a specific year
    filt <- serie[format(index(serie), "%Y") == as.character(year)]
    n_periods <- switch(time_scale,
                        weeks = 52, days = 365, months = 12)

    if (nrow(filt) >= n_periods) {
        return(filt)
    } else {
        return(NULL)
    }
}


filter_cvd_clustering <- function(cvd, source, time_scale)
{
    filt_con <- not %o% is.null %o%
        pryr::partial(filter_year, time_scale = time_scale) %o%
        pryr::partial(client_cluster_con_serie, time_scale = time_scale)
    filt_del <- not %o% is.null %o%
        pryr::partial(filter_year, time_scale = time_scale) %o%
        pryr::partial(client_cluster_del_serie, source = source, time_scale = time_scale)

    cvd <- Filter(filt_con, cvd) %>%
        Filter(filt_del, .)
    return(cvd)
}


mv_serie <- function(client, source, time_scale)
{
    con <- client_cluster_con_serie(client, time_scale)
    del <- client_cluster_del_serie(client, source, time_scale)

    merged <- merge(con, del)
    ## Remove NAs, can't use drop NA since this is a xts object
    merged <- merged[complete.cases(merged),]

    return(merged)
}


mv_series <- function(cvd, time_scale, source)
{
    return(lapply(cvd, mv_serie, source = source, time_scale = time_scale))
}


get_cluster_del_series <- function(cvd, source, time_scale)
{
    series <- get_del_series(cvd, source = source, time_scale = time_scale)
    res <- lapply(series, filter_year,
                  time_scale = time_scale)
    return(res)
}


get_cluster_con_series <- function(cvd, time_scale)
{
    series <- get_con_series(cvd, time_scale = time_scale)
    res <- lapply(series, filter_year,
                  time_scale = time_scale)
    return(res)
}


prep_data_clus <- function(cvd, source, time_scale)
{
    #' Speed up the execution of cluster preparation by merging the filtering
    #' and the data formatting
    del <- get_cluster_del_series(cvd, source, time_scale)
    con <- get_cluster_con_series(cvd, time_scale)

    ## Combine the both truth series of not null values for del and con
    f <- not %o% is.null
    ind <- sapply(del, f) & sapply(con, f)

    del <- del[ind]
    con <- con[ind]
    cvd <- cvd[ind]

    return(list(cvd=cvd, del=del, con=con))
}


#' Playing with dtwclust, hashing and memoization
## system.time(x <- serialize(cvd, NULL))
## system.time(unserialize(x))
## library(digest)
## tempdir()

## foo <- compact(get_cluster_con_series(cvd[1:10], "weeks"))

## digest(foo)
## memo_tsclust <- function(x, ..., cache = makeCache())
## {

##     dtw_cache <- cache

##     #' digest the arg
##     hash <- digest(x)

##     #' check if the hash is already in the environment
##     if is.null(dtw_cache$get(x) {

##     } else {
##         dtw_cache[[hash]] <- "foo"
##     }
## }


## makeCache <- function()
## {
##     cache <- new.env(parent=emptyenv())
##     list(get = function(key) cache[[key]],
##          set = function(key, value) cache[[key]] <- value)
## }

## a <- makeCache()

if (FALSE) {  # Prevents it from running when sourcing the file
    #' Testing dtw
    a <- ts(sin(seq(0, 2*pi, length.out = 100)))
    b <- ts(sin(seq(pi, 2*pi, length.out = 50)))
    foo <- dtw(a, b, window.type = "slantedband", window.size = 2)
    foo <- dtw(a, b)
    dtwPlotTwoWay(foo, a, b)
    dtwPlotThreeWay(foo, a, b)


    #' This is to show equivalency between dtw_basic and dtw::dtw
    dtw_basic(series[[1]], series[[2]], norm = "L1", step.pattern = symmetric1, window.size = 2)
    dtw::dtw(proxy::dist(series[[1]], series[[2]], "L1"), step.pattern = symmetric1, window.type = "slantedband", window.size = 2, distance.only = TRUE)$distance
}
