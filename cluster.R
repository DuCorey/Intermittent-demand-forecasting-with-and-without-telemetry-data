#' title: cluster.R
#' comments: functions for fetching data necessary for clustering
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(dtw)
library(dtwclust)

#' imports
source("data.R")
source("smooth.R")
source("operators.R")
source("series.R")

#' functions
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

    serie %<>% trim_ts(., n = 1)
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
        serie <- convert_xts_weekly(serie) %>%
            trim_ts(., n = 1, how = "end")
    }, days = {
        serie <- convert_xts_daily(serie)
    }, months = {
        serie <- convert_xts_monthly(serie) %>%
            trim_ts(., n = 1, how = "end")
    })

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
    if (is.null(serie)) {
        return(NULL)
    }

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


cluster_data <- function(cvd, source, time_scale) {
    ## Get the data
    print("Fetching delivery series")
    del_orig <- get_del_series(cvd, source = source, time_scale = time_scale)

    print("Fetching consumption series")
    con_orig <- get_con_series(cvd, time_scale = time_scale)

    print("Preprocessing delivery data")
    del <- mclapply(del_orig,
                    pryr::partial(filter_year, time_scale = time_scale) %o% z_score %o% my_croston,
                    mc.cores = 8)

    print("Preprocessing consumption data")
    con <- mclapply(con_orig,
                    pryr::partial(filter_year, time_scale = time_scale) %o% z_score %o% my_ets,
                    mc.cores = 8)

    ## Combine the both truth series of not null values for del and con
    f1 <- not %o% is.null
    ind <- sapply(del, f1) & sapply(con, f1)
    del <- del[ind]
    con <- con[ind]
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]

    structure(
        list(
            cvd = cvd,
            del = list(orig = del_orig,
                       smooth = del),
            con = list(orig = con_orig,
                       smooth = con)
        )
    )
}


my_clustering <- function(x, distmat = NULL)
{
    dtwclust::tsclust(x,
                      type = "hierarchical",
                      k = 5,
                      distance = "dtw_basic",
                      trace = TRUE,
                      control = hierarchical_control(method = "ward.D2",
                                                     distmat = distmat),
                      args = tsclust_args(dist = list(window.size = 2,
                                                      step.pattern = symmetric1)))
}


get_clus_DPs <- function(clus)
{
    return(sapply(clus$cvd, function(x) x$DP))
}


filter_clus <- function(clus, set)
{
    ind <- get_clus_DPs(clus) %in% set

    clus$cvd %<>% .[ind]
    clus$con$series %<>% .[ind]
    clus$con$orig %<>% .[ind]
    clus$del$series %<>% .[ind]
    clus$del$orig %<>% .[ind]

    return(clus)
}


## distance_matrix <- function(data, fun)
## {
##     series <- lapply(data, z_transform %o% fun)
##     ## Since the time series are in days the lag.max is 14
##     dm <- TSdist::TSDatabaseDistances(series, distance = "dtw", lag.max = 14)
##     return(dm)
## }


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

#' main
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
