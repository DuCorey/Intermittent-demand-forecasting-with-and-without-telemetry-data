#' title: cluster.R
#' comments: functions for fetching data necessary for clustering
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(dtwclust)
library(parallel)

#' imports
source("data.R")
source("smooth.R")
source("operators.R")
source("series.R")
source("utils.R")
source("forecast.R")
source("ADIDA.R")

#' functions
client_cluster_con_serie <- function(client, time_scale)
{
    time_scale <- match.arg(time_scale, c("days", "weeks", "months"))

    serie <- client_consumption_ts(client)

    switch(time_scale, weeks = {
        serie %<>% convert_xts_weekly(.)
    }, days = {
        serie %<>% convert_xts_daily(.)
    }, months = {
        serie %<>% convert_xts_monthly(.)
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


cluster_data <- function(cvd, source, time_scale)
{
    ## Get the data
    print("Fetching delivery series")
    del_orig <- get_del_series(cvd, source = source, time_scale = time_scale)

    print("Fetching consumption series")
    con_orig <- get_con_series(cvd, time_scale = time_scale)

    print("Preprocessing delivery data")
    del <- mclapply(del_orig,
                    pryr::partial(filter_year, time_scale = time_scale) %o% my_croston,
                    mc.cores = 8)

    print("Preprocessing consumption data")
    con <- mclapply(con_orig,
                    pryr::partial(filter_year, time_scale = time_scale) %o% my_ets,
                    mc.cores = 8)

    ## Combine both truth series of not null values for del and con
    f <- not %o% is.null
    ind <- sapply(del, f) & sapply(con, f)
    del <- del[ind]
    con <- con[ind]
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]


    ## Structuring the final output so  that it's a list
    ## This is so ugly that I feel like I did something wrong somewhere
    del <- mapply(list, orig = del_orig, smooth = del, SIMPLIFY = FALSE)
    con <- mapply(list, orig = con_orig, smooth = con, SIMPLIFY = FALSE)
    res <- mapply(list, cvd = cvd, del = del, con = con , SIMPLIFY = FALSE)
    return(res)
}


del_clus_series <- function(clus_data, type)
{
    #' Return deliveries series from the clustering data
    type <- match.arg(type, c("orig", "smooth"))

    switch(type,
           "orig"   = lapply(clus_data, function(x) x$del$orig),
           "smooth" = lapply(clus_data, function(x) x$del$smooth))
}


con_clus_series <- function(clus_data, type)
{
    #' Return deliveries series from the clustering data
    type <- match.arg(type, c("orig", "smooth"))

    switch(type,
           "orig"   = lapply(clus_data, function(x) x$con$orig),
           "smooth" = lapply(clus_data, function(x) x$con$smooth))
}


mv_serie <- function(con, del, pad = NULL)
{
    #' Return a multivate serie when given a con and del series
    merged <- merge(con, del) %>%
        as.matrix(.)  # Conver to a matrix so we can add padding later

    ## Clean up and remove time index
    rownames(merged) <- NULL

    ## Remove NAs, can't use drop NA since this is a matrix
    merged <- merged[complete.cases(merged),]

    ## Add some 0 padding for the mv clustering method
    if (!is.null(pad)) {
        zeros <- rep(0, times = pad)
        padding <- as.matrix(data.frame(con = zeros, del = zeros))
        merged <- rbind(padding, merged, padding)
    }

    return(merged)
}


mv_series <- function(clus_data, pad = NULL)
{
    #' Produce multivariate series from the cluster_data
    return(lapply(clus_data, function(x) mv_serie(x$con$smooth, x$del$smooth, pad)))

    ## TODO - Proabbly gonna need to track the amount of padding added to the series
    ## for use later on.
}


mv_series_con <- function(series)
{
    #' Return the consumption part of the multivariate series
    return(lapply(series, function(x) x[,"con"]))
}


mv_series_del <- function(series)
{
    #' Return the delivery part of the multivariate series
    return(lapply(series, function(x) x[,"del"]))
}


## TODO - Can we extend this to time series of less than 1 year or of mismatched time?
## Looks probable as the distance methods accept variable lenghts
my_clustering <- function(clus_data, type, k, distmat = NULL, ...)
{
    type <- match.arg(type, c("mv", "del"))
    switch(type,
           "mv" = {
               series <- mv_series(clus_data)
           }, "del" = {
               ## Convert to numeric probably
               series <- del_clus_series(clus_data, "smooth")
           })

    if (k != 1) {
        clus <- dtwclust::tsclust(series,
                                  type = "hierarchical",
                                  preproc = zscore,
                                  k = k,
                                  distance = "gak",
                                  trace = TRUE,
                                  centroid = shape_extraction,
                                  control = hierarchical_control(method = "ward.D2",
                                                                 distmat = distmat),
                                  ...)
                      ## args = tsclust_args(dist = list(window.size = 2,
        ##                                 step.pattern = symmetric1)))


        ## Add the original series in the object's attributes
        attributes(clus)$orig_data <- series

    } else {
        #' Allow for k = 1 clustering even though the original function complains about it
        ## Run it normally with 2 (fastest clustering)
        clus <- my_clustering(clus_data, type, k = 2, distmat)

        ## Overwrite the cluster member so that they are all 1
        attributes(clus)$cluster <- rep(1, times = length(clus@cluster))

        ## Overwrtie the clustering method to only return 1
        clus@family@cluster <- function(distmat = NULL, ...)
        {
            return(rep(1, nrow(distmat)))
        }
    }
    return(clus)
}


## The order of the columns in the centroids follows the order of the mv serie
## con is column 1
## del is column 2
con_centroids <- function(clus)
{
    #' Return the centroids of the consumption sequence of the mv data
    return(lapply(clus@centroids, function(x) x[,1]))
}


del_centroids <- function(clus)
{
    #' Return the centroids of the delivery sequence of the mv data
    return(lapply(clus@centroids, function(x) x[,2]))
}


mv_del_cluster_predictions <- function(newdata, clus)
{
    #' Predict which cluster to assign new_data
    nm <- names(newdata)

    ## Apply the preprocissing function on the data
    newdata <- quoted_call(
        clus@family@preproc,
        newdata,
        dots = subset_dots(clus@args$preproc, clus@family@preproc)
    )

    ## Calculate the distance matrix to the delivery centroids
    dist_mat <- quoted_call(
        clus@family@dist,
        x = newdata,
        centroids = del_centroids(clus),
        dots = clus@args$dist
    )

    ## Apply the clustering function
    ret <- clus@family@cluster(distmat = dist_mat, m = clus@control@fuzziness)
    names(ret) <- nm

    return(ret)
}


clus_shapes <- function(clus, method = "mean")
{
    #' How we determine the shapes of our cluster so we can use them in our
    #' future predictions

    method <- match.arg(method, c("mean", "unzscore"))

    clus_con_orig <- mv_series_con(clus@orig_data)
    clus_ids <- unique(clus@cluster)

    switch(method,
           "mean" = {
               ##' Method 1 - mean
               ## Instead of using the centroid of the consumption part of the cluster which is z_scored,
               ## our shape will be the mean of the original consumption data in the cluster.
               ## We can try using the shape extraction algorithm on the non scaled cluster,
               ## but this does not work as the resulting series will be z normalised.

               ## Determine the mean series for each cluster of the original consumption data
               shapes <- lapply(clus_ids,
                                function(x) series_agg(clus_con_orig[clus@cluster == x]))
           }, "unzscore" = {
               ##' Method 2 - unzscore
               ## Since we scaled initially with the zscore we first perform an average 'unzscore'
               ## using all the members of the cluster.
               ## Secondly, we can normalize the serie with the delivery data to conserve mass
               ## A problem with this method is there's no guarantee that the unzscored
               ## groups won't still contain negative values

               ## Getting the mean and sd for the consumption data of each cluster
               clus_mean <- sapply(clus_ids,
                                   function(x) mean(sapply(clus_con_orig[clus@cluster == x], mean)))
               clus_sd <- sapply(clus_ids,
                                 function(x) mean(sapply(clus_con_orig[clus@cluster == x], sd)))

               ## Apply the un_z_score to each centroid using the clus_mean and clus_sd
               centroids <- del_centroids(clus)
               shapes <- lapply(clus_ids,
                                function(x) un_z_score(centroids[[x]], clus_mean[[x]], clus_sd[[x]]))
           })

    return(shapes)
}


mv_clus_con_prediction <- function(clus, new_dels, orig_dels, shape = "mean")
{
    #' Predict a consumption series for each element of new data
    ## Predict a cluster for each delivery series
    pred_clus <- mv_del_cluster_predictions(new_dels, clus)

    ## Determine the shapes that will be used for the predictions
    shapes <- clus_shapes(clus, shape)

    ## The predicted con is simply the shape associate to that cluster
    pred_con <- lapply(pred_clus, function(x) shapes[[x]])

    ## Normalize the consumption prediction with the original delivery data
    pred_con_norm <- mapply(function(x,y) xts(normalize_relative(x,y), order.by = index(y)),
                       pred_con, orig_dels, SIMPLIFY = FALSE)

    ## Ugly list formatting again
    res <- mapply(list,
                  cluster = pred_clus,
                  series = pred_con_norm,
                  SIMPLIFY = FALSE)

    return(res)
}


## get_clus_DPs <- function(clus)
## {
##     return(sapply(clus$cvd, function(x) x$DP))
## }


## filter_clus <- function(clus, set)
## {
##     ind <- get_clus_DPs(clus) %in% set

##     clus$cvd %<>% .[ind]
##     clus$con$series %<>% .[ind]
##     clus$con$orig %<>% .[ind]
##     clus$del$series %<>% .[ind]
##     clus$del$orig %<>% .[ind]

##     return(clus)
## }


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

if (FALSE) {  # Prevents it from running when sourcing the file
    #' Testing dtw
    library(dtw)
    source("error.R")

    a <- ts(sin(seq(0, 2*pi, length.out = 100)))
    b <- ts(sin(seq(pi, 2*pi, length.out = 50)))
    foo <- dtw(a, b, window.type = "slantedband", window.size = 2)
    foo <- dtw(a, b)
    dtwPlotTwoWay(foo, a, b)
    dtwPlotThreeWay(foo, a, b)

    #' This is to show equivalency between dtw_basic and dtw::dtw
    dtw_basic(series[[1]], series[[2]], norm = "L1", step.pattern = symmetric1, window.size = 2)
    dtw::dtw(proxy::dist(series[[1]], series[[2]], "L1"), step.pattern = symmetric1, window.type = "slantedband", window.size = 2, distance.only = TRUE)$distance

    #' Testing multivariate z_score from dtwcust
    series <- mv_series(clus_tel_day)
    foo <- dtwclust::zscore(series[1:2])[[1]][,"con"]
    bar <- z_score(series[[1]][,"con"])
    all.equal(foo, bar)  # You cannot use '==' but must instead you all.equal for numerics

    #' Double checking equivalency between zscore
    foo <- clus@datalist[[1]][,"con"]
    bar <- clus@orig_data[[1]][,"con"]
    all.equal(dtwclust::zscore(bar), foo)

    #' Testing the scaling method
    clus_tel_day <- readRDS("../data/master/clus_tel_day.rds")
    series <- mv_series(clus_tel_day)
    normalize_relative(series[[1]][,'con'], clus_tel_day[[1]]$del$orig)
    foo <- series[[1]][,'con']
    bar <- normalize_relative(series[[1]][,'con'], clus_tel_day[[1]]$con$orig)

    #' Comparing the smoothing on the consumption data
    clus_tel_day[[1]]$con$smooth
    clus_tel_day[[1]]$con$orig %>%
        my_ets(.) %>%
        filter_year(., "days")

    #' Testing clustering
    clus <- my_clustering(train[1:10], type = "mv", k = 2)
    new_data <- train[1]

    new_dels <- del_clus_series(new_data, "smooth") %>%
        ## Convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., as.numeric)

    pred_clus <- mv_del_cluster_predictions(new_dels, clus)

    shapes <- clus_shapes(clus, "mean")


    orig_dels <- del_clus_series(new_data, "orig") %>%
        lapply(., filter_year, time_scale = "days")

    pred_con_norm <- mapply(function(x,y) xts(normalize_relative(x,y), order.by = index(y)),
                            pred_con, orig_dels, SIMPLIFY = FALSE)

    real_con <- con_clus_series(new_data, "orig") %>%
        lapply(., filter_year, time_scale = "days")

    err <- mapply(function(x,y) error_calc(x, y, "rmse"),
                  pred_con_norm, real_con, SIMPLIFY = FALSE)


    plot_mult_xts(make_xts(mean_clus[[1]]), make_xts(clus_con_orig[[1]]))

    plot_mult_xts(real_con[[1]], pred_con_norm[[1]])

    plot_mult_xts(append(
        lapply(clus_con_orig[clus@cluster == 1], as.xts %o% ts),
        list(as.xts(ts(mean_clus[[1]])))
    ))


    plot_mult_xts(make_xts(z_score(mean_clus[[1]])), make_xts(con_centroids(clus)[[1]]))

    plot(clus, type = "sc", 1)


}
