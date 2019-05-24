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
source("error.R")

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
        as.matrix(.)  # We can't add padding to an xts object so we convert to matrix

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


data_smooth_dels <- function(data)
{
    res <- del_clus_series(data, "smooth") %>%
        ## Carefully convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., function(x) as.matrix(x)[,1])

    return(res)
}


data_orig_dels <- function(data)
{
    res <- del_clus_series(data, "orig") %>%
        lapply(., filter_year, time_scale = "days")
    return(res)
}


data_smooth_con <- function(data)
{
    res <- con_clus_series(data, "smooth") %>%
        lapply(., function(x) as.matrix(x)[,1])
    return(res)
}


data_orig_con <- function(data)
{
    res <- con_clus_series(data, "orig") %>%
        ## Carefully convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., filter_year, time_scale = "days")
    return(res)
}


## TODO - Can we extend this to time series of less than 1 year or of mismatched time?
## Looks probable as the distance methods accept variable lenghts
my_clustering <- function(clus_data, series_type, k, trace = TRUE, ...)
{
    series_type <- match.arg(series_type, c("mv", "del"))
    switch(series_type,
           "mv" = {
               series <- mv_series(clus_data)
           }, "del" = {
               ## Convert to numeric probably
               series <- del_clus_series(clus_data, "smooth")
           })

    if (k != 1) {
        clus <- dtwclust::tsclust(series,
                                  trace = trace,
                                  k = k,
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


## del_datalist <- function(clus)
## {
##     #' Return the delivery sequence of the mv data in the cluster original data
##     return(lapply(clus@datalist, function(x) x[,"del"]))
## }


clus_preproc <- function(newdata, clus)
{
    ## Apply the preprocissing function on the data
    newdata <- quoted_call(
        clus@family@preproc,
        newdata,
        dots = subset_dots(clus@args$preproc, clus@family@preproc)
    )
    return(newdata)
}


clus_calc_distance_matrix <- function(newdata, clus, centroids)
{
    ## Calculate the distance matrix between the newdata and the centroids
    dist_mat <- quoted_call(
        clus@family@dist,
        x = newdata,
        centroids = centroids,
        dots = clus@args$dist
    )

    return(dist_mat)
}


mv_del_cluster_predictions <- function(newdata, clus)
{
    #' Predict which cluster to assign new_data
    nm <- names(newdata)
    ## Preprocess the newdata with the cluster's preprocessing function
    newdata <- clus_preproc(newdata, clus)

    ## Calculate the distance matrix with the cluster's distance function
    dist_mat <- clus_calc_distance_matrix(newdata, clus, del_centroids(clus))

    ## Apply the cluster's clustering function
    ret <- clus@family@cluster(distmat = dist_mat, m = clus@control$fuzziness)
    names(ret) <- nm

    return(ret)
}


clus_shapes <- function(clus, method, con_shape_series)
{
    #' How we determine the shapes of our cluster so we can use them in our
    #' future predictions
    method <- match.arg(method, c("mean", "unzscore", "medoid", "centroid"))

    clus_ids <- 1:clus@k

    switch(method,
           "mean" = {
               ##' Method 1 - mean
               ## Instead of using the centroid of the consumption part of the cluster which is z_scored,
               ## our shape will be the mean of the original consumption data in the cluster.
               ## We can try using the shape extraction algorithm on the non scaled cluster,
               ## but this does not work as the resulting series will be z normalised.

               ## Determine the mean series for each cluster of the original consumption data
               shapes <- lapply(clus_ids,
                                function(x) series_agg(con_shape_series[clus@cluster == x], mean))
           }, "unzscore" = {
               ##' Method 2 - unzscore
               ## Since we scaled initially with the zscore we first perform an average 'unzscore'
               ## using all the members of the cluster.
               ## Secondly, we can normalize the serie with the delivery data to conserve mass
               ## A problem with this method is there's no guarantee that the unzscored
               ## groups won't still contain negative values

               ## Getting the mean and sd for the consumption data of each cluster
               clus_mean <- sapply(clus_ids,
                                   function(x) mean(sapply(con_shape_series[clus@cluster == x], mean)))
               clus_sd <- sapply(clus_ids,
                                 function(x) mean(sapply(con_shape_series[clus@cluster == x], sd)))

               ## Apply the un_z_score to each centroid using the clus_mean and clus_sd
               centroids <- del_centroids(clus)
               shapes <- lapply(clus_ids,
                                function(x) un_z_score(centroids[[x]], clus_mean[[x]], clus_sd[[x]]))
           }, "medoid" = {
               ##' Method 3 - The original serie of the medoid of the cluster
               medoids_id <- lapply(clus_ids,
                                    function(x) attributes(pam_cent(clus@datalist,
                                                                    ids = which(clus@cluster == x),
                                                                    distmat = clus@distmat))$series_id
                                 )
               shapes <- lapply(medoids_id, function(x) con_shape_series[[x]])
           }, "centroid" = {
               ##' Method 4 - The original centroid
               ## Simply return the originaly calculated centroid.
               ## Useful when there is no data preprocessing step in the clustering
               shapes <- con_centroids(cluster)
           })

    return(shapes)
}


my_nearest_neighbors <- function(i, distance_matrix, k)
{
    #' Return the nearest_neighbors for the ith row in the distance_matrix
    return(order(distance_matrix[i, ])[1:k])
}


nn_shape <- function(method, con_shape_series)
{
    #' How we determine the shape of the nearest neighbors group of series
    method <- match.arg(method, c("mean"))

    switch(method,
           "mean" = {
               ##' Method 1 - mean
               ## The shape is the mean of the series
               shape <- series_agg(con_shape_series, mean)
           }, "shape_extraction" = {
               ##' Method 2 - shape extraction algorithm
               ## The resulting serie will be z_normalized so that won't work
               shape <- shape_extraction(con_shape_series)
           })

    return(shape)
}


#' This is dangerous you shouldn't ever need to do something like this.
## is_multivariate <- function(x) {
##     if (length(x) == 0L) {
##         stop("Empty list of series received.") # nocov
##     }

##     ncols <- sapply(x, NCOL)

##     if (any(diff(ncols) != 0L)) {
##         stop("Inconsistent dimensions across series.")
##     }

##     any(ncols > 1L)
## }


## my_is_multivariate <- function(x)
## {
##     return(TRUE)
## }


## assignInNamespace("is_multivariate", is_multivariate, "dtwclust")

## getFromNamespace("is_multivariate", "dtwclust")


mv_clus_con_prediction <- function(clus, new_dels, shape, con_shape_series)
{
    ## Predict a cluster for each delivery series
    pred_clus <- mv_del_cluster_predictions(new_dels, clus)

    ## Determine the shapes for each cluster
    shapes <- clus_shapes(clus, shape, con_shape_series)

    ## The predicted con is simply the shape associate to that cluster
    pred_con <- lapply(pred_clus, function(x) shapes[[x]])

    return(pred_con)
}


mv_knn_con_prediction <- function(shape, con_shape_series, k, distmat)
{
    #' Find the nearest neighbors for each delivery series and create a shape
    #' from the original consumption of those nearest neighbors
    ## We need to find the nearest_neighbor for each new delivery
    NNs <- lapply(1:nrow(distmat), function(i) my_nearest_neighbors(i, distmat, k))

    ## The predicted consumption is the shape extracted from the consumption series of the nearest neighbours
    pred_con <- lapply(NNs, function(x) nn_shape(con_shape_series[x], method = shape))

    return(pred_con)
}


mv_fuzzy_con_prediction <- function(clus, new_dels, shape, con_shape_series)
{
    #' Return the consumption prediction for fuzzy clusters
    ## Returns a fuzzy distance matrix to each centroid
    pred_clus <- mv_del_cluster_predictions(new_dels, clus)

    ## Determine the shapes for ever cluster
    shapes <- clus_shapes(clus, shape, con_shape_series)

    ## Apply the weighted sum of each cluster shape based on the fuzzy distance
    ## to each centroid
    pred_mat <- pred_clus %*% do.call(rbind, shapes)  # doing it with matrices

    ## conver the matrix into a list for each row
    pred_con <- plyr::alply(pred_mat, 1)
    names(pred_con) <- NULL  # Sanitize the names

    return(pred_con)
}


mv_con_prediction <- function(train, test, cluster, clustering, shape, shape_series, ...)
{
    #' Predict a consumption series for each element of the new_dels
    ## Fetch the shape series
    shape_series <- match.arg(shape_series, c("smooth", "orig"))
    switch(shape_series,
           "smooth" = {
               con_shape_series <- data_smooth_con(train)
           }, "orig" = {
               con_shape_series  <- data_orig_con(train)
           })

    if (clustering == "NN") {
        pred_con <- mv_knn_con_prediction(shape, con_shape_series, ...)
    } else if (clustering == "fuzzy" {
        new_dels <- data_smooth_dels(test)
        pred_con <- mv_fuzzy_con_prediction(cluster, new_dels, shape, con_shape_series)
    } else {
        new_dels <- data_smooth_dels(test)
        pred_con <- mv_clus_con_prediction(cluster, new_dels, shape, con_shape_series)
    }

    ## Normalize the consumption prediction with the original delivery data
    orig_dels <- data_orig_dels(test)
    pred_con_norm <- mapply(function(x,y) xts(normalize_relative(x, y), order.by = index(y)),
                            pred_con, orig_dels, SIMPLIFY = FALSE)

    ## Sample error
    orig_con <- data_orig_con(test)
    err <- mapply(function(x,y) error_calc(x, y, "rmse"),
                  pred_con_norm, orig_con, SIMPLIFY = FALSE)

    ## Ugly list formatting again
    res <- mapply(list,
                  series = pred_con,
                  norm = pred_con_norm,
                  error = err,
                  SIMPLIFY = FALSE)

    return(res)
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

if (FALSE) {  # Prevents it from running when sourcing the file
    #' dtw example
    a <- ts(sin(seq(0, 2*pi, length.out = 100)))
    b <- ts(sin(seq(pi, 2*pi, length.out = 50)))
    foo <- dtw(a, b, window.type = "slantedband", window.size = 2)
    foo <- dtw(a, b)
    dtwPlotTwoWay(foo, a, b)
    dtwPlotThreeWay(foo, a, b)

    #' This is to show equivalency between dtwclust::dtw_basic and dtw::dtw
    data("uciCT")
    series <- CharTraj
    dtwclust::dtw_basic(series[[1]], series[[2]], norm = "L1", step.pattern = symmetric1, window.size = 2)
    dtw::dtw(proxy::dist(series[[1]], series[[2]], "L1"), step.pattern = symmetric1, window.type = "slantedband", window.size = 2, distance.only = TRUE)$distance

    #' Testing multivariate z_score from dtwcust
    clus_tel_day <- readRDS("../data/master/clus_tel_day.rds")
    series <- mv_series(clus_tel_day)
    foo <- dtwclust::zscore(series[1:2])[[1]][,"con"]
    bar <- z_score(series[[1]][,"con"])
    all.equal(foo, bar)  # You cannot use '==' but must instead you all.equal for floats

    #' Testing the scaling method
    clus_tel_day <- readRDS("../data/master/clus_tel_day.rds")
    series <- mv_series(clus_tel_day)
    normalize_relative(series[[1]][,'con'], clus_tel_day[[1]]$del$orig)
    foo <- series[[1]][,'con']
    bar <- normalize_relative(series[[1]][,'con'], clus_tel_day[[1]]$con$orig)

    #' Comparing the smoothing on the consumption data
    foo <- clus_tel_day[[1]]$con$smooth
    bar <- clus_tel_day[[1]]$con$orig %>%
        my_ets(.) %>%
        filter_year(., "days")
    all.equal(foo, bar)

    #' Testing clustering
    data <- clus_tel_day[1:10]
    clus <- my_clustering(data, series_type = "mv", k = 2, preproc = zscore)

    ##' Double checking equivalency between zscore with the preprocessing
    ## Each part of the mv serie should be zscored independently
    foo <- clus@datalist[[1]][,"con"] %>%  ## The result of the preprocessing by the clustering function
        as.numeric
    bar <- dtwclust::zscore(clus@orig_data[[1]][,"con"])
     all.equal(bar, foo)

    foo <- clus@datalist[[1]][,"del"] %>%  ## The result of the preprocessing by the clustering function
        as.numeric
    bar <- dtwclust::zscore(clus@orig_data[[1]][,"del"])
    all.equal(bar, foo)

    #' Prediction
    ##' Cluster prediction
    new_dels <- del_clus_series(data, "smooth") %>%
        ## Convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., function(x) as.matrix(x)[,1])
    con_shape_series <- mv_series_con(clus@orig_data)

    ## Apply the preprocessing function on the data
    newdata <- clus_preproc(new_dels, clus)[[1]]
    foo <- clus@datalist[[1]][,"del"] %>%  ## The result of the preprocessing by the clustering function
        as.numeric
    all.equal(newdata, foo)

    ## Checking the distance matrix calculation
    dist_mat <- clus_calc_distance_matrix(clus_preproc(new_dels, clus), clus, del_centroids(clus))
    foo <- apply(dist_mat, 1, which.min)
    bar <- mv_del_cluster_predictions(new_dels, clus)
    all.equal(foo, bar)

    ##' Consumption prediction
    pred <- mv_clus_con_prediction(clus, new_dels, "mean", con_shape_series)

    pred_clus <- mv_del_cluster_predictions(new_dels, clus)
    shapes <- clus_shapes(clus, "mean", con_shape_series)
    pred_con_clus <- lapply(pred_clus, function(x) shapes[[x]])

    all.equal(pred, pred_con_clus)


    #' Nearest Neighbor
    test_dels <- del_clus_series(data, "smooth") %>%
        ## Convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., function(x) as.matrix(x)[,1])
    train_dels <- test_dels
    clus <- my_clustering(data, series_type = "mv", k = 2, preproc = zscore)

    distmat <- clus_calc_distance_matrix(test_dels, clus, train_dels)
    NNs <- lapply(1:nrow(distmat), function(i) my_nearest_neighbors(i, distmat, 2))

    pred_con_NN <- lapply(NNs, function(x) nn_shape(con_shape_series[x], method = "mean"))
    pred_con <- mv_knn_con_prediction("mean", con_shape_series, k = 2, distmat = distmat)
    all.equal(pred_con_NN, pred_con)

    #' The top function
    ##' Clustering
    data <- clus_tel_day[1:10]
    new_dels <- del_clus_series(data, "smooth") %>%
        ## Convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., function(x) as.matrix(x)[,1])
    orig_dels <- del_clus_series(data, "orig") %>%
        lapply(., filter_year, time_scale = "days")
    orig_con <- con_clus_series(data, "orig") %>%
        lapply(., filter_year, time_scale = "days")

    clus <- my_clustering(data, series_type = "mv", k = 2, preproc = zscore, trace = FALSE)

    pred_con <- mv_con_prediction(new_dels, orig_dels, orig_con, clus, clustering = "hierarchical", shape = "mean", shape_series = "smooth") %>%
        lapply(., function(x) as.matrix(x$series)[,1])

    all.equal(pred_con, pred_con_clus)

    pred_con <- mv_con_prediction(new_dels, orig_dels, orig_con, clus, clustering = "NN", shape = "mean", shape_series = "smooth", k = 2, distmat = distmat) %>%
        lapply(., function(x) as.matrix(x$series)[,1])
    all.equal(pred_con, pred_con_NN)

    clus <- my_clustering(data, series_type = "mv", k = 2, preproc = zscore, type = "hierarchical")

    #' Fuzzy clustering
    train <- clus_tel_day[1:10]
    test <- clus_tel_day[11:20]
    clus <- my_clustering(train, series_type = "mv", k = 2, preproc = zscore, type = "fuzzy")

    new_dels <- data_smooth_dels(test)

    pred_clus <- mv_del_cluster_predictions(new_dels, clus)
    con_shape_series <- data_smooth_con(train)
    shapes <- clus_shapes(clus, "mean", con_shape_series)

    pred_clus %*% do.call(rbind, shapes)

    mv_fuzzy_con_prediction(clus, new_dels, "mean", con_shape_series)[[1]]
}
