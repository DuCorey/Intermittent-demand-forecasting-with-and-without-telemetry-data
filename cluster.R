#' title: cluster.R
#' comments: functions for fetching data necessary for clustering
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(parallel)
library(scales)
library(kableExtra)

#' imports
source("data.R")
source("smooth.R")
source("operators.R")
source("series.R")
source("utils.R")
source("forecast.R")
source("ADIDA.R")
source("error.R")
source("dtwclust.R")
source("purrr.R")

#' functions
## TODO - Can we extend this to time series of less than 1 year or of mismatched time?
## Looks probable as the distance methods accept variable lenghts
my_clustering <- function(series, k, trace = TRUE, ...) {
    #' Wrapper over dtwclust::tsclust to save initial data
    #' Also allows it to run for k = 1 cluster

    if (k != 1) {
        clus <- dtwclust::tsclust(series,
                                  trace = trace,
                                  k = k,
                                  ...)

        ## Add the original series in the object's attributes
        attributes(clus)$orig_data <- series

    } else {
        #' Allow for k = 1 clustering even though the original function complains about it
        ## Run it normally with 2 (fastest clustering)
        clus <- my_clustering(series, k = 2, trace = trace, ...)

        ## Overwrite the cluster member so that they are all 1
        ones <- rep(1, times = length(clus@cluster))
        names(ones) <- names(series)
        attributes(clus)$cluster <- ones

        ## Overwrtie the clustering method to only return 1
        clus@family@cluster <- function(distmat = NULL, ...) {
            return(rep(1, nrow(distmat)))
        }
        attributes(clus)$k <- 1
    }
    return(clus)
}


## The order of the columns in the centroids follows the order of the mv serie
## con is column 1
## del is column 2
con_centroids <- function(clus) {
    #' Return the centroids of the consumption sequence of the mv data
    return(lapply(clus@centroids, function(x) x[,1]))
}


del_centroids <- function(clus) {
    #' Return the centroids of the delivery sequence of the mv data
    return(lapply(clus@centroids, function(x) x[,"del"]))
}


clus_preproc <- function(newdata, clus) {
    #' Apply the preprocessing function on the data
    newdata <- quoted_call(
        clus@family@preproc,
        newdata,
        dots = subset_dots(clus@args$preproc, clus@family@preproc)
    )
    return(newdata)
}


clus_calc_distance_matrix <- function(newdata, clus, centroids) {
    #' Calculate the distance matrix between the newdata and the centroids
    dist_mat <- quoted_call(
        clus@family@dist,
        x = newdata,
        centroids = centroids,
        dots = clus@args$dist
    )

    return(dist_mat)
}


clus_centroid <- function(newdata, clus) {
    #' Apply the clusters centroid function to the new data
    return(clus@family@allcent(newdata))
}


mv_del_cluster_predictions <- function(newdata, clus) {
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


clus_shapes <- function(clus, method, con_shape_series) {
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
               centroids <- con_centroids(clus)
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
               shapes <- con_centroids(clus)
           })

    return(shapes)
}


my_nearest_neighbors <- function(i, distance_matrix, k) {
    #' Return the k nearest_neighbors for the ith row in the distance_matrix
    return(order(distance_matrix[i, ])[1:k])
}


nn_shape <- function(method, con_shape_series) {
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


mv_clus_con_prediction <- function(clus, new_dels, shape, con_shape_series) {
    ## Predict a cluster for each delivery series
    pred_clus <- mv_del_cluster_predictions(new_dels, clus)

    ## Determine the shapes for each cluster
    shapes <- clus_shapes(clus, shape, con_shape_series)

    ## The predicted con is simply the shape associate to that cluster
    pred_con <- lapply(pred_clus, function(x) shapes[[x]])

    return(pred_con)
}


mv_knn_con_prediction <- function(shape, con_shape_series, k, distmat) {
    #' Find the nearest neighbors for each delivery series and create a shape
    #' from the original consumption of those nearest neighbors
    ## We need to find the nearest_neighbor for each new delivery
    NNs <- lapply(1:nrow(distmat), function(i) my_nearest_neighbors(i, distmat, k))

    ## The predicted consumption is the shape extracted from the consumption series of the nearest neighbours
    pred_con <- lapply(NNs, function(x) nn_shape(con_shape_series[x], method = shape))

    return(pred_con)
}


mv_fuzzy_con_prediction <- function(clus, new_dels, shape, con_shape_series) {
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


mv_con_prediction <- function(train, test, cluster, clustering, shape, shape_series, ...) {
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
    } else if (clustering == "fuzzy") {
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


cluster_table <- function(c1, c2) {
    #' Return the cross tabulation table between the clusters c1 and c2
    #' Return the greatest elements in the table and their weight in the cluster
    orig <- table(c1@cluster, c2@cluster)

    dels <- apply(orig, 1, function(x) paste("D", which.max(x), "-", scales::percent(max(x)/sum(x)), sep = ""))
    dels <- matrix(dels, ncol = 1)
    colnames(dels) <- "%"

    cons <- apply(orig, 2, function(x) paste("C", which.max(x), "-", scales::percent(max(x)/sum(x)), sep = ""))
    cons <- matrix(cons, nrow = 1)
    rownames(cons) <- "%"

    res <- cbind(orig, dels)
    res <- plyr::rbind.fill.matrix(res, cons)
    rownames(res) <- c(rownames(orig), "%")

    return(res)
}


## distance_matrix <- function(data, fun)
## {
##     series <- lapply(data, z_transform %c% fun)
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
    b <- ts(cos(seq(0, 2*pi, length.out = 100)))
    foo <- dtw(a, b, window.type = "slantedband", window.size = 2)
    foo <- dtw(a, b)

    png("./papers/thesis/2waydtw.png")
    dtwPlotTwoWay(foo, a, b)
    dev.off()

    png("./papers/thesis/3waydtw.png")
    dtwPlotThreeWay(foo, a, b)
    dev.off()


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
        ets_smooth_xts(.) %>%
        filter_full_year(., "days")
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
    new_dels <- pluck_list(data, "del", "smooth") %>%
        ## Convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., function(x) as.matrix(x)[,1])

    con_shape_series <- lapply(clus@orig_data, function(x) x[,"con"])

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
    test_dels <- pluck_list(data, "del", "smooth") %>%
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
    new_dels <- pluck_list(data, "del", "smooth") %>%
        ## Convert the delivery series to numeric so as to be consistent with the
        ## data being sent over the the clusering method
        lapply(., function(x) as.matrix(x)[,1])
    orig_dels <- pluck_series(data, "del", "orig") %>%
        lapply(., filter_full_year, time_scale = "days")
    orig_con <- pluck_list(data, "con", "orig") %>%
        lapply(., filter_full_year, time_scale = "days")

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
    clus <- my_clustering(train, series_type = "mv", k = 2, preproc = zscore, type = "fuzzy", centroid = "fcm")

    new_dels <- data_smooth_dels(test)

    pred_clus <- mv_del_cluster_predictions(new_dels, clus)
    con_shape_series <- data_smooth_con(train)
    shapes <- clus_shapes(clus, "mean", con_shape_series)

    pred_clus %*% do.call(rbind, shapes)

    mv_fuzzy_con_prediction(clus, new_dels, "mean", con_shape_series)[[1]]
}
