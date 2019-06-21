#' title: dtwclust.R
#' comments: this file contains edits and imports from the dtwclust package
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(dtwclust)

#' imports

#' functions
enlist <- getFromNamespace("enlist", "dtwclust")
has_dots <- getFromNamespace("has_dots", "dtwclust")
subset_dots <- getFromNamespace("subset_dots", "dtwclust")


#' quoted_call does not exist in the dtwclust so we just copied it from him
quoted_call <- function(fun, ..., dots = NULL)
{
    #' do.call but always quoted
    do.call(fun, enlist(..., dots = dots), quote = TRUE)
}


is_multivariate <- getFromNamespace("is_multivariate", "dtwclust")
check_consistency <- getFromNamespace("check_consistency", "dtwclust")


#' We are redoing the plotting method because I want the clusters to be in the front

#'   The plot method uses the `ggplot2` plotting system (see [ggplot2::ggplot()]).
#'
#'   The default depends on whether a hierarchical method was used or not. In those cases, the
#'   dendrogram is plotted by default; you can pass any extra parameters to [stats::plot.hclust()]
#'   via the ellipsis (`...`).
#'
#'   Otherwise, the function plots the time series of each cluster along with the obtained centroid.
#'   The default values for cluster centroids are: `linetype = "dashed"`, `size = 1.5`, `colour =
#'   "black"`, `alpha = 0.5`. You can change this by means of the ellipsis (`...`).
#'
#'   You can choose what to plot with the `type` parameter. Possible options are:
#'
#'   - `"dendrogram"`: Only available for hierarchical clustering.
#'   - `"series"`: Plot the time series divided into clusters without including centroids.
#'   - `"centroids"`: Plot the obtained centroids only.
#'   - `"sc"`: Plot both series and centroids
#'
#'   In order to enable labels on the (non-dendrogram) plot, you have to select an option that plots
#'   the series and at least provide an empty list in the `labels` argument. This list can contain
#'   arguments for [ggrepel::geom_label_repel()] and will be passed along. The following are
#'   set by the plot method if they are not provided:
#'
#'   - `"mapping"`: set to [aes_string][ggplot2::aes_string](x = "t", y = "value", label = "label")
#'   - `"data"`: a data frame with as many rows as series in the `datalist` and 4 columns:
#'     + `t`: x coordinate of the label for each series.
#'     + `value`: y coordinate of the label for each series.
#'     + `cl`: index of the cluster to which the series belongs (i.e. `x@cluster`).
#'     + `label`: the label for the given series (i.e. `names(x@datalist)`).
#'
#'   You can provide your own data frame if you want, but it must have those columns and, even if
#'   you override `mapping`, the `cl` column must have that name. The method will attempt to spread
#'   the labels across the plot, but note that this is **subject to randomness**, so be careful if
#'   you need reproducibility of any commands used after plotting (see examples).
#'
#'   If created, the function returns the `gg` object invisibly, in case you want to modify it to
#'   your liking. You might want to look at [ggplot2::ggplot_build()] if that's the case.
#'
#'   If you want to free the scale of the X axis, you can do the following:
#'
#'   `plot(x, plot = FALSE)` `+` `facet_wrap(~cl, scales = "free")`
#'
#'   For more complicated changes, you're better off looking at the source code at
#'   \url{https://github.com/asardaes/dtwclust/blob/master/R/S4-TSClusters-methods.R} and creating your
#'   own plotting function.
#'
#' @return
#'
#' The plot method returns a `gg` object (or `NULL` for dendrogram plot) invisibly.
#'
#' @examples
#'
#' \dontrun{
#' plot(pc_obj, type = "c", linetype = "solid",
#'      labs.arg = list(title = "Clusters' centroids"))
#'
#' set.seed(15L)
#' plot(pc_obj, labels = list(nudge_x = -5, nudge_y = 0.2),
#'      clus = c(1L,4L))
#' }
#'
plot.TSClusters <- function(x, y, ...,
                            clus = seq_len(x@k), labs.arg = NULL,
                            series = NULL, time = NULL,
                            plot = TRUE, type = NULL,
                            labels = NULL)
{
                                        # set default type if none was provided
    if (!is.null(type)) # nocov start
        type <- match.arg(type, c("dendrogram", "series", "centroids", "sc"))
    else if (x@type == "hierarchical")
        type <- "dendrogram"
    else
        type <- "sc" # nocov end

                                        # plot dendrogram?
    if (inherits(x, "HierarchicalTSClusters") && type == "dendrogram") {
        x <- methods::S3Part(x, strictS3 = TRUE, "hclust")
        if (plot) graphics::plot(x, ...)
        return(invisible(NULL))
    }
    else if (x@type != "hierarchical" && type == "dendrogram") {
        stop("Dendrogram plot only applies to hierarchical clustering.")
    }

                                        # Obtain data, the priority is: provided data > included data list
    if (!is.null(series)) {
        data <- tslist(series)
    }
    else {
        if (length(x@datalist) < 1L)
            stop("Provided object has no data. Please provide the data manually.") # nocov
        data <- x@datalist
    }

                                        # centroids consistency
    check_consistency(centroids <- x@centroids, "vltslist")

                                        # force same length for all multivariate series/centroids in the same cluster by
                                        # adding NAs
    if (mv <- is_multivariate(data)) {
        data <- lapply(data, base::as.matrix)
        centroids <- lapply(centroids, base::as.matrix)
        clusters <- split(data, factor(x@cluster, levels = 1L:x@k), drop = FALSE)
        for (id_clus in 1L:x@k) {
            cluster <- clusters[[id_clus]]
            if (length(cluster) < 1L) next # nocov (empty cluster)
            nc <- NCOL(cluster[[1L]])
            len <- sapply(cluster, NROW)
            L <- max(len, NROW(centroids[[id_clus]]))
            trail <- L - len
            clusters[[id_clus]] <- Map(cluster, trail, f = function(mvs, trail) {
                rbind(mvs, matrix(NA, trail, nc))
            })
            trail <- L - NROW(centroids[[id_clus]])
            centroids[[id_clus]] <- rbind(centroids[[id_clus]], matrix(NA, trail, nc))
        }
                                        # split returns the result in order of the factor levels,
                                        # but I want to keep the original order as returned from clustering
        ido <- sort(sort(x@cluster, index.return = TRUE)$ix, index.return = TRUE)$ix
        data <- unlist(clusters, recursive = FALSE)[ido]
    }

                                        # helper values (lengths() here, see issue #18 in GitHub)
    L1 <- lengths(data)
    L2 <- lengths(centroids)
                                        # timestamp consistency
    if (!is.null(time) && length(time) < max(L1, L2))
        stop("Length mismatch between values and timestamps") # nocov
                                        # Check if data was z-normalized
    if (x@preproc == "zscore")
        title_str <- "Clusters' members (z-normalized)"
    else
        title_str <- "Clusters' members"

                                        # transform to data frames
    dfm <- reshape2::melt(data)
    dfcm <- reshape2::melt(centroids)

                                        # time, cluster and colour indices
    color_ids <- integer(x@k)
    dfm_tcc <- mapply(x@cluster, L1, USE.NAMES = FALSE, SIMPLIFY = FALSE,
                      FUN = function(clus, len) {
                          t <- if (is.null(time)) seq_len(len) else time[1L:len]
                          cl <- rep(clus, len)
                          color <- rep(color_ids[clus], len)
                          color_ids[clus] <<- color_ids[clus] + 1L
                          data.frame(t = t, cl = cl, color = color)
                      })
    dfcm_tc <- mapply(1L:x@k, L2, USE.NAMES = FALSE, SIMPLIFY = FALSE,
                      FUN = function(clus, len) {
                          t <- if (is.null(time)) seq_len(len) else time[1L:len]
                          cl <- rep(clus, len)
                          data.frame(t = t, cl = cl)
                      })

                                        # bind
    dfm <- data.frame(dfm, do.call(rbind, dfm_tcc, TRUE))
    dfcm <- data.frame(dfcm, do.call(rbind, dfcm_tc, TRUE))
                                        # make factor
    dfm$cl <- factor(dfm$cl)
    dfcm$cl <- factor(dfcm$cl)
    dfm$color <- factor(dfm$color)

                                        # create gg object
    gg <- ggplot2::ggplot(data.frame(t = integer(),
                                     variable = factor(),
                                     value = numeric(),
                                     cl = factor(),
                                     color = factor()),
                          ggplot2::aes_string(x = "t",
                                              y = "value",
                                              group = "L1"))

                                        # add series appropriate
    if (type %in% c("sc", "series"))
        gg <- gg + ggplot2::geom_line(data = dfm[dfm$cl %in% clus, ], aes_string(colour = "color"))

                                        # add centroids next if appropriate, so that they are at the very front
    if (type %in% c("sc", "centroids")) {
        if (length(list(...)) == 0L)
            gg <- gg + ggplot2::geom_line(data = dfcm[dfcm$cl %in% clus, ],
                                          linetype = "dashed",
                                          size = 1.5,
                                          colour = "black",
                                          alpha = 0.5)
        else
            gg <- gg + ggplot2::geom_line(data = dfcm[dfcm$cl %in% clus, ], ...)
    }
                                        # add vertical lines to separate variables of multivariate series
    if (mv) {
        ggdata <- data.frame(cl = rep(1L:x@k, each = (nc - 1L)),
                             vbreaks = as.numeric(1L:(nc - 1L) %o% sapply(centroids, NROW)))
        gg <- gg + ggplot2::geom_vline(data = ggdata[ggdata$cl %in% clus, , drop = FALSE],
                                       colour = "black", linetype = "longdash",
                                       ggplot2::aes_string(xintercept = "vbreaks"))
    }

                                        # add labels
    if (type %in% c("sc", "series") && is.list(labels)) {
        if (is.null(labels$mapping))
            labels$mapping <- ggplot2::aes_string(x = "t", y = "value", label = "label")
        if (is.null(labels$data) && !is.null(names(x@datalist))) {
            label <- names(x@datalist)[x@cluster %in% clus]
            label <- split(label, x@cluster[x@cluster %in% clus])
            dfm <- dfm[dfm$cl %in% clus,]
            labels$data <- dplyr::bind_rows(lapply(split(dfm, dfm$cl), function(df_cluster) {
                                        # keep given order for split
                df_cluster$L1 <- factor(df_cluster$L1, levels = unique(df_cluster$L1))
                df_series <- split(df_cluster[c("t", "value", "cl")], df_cluster$L1)
                ret <- vector("list", length(df_series))
                for (i in seq_along(df_series)) {
                    this_df <- df_series[[i]]
                    for (not_this in df_series[-i]) {
                        if (isTRUE(all.equal(not_this, this_df, check.attributes = FALSE))) next
                        this_df <- dplyr::anti_join(this_df, not_this, by = c("t", "value"))
                    }
                    ret[[i]] <- dplyr::sample_n(this_df, 1L)
                }
                                        # return
                dplyr::bind_rows(ret)
            }))
            labels$data$label <- unlist(label)
        }
        labels$data <- labels$data[labels$data$cl %in% clus,]
        labels$inherit.aes <- FALSE
        gg <- gg + do.call(ggrepel::geom_label_repel, labels, TRUE)
    }

                                        # add facets, remove legend, apply kinda black-white theme
    gg <- gg +
        ggplot2::facet_wrap(~cl, scales = "free_y") +
        ggplot2::guides(colour = FALSE) +
        ggplot2::theme_bw()

                                        # labs
    if (!is.null(labs.arg)) # nocov start
        gg <- gg + ggplot2::labs(labs.arg)
    else
        gg <- gg + ggplot2::labs(title = title_str)

                                        # plot without warnings in case I added NAs for multivariate cases
    if (plot) suppressWarnings(graphics::plot(gg))
    invisible(gg) # nocov end
}

setMethod("plot", methods::signature(x = "TSClusters", y = "missing"), plot.TSClusters)
