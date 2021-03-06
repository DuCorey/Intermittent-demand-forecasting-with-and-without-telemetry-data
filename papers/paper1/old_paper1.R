#' title: paper1.R
#' comments: Code for the first paper
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' Work directory
setwd("/home/corey/AL/code")
packrat::init()

#' packages
library(rowr)

#' imports
source("data.R")
source("cluster.R")
source("utils.R")
source("plots.R")

## Data
clus_tel_day <- readRDS("../data/master/clus_tel_day_paper1.rds")
clus_tel_day <- readRDS("../data/master/clus_tel_day_paper1old.rds")

## Create the clus_tel_day data object for paper 1
## Filter time series with a minimun of 2 years
cvd <- readRDS("../data/master/client.rds") %>%
    filter_cvd


con_del_data_1 <- function(cvd, source, time_scale) {
    ## Get the data
    print("Fetching delivery series")
    del_orig <- lapply(cvd, client_del_xts, source = source, time_scale = time_scale)

    print("Fetching consumption series")
    con_orig <- lapply(cvd, client_con_xts, time_scale = time_scale)

    ## Cut the clients that have series less than 1 year since it's impossible they won't
    ## be a full year
    f <- function(x) length(x) >= 365
    ind <- sapply(del_orig, f) & sapply(con_orig, f)
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]

    print("Preprocessing delivery data")
    del <- mclapply(del_orig,
                    function(x) filter_full_year(croston_smooth(x), time_scale),
                    mc.cores = 8)

    print("Preprocessing consumption data")
    con <- mclapply(con_orig,
                    function(x) filter_full_year(ksmooth_xts(x, kernel = "normal", bandwidth = 50), time_scale),
                    mc.cores = 8)

    ## Combine both truth series of not null values for del and con
    f <- not %c% is.null
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

clus_tel_day <- con_del_data_1(cvd, source = "tel", time_scale = "day")

## replace_smooth_clus <- function(data)
## {
##     #' Replace the smooth consumption in the data object
##     con_orig <- data$con$orig
##     con_smooth <- filter_full_year(ksmooth_xts(con_orig, kernel = "normal", bandwidth = 50), "days")

##     data$con$smooth <- con_smooth

##     return(data)
## }

## clus_tel_day <- lapply(clus_tel_day, replace_smooth_clus)

## foo <- clus_tel_day[[2]]$con$orig

## bar <- ksmooth_xts(foo, kernel = "normal", bandwidth = 30)
## bar <- ksmooth(time(foo), as.numeric(foo), kernel = "normal", bandwidth = 30)

## plot_mult_xts(bar, foo)

## Add the filtered yearly data to the object
## add_yearly_series <- function(data)
## {
##     #'Add yearly con and del series in the data object
##     data$con$year <- filter_full_year(data$con$orig, "days")
##     data$del$year <- filter_full_year(data$del$orig, "days")

##     return(data)
## }

saveRDS(clus_tel_day, "../data/master/clus_tel_day_paper1.rds")


## Descriptive stats
## Number of clients
length(clus_tel_day)

## Time series categorisations
bar <- do.call(partial(rowr::cbind.fill, fill = NA), lapply(clus_tel_day, function(x) as.numeric(x$del$orig)))
tsintermittent::idclass(bar, type = "PKa", outplot = "none")$summary

bar <- do.call(partial(rowr::cbind.fill, fill = NA), lapply(clus_tel_day, function(x) as.numeric(x$con$orig)))
tsintermittent::idclass(bar, type = "PKa", outplot = "none")$summary


## Univariate Consumption
con_smooth <- pluck_list(clus_tel_day, "con", "smooth")



## con_clus_mean_9 <- tsclust(con_smooth, type = "hierarchical", k = 9,
##                          preproc = zscore,
##                          distance = "dtw_basic",
##                          centroid = mean_series,
##                          trace = TRUE,
##                          control = hierarchical_control(method = "ward.D2"),
##                          args = tsclust_args(dist = list(window.size = 7L, step.pattern = symmetric1)))
##plot(con_clus_mean_9, type = "sc")

## ggsave("./papers/paper1/con_clus_mean_9.eps", device = "eps", width = 8, height = 8, units = "in", dpi = 1000)


## con_clus_mean_12 <- tsclust(con_smooth, type = "hierarchical", k = 12,
##                          preproc = zscore,
##                          distance = "dtw_basic",
##                          centroid = mean_series,
##                          trace = TRUE,
##                          control = hierarchical_control(method = "ward.D2", distmat=con_clus_mean_9@distmat),
##                          args = tsclust_args(dist = list(window.size = 7L, step.pattern = symmetric1)))
## plot(con_clus_mean_12, type = "sc")

## ggsave("./papers/paper1/con_clus_mean_12.eps", device = "eps", width = 8, height = 8, units = "in", dpi = 1000)


## Univariate Delivery
del_smooth <- pluck_list(clus_tel_day, "del", "smooth")

del_clus_mean_9 <- tsclust(del_smooth, type = "hierarchical", k = 9,
                           preproc = zscore,
                           distance = "dtw_basic",
                           centroid = mean_series,
                           trace = TRUE,
                           control = hierarchical_control(method = "ward.D2"),
                           args = tsclust_args(dist = list(window.size = 7L, step.pattern = symmetric1)))
##plot(del_clus_mean_9, type = "sc")

## ggsave("./papers/paper1/del_clus_mean_9.eps", device = "eps", width = 8, height = 8, units = "in", dpi = 1000)

## del_clus_mean_12 <- tsclust(del_smooth, type = "hierarchical", k = 12,
##                            preproc = zscore,
##                            distance = "dtw_basic",
##                            centroid = mean_series,
##                            trace = TRUE,
##                            control = hierarchical_control(method = "ward.D2", distmat = del_clus_mean_9@distmat),
##                            args = tsclust_args(dist = list(window.size = 7L, step.pattern = symmetric1)))
## plot(del_clus_mean_12, type = "sc")

## ggsave("./papers/paper1/del_clus_mean_12.eps", device = "eps", width = 8, height = 8, units = "in", dpi = 1000)


## Multivariate consumption
### 9
## mv_series_smooth <- mapply(function(x,y) mv_serie(x,y), con_smooth, del_smooth, SIMPLIFY = FALSE)

## clus <- con_clus_mean_9

## series <- del_smooth %>%
##     lapply(., function(x) as.matrix(x)[,1]) %>%
##     clus_preproc(., clus)
## new_del_centroids <- lapply(seq_len(clus@k), function(x) clus_centroid(series[clus@cluster == x], clus)[[1]])

## new_del_centroids <- lapply(seq_len(clus@k), function(x) rep(NA, 366))

## clus@centroids <- mapply(function(x,y) cbind(con = x, del = y), clus@centroids, new_del_centroids, SIMPLIFY = FALSE)

## plot_cluster_series(clus, series = mv_series_smooth, TRUE, FALSE)
## ggsave("./papers/paper1/con_clus_mean_9_mv.eps", device = "eps", width = 6.5, height = 4.5, units = "in", dpi = 1000)

## pl <- plot_vertical_cluster_series(x = clus, series = mv_series_smooth, TRUE, FALSE)
## ggsave("./papers/paper1/con_clus_mean_vert_9_mv.eps", plot = pl, device = "eps", width = 6.5, height = 6.5, units = "in", dpi = 1000)

## plot_mv_centroids(clus)
## ggsave("./papers/paper1/con_clus_mean_cent_9.eps", device = "eps", width = 6.5, height = 4.5, units = "in", dpi = 1000)

### 12
## clus <- con_clus_mean_12

## ## new_del_centroids <- lapply(seq_len(clus@k), function(x) clus_centroid(series[clus@cluster == x], clus)[[1]])

## new_del_centroids <- lapply(seq_len(clus@k), function(x) rep(NA, 366))

## clus@centroids <- mapply(function(x,y) cbind(x,y), clus@centroids, new_del_centroids, SIMPLIFY = FALSE)

## plot_cluster_series(clus, series = mv_series_smooth, TRUE, FALSE)

## ggsave("./papers/paper1/con_clus_mean_12_mv.eps", device = "eps", width = 8, height = 8, units = "in", dpi = 1000)


## Multivariate Delivery
mv_series_smooth <- mapply(
    function(x,y) {
        foo <- mv_serie(x,y)
        colnames(foo) <- c("con", "del")
        return(foo)
    }, con_smooth, del_smooth, SIMPLIFY = FALSE)


clus <- del_clus_mean_9

series <- con_smooth %>%
    lapply(., function(x) as.matrix(x)[,1]) %>%
    clus_preproc(., clus)

new_con_centroids <- lapply(seq_len(clus@k), function(x) clus_centroid(series[clus@cluster == x], clus)[[1]])

clus@centroids <- mapply(
    function(x,y) {
        mat <- cbind(con = x, del = y)
        rownames(mat) <- rownames(mv_clus_mean_9@centroids[[1]])
        return(mat)
    }, new_con_centroids, clus@centroids, SIMPLIFY = FALSE)

## Not sure why the melt no longer works here
#foo <- reshape2::melt(tslist(mv_series_smooth))




## plot_cluster_series(clus, series = mv_series_smooth, TRUE, FALSE)
## ggsave("./papers/paper1/del_clus_mean_9_mv.eps", device = "eps", width = 6.5, height = 4.5, units = "in", dpi = 1000)

pl <- plot_vertical_cluster_series(x = clus, series = mv_clus_mean_9@datalist, FALSE, TRUE)
## plot_vertical_cluster_series(x = clus, series = mv_series_smooth, TRUE, FALSE)
ggsave("./papers/paper1/del_clus_mean_vert_9_mv.eps", plot = pl, device = cairo_ps, width = 6.5, height = 6.5, units = "in", dpi = 300)
ggsave("./papers/paper1/del_clus_mean_vert_9_mv.png", plot = pl, device = "png", width = 6.5, height = 6.5, units = "in", dpi = 300)
ggsave("./papers/paper1/del_clus_mean_vert_9_mv.jpg", plot = pl, device = "jpg", width = 6.5, height = 6.5, units = "in", dpi = 300)

plot_mv_centroids(clus)
ggsave("./papers/paper1/del_clus_mean_cent_9.eps", device = "eps", width = 6.5, height = 4.5, units = "in", dpi = 300)
ggsave("./papers/paper1/del_clus_mean_cent_9.png", device = "png", width = 6.5, height = 4.5, units = "in", dpi = 300)


## Confusion matrix
## cluster_table(con_clus_mean_9, del_clus_mean_9)


## Multivariate clustering
mv_clus_mean_9 <- tsclust(mv_series_smooth, type = "hierarchical", k = 9,
                           preproc = zscore,
                           distance = "dtw_basic",
                           centroid = mean_series,
                           trace = TRUE,
                           control = hierarchical_control(method = "ward.D2"),
                           args = tsclust_args(dist = list(window.size = 7L, step.pattern = symmetric1)))

## plot(mv_clus_mean_9, type = "sc")
## ggsave("./papers/paper1/mv_clus_mean_9.eps", device = "eps", width = 6.5, height = 4.5, units = "in", dpi = 1000)

pl <- plot_vertical_clus(mv_clus_mean_9, grob = TRUE)
ggsave("./papers/paper1/mv_clus_mean_vert_9.eps", plot = pl, device = cairo_ps, width = 6.5, height = 6.5, units = "in", dpi = 300)
ggsave("./papers/paper1/mv_clus_mean_vert_9.png", plot = pl, device = "png", width = 6.5, height = 6.5, units = "in", dpi = 300)
ggsave("./papers/paper1/mv_clus_mean_vert_9.jpg", plot = pl, device = "jpeg", width = 6.5, height = 6.5, units = "in", dpi = 300)

plot_mv_centroids(mv_clus_mean_9)
ggsave("./papers/paper1/mv_clus_mean_cent_9.eps", device = "eps", width = 6.5, height = 4.5, units = "in", dpi = 300)
ggsave("./papers/paper1/mv_clus_mean_cent_9.png", device = "png", width = 6.5, height = 4.5, units = "in", dpi = 300)


## Homogeneity and heterogeneity
### Consumption
clus_homogeneity <- function(cluster, clus, distmat) {
    id <- which(clus@cluster==cluster)
    mat_sub <- distmat[id, id]
    diag(mat_sub) <- NA
    return(mean(mat_sub, na.rm = TRUE))
}


clus_heterogeneity <- function(cluster,clus, distmat) {
    id <- which(clus@cluster==cluster)
    mat_sub <- distmat[id, -id]
    return(mean(mat_sub))
}


distmat <- mv_clus_mean_9@distmat

## clus <- con_clus_mean_9
## mean(sapply(seq_len(clus@k), function(x) clus_homogeneity(x, clus, distmat)))
## mean(sapply(seq_len(clus@k), function(x) clus_heterogeneity(x, clus, distmat)))

clus <- del_clus_mean_9
mean(sapply(seq_len(clus@k), function(x) clus_homogeneity(x, clus, distmat)))
mean(sapply(seq_len(clus@k), function(x) clus_heterogeneity(x, clus, distmat)))

clus <- mv_clus_mean_9
mean(sapply(seq_len(clus@k), function(x) clus_homogeneity(x, clus, distmat)))
mean(sapply(seq_len(clus@k), function(x) clus_heterogeneity(x, clus, distmat)))

## Plots for the methodology
autoplot(filter_full_year(clus_tel_day[[8]]$con$orig, "day")) + xlab(NULL) + ylab(NULL)
ggsave("./papers/paper1/con_orig.eps", device = "eps", width = 3, height = 2, units = "in", dpi = 1000)

autoplot(filter_full_year(clus_tel_day[[8]]$del$orig, "day")) + xlab(NULL) + ylab(NULL)
ggsave("./papers/paper1/del_orig.eps", device = "eps", width = 3, height = 2, units = "in", dpi = 1000)

clus <- mv_clus_mean_9
## ggplot(data = reshape2::melt(clus@datalist[[8]]), mapping = aes(x=seq_len(732), y=value)) +
##     geom_line() +
##     xlab(NULL) +
##     ylab(NULL) +
##     geom_vline(xintercept = 366, colour = "black", linetype = "32", size = 1.5) +
##     ggplot2::theme_bw() +
##     ggplot2::annotate("text", label = "Con   Del", x = 360, y = Inf, size = 3, vjust = 2)
## ggsave("./papers/paper1/mv.eps", device = "eps", width = 3, height = 2, units = "in", dpi = 1000)

data <- reshape2::melt(clus@datalist[[8]])
data$Var1 <- as.Date(data$Var1)
ggplot(data = data, mapping = aes(x=Var1, y=value)) +
    facet_grid(Var2 ~ .) +
    geom_line() +
    xlab(NULL) +
    ylab(NULL) +
    ggplot2::theme_bw()
ggsave("./papers/paper1/mv_vert.eps", device = "eps", width = 3, height = 2, units = "in", dpi = 1000)
