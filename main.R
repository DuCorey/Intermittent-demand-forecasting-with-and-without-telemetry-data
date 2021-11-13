# Run code from main

#' Work directory
setwd("/home/corey/AL/code")

#' Soucing files
source("cluster.R")

#' Repl options
options(max.print = 10000, error=recover)

#' Logging error messages
error <- file("error.txt")
sink(error)
file.show("error.txt")
## Unsink
sink()

#' Data objects
delivery_data <- DeliveryData()
delivery_data <- readRDS("../data/master/delivery.rds")
telemetry_data <- TelemetryData()
telemetry_data <- readRDS("../data/master/telemetry.rds")
telemetry_data$data %<>% as.data.table

tank_info <- readxl::read_excel("../data/internship data/US Tank info.xlsx") %>%
    dplyr::mutate_all(function(x) ifelse(x == "NULL", NA, x)) %>%
    tidyr::drop_na(., TelemetrySitename, TelemetryLevelVariable, `Storage ERPCode`)
tank_info[['DPNumber']] <-  sapply(tank_info[['Storage ERPCode']],
                                   split_erp_codes,
                                   USE.NAMES = FALSE)
tank_info %<>% as.data.table

client_sample <- client_view_data(sample_number = 40)
cvd <- client_view_data()

#' Saving data objects
saveRDS(delivery_data, "../data/master/delivery.rds")
saveRDS(telemetry_data, "../data/master/telemetry.rds")
saveRDS(client_sample, "../data/master/client_sample.rds")
saveRDS(cvd, "../data/master/client.rds")

#' Testing the CVD data object
depot_number <- "140041"
depot_number <- "600881"
depot_number <- "600000"
depot_number <- "16618"
depot_number <- "605177"
depot_number <- "600003"  # Two sitenames
depot_number <- "70835"


tank_info_client <- tank_info[DPNumber==depot_number]
sites_tanks <- sites_tanks_for_depot_subset(tank_info_client, telemetry_data$sites)

## Site and tank
f <- pryr::partial(tank_data_for_sitename,
                   telemetry_data = telemetry_data$data,
                   tank_info_client = tank_info_client)

## Tank data
sitename <- names(sites_tanks)[[1]]
tank <- sites_tanks[[1]]
telem_sub <- telemetry_data$data[SITENAME == sitename]
telemetry_tank <- telem_sub[VARNAME == tank]
tank_info_sub_tank <- tank_info_client[TelemetryLevelVariable == tank &
                                       TelemetrySitename == sitename]

Tank(tank, tank_info_sub_tank, telemetry_tank, sitename)

tanks_data <- c(mapply(f, sitename = names(sites_tanks), tanks = sites_tanks,
                       USE.NAMES = FALSE))

## Delivery data
deliverie_data <- Delivery(delivery_data$data[DPNumber==depot_number], depot_number)

## Matched
matched <- match_deliverie_tanks(deliverie_data, tanks_data, depot_number) %>%
    plyr::compact()  # Should we fail to do a matching remove it from the result


#' Delivery Distribution
deliveries <- do.call(rbind, lapply(CVD, function(x) x$delivery))
plot <- qplot(deliveries$DeliveredQuantity, geom="histogram")
ggsave("../code/histplot.png", plot = plot, device = png())


#' Removing customers with no matching
cvd <- Filter(function(x) !is.null(x$matched), cvd)


#' Filtering out customers with no matching ratio
cvd <- Filter(function(x) !is.na(x$match$ratio), cvd)

boxplot(sapply(cvd, get_matching_ratio))


#' Subset based on products
table(sapply(client_sample, get_client_product))
table(sapply(cvd, get_client_product))


#' Unique delivery products
unique(delivery_data$data$RawProduct)


#' Product with LBS
cvd[[976]]

ln2 <- filter_product(cvd, c("LN2"))
cvd <- filter_product(cvd, c("LN2", "LO2", "LAR"))


#' Make sure the tank unit is good
table(sapply(cvd, get_client_tank_unit))
which(sapply(cvd, get_client_tank_unit) != "inH2O")
cvd <- filter_tank_unit(cvd, c("inH2O"))


#' Verify the minimum amounts so we can correct the delivery data
lapply(client_sample, partial(fetch_small_amounts, serie = 'del', amount = 25000))
lapply(cvd, partial(fetch_small_amounts, serie = 'del', amount = 8000))


#' Based on the product the minimal amount in the delivery should be removed
which.min(sapply(cvd, partial(min_matched_amount, serie = 'del')))
cvd[[359]]$match$df
cvd[[65]]$match$df
cvd[[1267]]$match$df[,c(1,2,3,4)]
cvd[[841]]$match$df[,c(1,2,3,4)]
cvd[[65]]$match$df[,c(1,2,3,4)]

#' Deliveries less than 8000s that are matched
#' Seeing 6000 and less that don't match 2300, 15000



#' Are telemetry data even the same length
table(sapply(series, length))
which(sapply(series, length)==7)
series <- Filter(function(x) length(x) == 138, series)


#' Weird start dates
table(sapply(series, function(x) start(x)))
table(sapply(series, function(x) week(start(x))))
table(sapply(series, function(x) week(end(x))))

series <- Filter(function(x) week(end(x)) == 38, series)


serie[14930:14940,]

lapply(client_sample, function(x) head(x$tank[[1]]$telemetry.serie))

#' Are the consumption and delivery amounts consistent
boxplot(sapply(cvd, client_delivery_consumption_ratio))


## Clustering
cvd <- readRDS("../data/master/client.rds") %>%
    filter_cvd


#' Perfect macthes
length(Filter(function(x) get_client_matching_ratio(x) == 1, cvd))
length(cvd)

boxplot(sapply(cvd, get_client_matching_ratio))

## Clustering data
clus_tel_week <- con_del_data(cvd, "tel", "weeks")
saveRDS(clus_tel_week, "../data/master/clus_tel_week.rds")

clus_tel_day <- con_del_data(cvd, "tel", "day")
saveRDS(clus_tel_day, "../data/master/clus_tel_day.rds")

clus_tel_month <- con_del_data(cvd, "tel", "month")
saveRDS(clus_tel_month, "../data/master/clus_tel_month.rds")

time_scale <- "day"
source <- "tel"
del_orig <- lapply(cvd, client_del_xts, source = source, time_scale = time_scale)

sapply(del_orig, function(x) any(x < 0))

sapply(del_orig, function(x) any(is.infinite(x)))

con_orig <- lapply(cvd, client_con_xts, time_scale = time_scale)

del <- lapply(del_orig, my_croston) %>%
    lapply(., z_score) %>%
    lapply(., filter_full_year, time_scale = time_scale)

con <- lapply(con_orig, my_ets) %>%
    lapply(., z_score) %>%
    lapply(., filter_full_year,
           time_scale = time_scale)



## Checking if they match
clus_data$con[[1]] == filter_full_year(client_con_xts(clus_data$cvd[[1]], "weeks"), "weeks")


#' Clustering deliveries
table(del_clus_10@cluster)


#' Clustering consumption
plot(con_clus_5, type = "c")

plot(cvd[[2]]$tank[[1]]$telemetry.serie)
plot(con[[2]])

plot(ts(cvd[[2]]$tank[[1]]$consumption.serie$value))


#' confusion matrix
table(con_clus_3@cluster, del_clus_3@cluster)


pc@centroids

groups <- cutree(fit, 3)

names(groups) = c("clus")

## Let's segment the consumption with and without smoothing
## Then we test the conversion model
clus_tel_day <- con_del_data(cvd, "tel", "day")

day_con_clus_5 <- tsclust(clus_tel_day_smooth$con$series, type = "hierarchical", k = 5,
                          distance = "dtw_basic",
                          trace = TRUE,
                          control = hierarchical_control(method = "ward.D2"),
                          args = tsclust_args(dist = list(window.size = 2, step.pattern = symmetric1)))







plot(day_con_clus_5, type = "sc")

plot_cluster_series(day_con_clus_5, clus_tel_day_smooth$del$orig)

## plots
foo <- fetch_serie_time(cvd[[1]], "tel", "2016-01-07", "2016-02-20")

bar <- xts(foo[["level"]], order.by = foo[["datetime"]])
png("./tel_series.png", width = 5.52, height = 3.76, res = 300, units = "in")
plot(bar)
dev.off()



################################################################################
## Clustering tests
################################################################################
source("cluster.R")
source("utils.R")
source("con_del_models.R")
source("plots.R")
source("forecast.R")

clus_tel_day <- readRDS("../data/master/clus_tel_day.rds")

train <- clus_tel_day[train_test_split(clus_tel_day)$train]
test <- clus_tel_day[train_test_split(clus_tel_day)$test]

## Testing just the consumption segmentation
## Let's segment the consumption with and without smoothing
## Then we test the conversion model

## Using forecast::ets
con_orig <- con_clus_series(train, "orig")

con_smooth_ets_ANN <- mclapply(con_orig, function(x) filter_full_year(my_ets(x, model = "ANN"), "days"), mc.cores = 8)
con_smooth_ets_ZNN <- mclapply(con_orig, function(x) filter_full_year(my_ets(x, model = "ZNN"), "days"), mc.cores = 8)
con_smooth_ets_ZZZ <- mclapply(con_orig, function(x) filter_full_year(my_ets(x, model = "ZZZ"), "days"), mc.cores = 8)

con_smooth_ets_ZNN[[1]] == con_clus_series(train, "smooth")[[1]]

plot_mult_xts(con_smooth_ets_ZNN[[2]], con_orig[[2]])

con_clus_ets_z <- tsclust(con_smooth_ets_ZNN, type = "hierarchical", k = 10,
                          preproc = zscore,
                          distance = "dtw_basic",
                          centroid = shape_extraction,
                          trace = TRUE,
                          control = hierarchical_control(method = "ward.D2"),
                          args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(con_clus_ets_z, type = "sc")

con_clus_ets <- tsclust(con_smooth_ets_ZNN, type = "hierarchical", k = 10,
                        distance = "dtw_basic",
                        trace = TRUE,
                        control = hierarchical_control(method = "ward.D2"),
                        args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(con_clus_ets, type = "sc")


## Using the smooth package
smooth_wrapper <- function(data, f)
{
    #' Wrapper for the smooth package to return a proper xts
    res <- fitted(f(as.numeric(data))) %>%
        xts(., order.by = index(data))

    attr(res, 'frequency') <- frequency(data)

    ## This has to be a bug bug you need to conver it again to an xts. Usually you don't.
    res <- as.xts(res)
    return(res)
}

con_smooth_cma <- mclapply(con_orig, function(x) filter_full_year(smooth_wrapper(x, partial(cma, order = 20)), "days"), mc.cores = 8)

id <- 2
plot_mult_xts(con_orig[[id]], con_smooth_ets_ZNN[[id]], con_smooth_cma[[id]], legend.names = c("Real", "ES", "CMA"))

con_clus_cma <- tsclust(con_smooth_cma, type = "hierarchical", k = 10,
                        preproc = zscore,
                        distance = "dtw_basic",
                        trace = TRUE,
                        control = hierarchical_control(method = "ward.D2"),
                        args = tsclust_args(dist = list(window.size = 20, step.pattern = symmetric1)))

plot(con_clus_cma, type = "sc")

## ASACT for the smoothing
## Almost no difference between ets smoothing of consumption and croston because the data is not intermittent


## Lissage kernel
con_smooth_kern <- mclapply(con_orig, function(x) filter_full_year(ksmooth_xts(x, kernel = "normal", bandwidth = 50), "days"), mc.cores = 8)

id <- 1
plot_mult_xts(con_orig[[id]], con_smooth_ets_ZNN[[id]], con_smooth_cma[[id]], con_smooth_kern[[id]],
              legend.names = c("Real", "ES", "CMA", "Kernel"))

plot_mult_xts(con_orig[[2]], con_smooth_kern[[2]])

plot(con_smooth_kern[[1]])
plot(z_score(con_smooth_kern[[1]]))


con_clus_kern_dtw <- tsclust(con_smooth_kern, type = "hierarchical", k = 20,
                             preproc = zscore,
                             distance = "dtw_basic",
                             centroid = shape_extraction,
                             trace = TRUE,
                             control = hierarchical_control(method = "ward.D2"),
                             args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(con_clus_kern_dtw, type = "sc")


con_clus_kern_dtw_1 <- tsclust(con_smooth_kern, type = "hierarchical", k = 10,
                               preproc = zscore,
                               distance = "dtw_basic",
                               centroid = shape_extraction,
                               trace = TRUE,
                               control = hierarchical_control(method = "ward.D2"),
                               args = tsclust_args(dist = list(window.size = 14, step.pattern = symmetric1)))

plot(con_clus_kern_dtw_1, type = "sc")


con_clus_kern_dtw_2 <- tsclust(con_smooth_kern, type = "hierarchical", k = 20,
                               preproc = zscore,
                               distance = "dtw_basic",
                               centroid = shape_extraction,
                               trace = TRUE,
                               control = hierarchical_control(method = "ward.D2", distmat = con_clus_kern_dtw_1@distmat),
                               args = tsclust_args(dist = list(window.size = 14, step.pattern = symmetric1)))

plot(con_clus_kern_dtw_2, type = "sc")

plot_cluster_series(con_clus_kern_dtw_2, con_orig)

## For now a pretty hard kernel smoothing with shape_extraction for the clusters gives a pretty good shape for the zscored ones
clus_tel_day_kernel <- lapply(clus_tel_day, function(x) replace_ksmooth_clus(x))

train <- clus_tel_day_kernel[train_test_split(clus_tel_day_kernel)$train]
test <- clus_tel_day_kernel[train_test_split(clus_tel_day_kernel)$test]


## Consumption clustering
con_smooth <- con_clus_series(train, "smooth")

con_clus_mean <- tsclust(con_smooth, type = "hierarchical", k = 10,
                         preproc = zscore,
                         distance = "dtw_basic",
                         centroid = mean_series,
                         trace = TRUE,
                         control = hierarchical_control(method = "ward.D2"),
                         args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(con_clus_mean, type = "sc")

## Testing the mean_series centroids
mean_series(con_clus@datalist[con_clus@cluster == 1]) == con_clus@centroids[[1]]

con_clus_shape <- tsclust(con_smooth, type = "hierarchical", k = 10,
                         preproc = zscore,
                         distance = "dtw_basic",
                         centroid = shape_extraction,
                         trace = TRUE,
                         control = hierarchical_control(method = "ward.D2", distmat = con_clus_mean@distmat),
                         args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(con_clus_shape, type = "sc")


## Deliveries clustering
del_smooth <- pluck_list(train, "del", "smooth")

del_clus_mean <- tsclust(del_smooth, type = "hierarchical", k = 10,
                         preproc = zscore,
                         distance = "dtw_basic",
                         centroid = shape_extraction,
                         trace = TRUE,
                         control = hierarchical_control(method = "ward.D2"),
                         args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(del_clus_mean, type = "sc")

del_clus_shape <- tsclust(del_smooth, type = "hierarchical", k = 10,
                          preproc = zscore,
                          distance = "dtw_basic",
                          centroid = shape_extraction,
                          trace = TRUE,
                          control = hierarchical_control(method = "ward.D2", distmat = del_clus_mean@distmat),
                          args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(del_clus_shape, type = "sc")


## MV cluster
mv_series_smooth <- mapply(function(x,y) mv_serie(x,y), con_smooth, del_smooth, SIMPLIFY = FALSE)

mv_clus <- tsclust(mv_series_smooth[1:10], type = "hierarchical", k = 2,
                   preproc = zscore,
                   distance = "dtw_basic",
                   centroid = mean_series,
                   trace = TRUE,
                   control = hierarchical_control(method = "ward.D2"),
                   args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))


## Plot the MV series with only just 1 cluster
### Consumption clus
clus <- con_clus_mean
series <- del_smooth %>%
    lapply(., function(x) as.matrix(x)[,1]) %>%
    clus_preproc(., clus)

new_del_centroids <- lapply(seq_len(clus@k), function(x) clus_centroid(series[clus@cluster == x], clus)[[1]])
clus@centroids <- mapply(function(x,y) cbind(x,y), clus@centroids, new_del_centroids, SIMPLIFY = FALSE)

plot_cluster_series(clus, series = mv_series_smooth, TRUE, FALSE)

### Delivery clus
clus <- del_clus_mean
series <- con_smooth %>%
    lapply(., function(x) as.matrix(x)[,1]) %>%
    clus_preproc(., clus)

new_con_centroids <- lapply(seq_len(clus@k), function(x) clus_centroid(series[clus@cluster == x], clus)[[1]])
clus@centroids <- mapply(function(x,y) cbind(y,x), clus@centroids, new_con_centroids, SIMPLIFY = FALSE)

plot_cluster_series(clus, series = mv_series_smooth, TRUE, FALSE)

## Testing that the new mv clusters are correct
con_centroids(clus)[[1]] == new_con_centroids[[1]]


## confusion matrix
foo <- cluster_table(con_clus, del_clus)
kable(foo, format = "markdown", booktabs = T, caption = "Demo Table")

source("plots.R")

asact_preds <- asact_con_preds(test)
cum_preds <- cum_adida_preds(test)

train <- train[1:20]
test <- test[1:10]

z_dtw_mv_se_10_ms <- list(
    preproc = "zscore",
    distance = "dtw_basic",
    series = list(
        del = "smooth",
        con = "smooth",
        pad = 0
    ),
    dist_args = tsclust_args(dist = list(window.size = 7L)),
    centroid = "mean",
    clustering = "hierarchical",
    control = hierarchical_control(method = "ward.D2"),
    k = 15,
    shape = list(
        method = "mean",
        series = "smooth"
    ),
    train = train,
    test = test
)

kernel_z_dtw_clus <- run_CFM(z_dtw_mv_se_10_ms, cache = FALSE)

## What is the cluster for our first forecast
clus <- kernel_z_dtw_clus$cluster
plot(clus, type = "sc")

centroids <- con_centroids(clus)

new_dels <- data_smooth_dels(test)
new_cons <- data_smooth_con(test)

orig_dels <- data_orig_dels(test)
orig_cons <- data_orig_con(test)


## Let's pick a specific delivery
id <- 125
plot(make_xts(new_dels[[id]]))
plot_mult_xts(make_xts(new_cons[[id]]), make_xts(orig_cons[[id]]))
plot(make_xts(orig_dels[[id]]))

## What is it's prediction
pred_clus <- mv_del_cluster_predictions(new_dels, clus)
pred_clus[[id]]

## what does it's cluster look like
plot(clus, type = "sc", clus = pred_clus[[id]])

## what is the consumption centroid
plot(centroids[[pred_clus[[id]]]])

## What does the shape look like
shape_series <- data_orig_con(train)
shape_series <- data_smooth_con(train)
shape <- "mean"
shapes <- clus_shapes(clus, shape, shape_series)

plot(shapes[[pred_clus[[id]]]])

plot_mult_xts(make_xts(shapes[[pred_clus[[id]]]]),
              make_xts(centroids[[pred_clus[[id]]]]))

## ZScored
plot_mult_xts(make_xts(zscore(shapes[[pred_clus[[id]]]])),
              make_xts(centroids[[pred_clus[[id]]]]))

## With the consumption
plot_mult_xts(make_xts(zscore(shapes[[pred_clus[[id]]]])),
              make_xts(centroids[[pred_clus[[id]]]]),
              make_xts(zscore(new_cons[[id]])),
              legend.names = c("shape", "centroid", "real"))

plot_mult_xts(make_xts(del_centroids(clus)[[pred_clus[[id]]]]),
              make_xts(zscore(new_dels[[id]])),
              legend.names = c("centroid", "real"))


## What does it look unzscored
pred_con <- lapply(pred_clus, function(x) shapes[[x]])

pred_con <- mv_clus_con_prediction(clus, new_dels, "unzscore", con_shape_series)

orig_dels <- data_orig_dels(test)

pred_con_norm <- mapply(function(x,y) xts(normalize_relative(x, y), order.by = index(y)),
                        pred_con, orig_dels, SIMPLIFY = FALSE)

plot_mult_xts(pred_con_norm[[id]], xts(new_cons[[id]], order.by = index(pred_con_norm[[id]])),
              legend.names = c("Pred", "Real Smooth"))

plot_mult_xts(pred_con_norm[[id]], orig_cons[[id]], legend.names = c("Pred", "Real"))

plot_pred_error(kernel_z_dtw_clus, id)

foo <- un_z_score(shapes[[pred_clus[[id]]]], mean(orig_dels[[id]]), sd(orig_dels[[id]])) %>%
    normalize_relative(., orig_dels[[id]])

plot(make_xts(foo))

mean(new_cons[[id]])
sd(new_cons[[id]])

mean(orig_dels[[id]])
sd(orig_dels[[id]])


## 95 is kinda stupid

id <- 95
id <- 125
id <- 4
id <- 40
id <- 223
id <- 43
id <- 183
id <- 182
id <- 26

foo <- un_z_score(zscore(shapes[[pred_clus[[id]]]]), mean(orig_dels[[id]]), sd(orig_dels[[id]])) %>%
    normalize_relative(., orig_dels[[id]])

plot_mult_xts(make_xts(foo), make_xts(new_cons[[id]]))


foo <- un_z_score(zscore(shapes[[pred_clus[[id]]]]), mean(orig_cons[[id]]), sd(orig_cons[[id]])) %>%
    normalize_relative(., orig_dels[[id]])

plot_mult_xts(make_xts(foo), make_xts(new_cons[[id]]))

## there's is definately room for improvement when converting the shape to the real consumption
## We are definately predicting the right shape but for some customers the shape is not being converted properly
## it's lost when we take the mean of the cluster

## We'd like to see if there's a relationship between the mean and sd values and the deliveries so that we can predict the best values for the unzscore

##
cons <- data_orig_con(train)
dels <- data_orig_dels(train)

means <- sapply(cons, mean)
sds <- sapply(cons, sd)

dels_sum <- sapply(dels, sum)
dels_sd <- sapply(dels, sd)
dels_mean <- sapply(dels, mean)

dels_cv2 <- sapply(dels, function(x) (sd(x)/mean(x))**2)

plot(dels_cv2, sds)
plot(log(dels_cv2), log(sds))
plot(means, dels_sum)

summary(lm(log(sds) ~ log(dels_cv2)))

data_sd <- data.frame(sd = sds, del_sd = dels_sd)
data_mean <- data.frame(mean = means, del_mean = dels_mean)

model_sd <- lm(log(sd) ~ log(del_sd), data = data_sd)
model_mean <- lm(mean ~ del_mean, data = data_mean)

predict(model_mean, data = data.frame(del_mean = c(10)))

summary(model_mean)
summary(model_sd)

newdata <- data.frame(del_sd=c(2))

exp(predict(model, newdata))

un_zscore_pred <- function(shape, model_mean, model_sd, del)
{
    pred_mean <- predict(model_mean, newdata = data.frame(del_mean = c(mean(del))))
    pred_sd <- exp(predict(model_sd, newdata = data.frame(del_sd = c(sd(del)))))

    res <- un_z_score(shape, pred_mean, pred_sd)
    res <- normalize_relative(res, del)
    res <- xts(res, order.by = index(del))

    return(res)
}

foo <- un_zscore_pred(zscore(shapes[[pred_clus[[id]]]]), model_mean, model_sd, orig_dels[[id]])

plot_mult_xts(make_xts(foo), make_xts(new_cons[[id]]))

## Error as a function of the CV2
bar <- Reduce(c, clus_mase)
del_sd <- sapply(data_orig_dels(test), sd)
del_cv2 <- sapply(data_orig_dels(test), function(x) (sd(x)/mean(x))**2)

plot(log(del_sd), log(bar))

## Comparing min_max normalization
id <- 95
id <- 125
id <- 4
id <- 40
id <- 223
id <- 43
id <- 183
id <- 182
id <- 26

id <- 707

plot_mult_xts(make_xts(z_score(new_cons[[id]])),
              make_xts(zscore(new_cons[[id]])))

scaled <- make_xts(scale(new_cons[[id]]))


plot_mult_xts(scaled)

cons <- data_orig_con(train)
dels <- data_orig_dels(train)

maxs <- sapply(cons, max)
mins <- sapply(cons, min)

deltas <- sapply(cons, function(x) max(x) - min(x))

dels_sum <- sapply(dels, sum)
dels_sd <- sapply(dels, sd)
dels_mean <- sapply(dels, mean)
dels_cv2 <- sapply(dels, function(x) (sd(x)/mean(x))**2)
dels_max <- sapply(dels, max)


remove_zeros_ts <- function(ts)
{
    return(as.numeric(ts[ts!=0]))
}


remove_zeros_ts(dels[[1]])


dels_deltas <- sapply(dels, function(x) max(remove_zeros_ts(x)) - min(remove_zeros_ts(x)))

plot(maxs, dels_max)

plot(mins, dels_mean)

plot(log(deltas), log(dels_deltas))

summary(lm(log(deltas) ~ log(dels_deltas)))





## convolution does not work
shape <- shapes[[pred_clus[[id]]]]

plot_mult_xts(make_xts(zscore(shape)),
              make_xts(zscore(new_cons[[id]])),
              legend.names = c("shape", "real"))


orig_del <- orig_dels[[id]]
plot(orig_del)

plot(make_xts(new_cons[[id]]))

smooth_del <- new_dels[[id]]
plot(make_xts(smooth_del))

mean(orig_del)
sd(orig_del)

mean(new_cons[[id]])
sd(new_cons[[id]])

foo <- convolve(orig_del, shape, type = "filter")

foo <- smooth_del * shape

plot_mult_xts(make_xts(z_score(foo)), zscore(shape), zscore(new_cons[[id]]))

foo <- normalize_relative(foo, orig_del), order.by = index(orig_del)

plot_mult_xts(make_xts(foo), make_xts(new_cons[[id]]))


## Deliveries clustering without normalisation
del_orig <- data_orig_dels(train)

del_clus_orig <- tsclust(del_orig, type = "hierarchical", k = 10,
                         preproc = zscore,
                         distance = "dtw_basic",
                         centroid = shape_extraction,
                         trace = TRUE,
                         control = hierarchical_control(method = "ward.D2"),
                         args = tsclust_args(dist = list(window.size = 7, step.pattern = symmetric1)))

plot(del_clus_orig, type = "sc")


## Determine the amount for the denormalization based on the nearest neighbour of unnomalized zscored deliveries
