# Run code from main

#' Work directory
setwd("/home/corey/AL/code")

#' Soucing files
source("cluster.R")

#' Repl options
options(max.print = 1000, error=recover, warn=0)

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
clus_tel_week <- cluster_data(cvd, "tel", "weeks")
saveRDS(clus_tel_week, "../data/master/clus_tel_week.rds")

clus_tel_day <- cluster_data(cvd, "tel", "day")
saveRDS(clus_tel_day, "../data/master/clus_tel_day.rds")

clus_tel_month <- cluster_data(cvd, "tel", "month")
saveRDS(clus_tel_month, "../data/master/clus_tel_month.rds")

time_scale <- "day"
source <- "tel"
del_orig <- get_del_series(cvd, source = source, time_scale = time_scale)
sapply(del_orig, function(x) any(x < 0))

sapply(del_orig, function(x) any(is.infinite(x)))

con_orig <- get_con_series(cvd, time_scale = time_scale)

del <- lapply(del_orig, my_croston) %>%
    lapply(., z_score) %>%
    lapply(., filter_year, time_scale = time_scale)

con <- lapply(con_orig, my_ets) %>%
    lapply(., z_score) %>%
    lapply(., filter_year,
           time_scale = time_scale)



## Checking if they match
clus_data$con[[1]] == filter_year(client_cluster_con_serie(clus_data$cvd[[1]], "weeks"), "weeks")


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
clus_tel_day <- cluster_data(cvd, "tel", "day")

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
