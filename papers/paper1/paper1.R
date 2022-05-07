#' title: paper1.R
#' comments: Code for the first paper
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' Work directory
setwd("/home/corey/AL/code")
packrat::init()

#' Packages
library(splitstackshape)
library(ggplot2)

#' Sourcing
source("data.R")
source("cluster.R")
source("con_del_models.R")
source("matrix.R")

## Data
con_del_data <- readRDS("../data/master/clus_tel_day_paper1_1.rds")

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

    ## Filter out customers that do not have a full year for 2016 and the second half of 2015
    ## and the first two months of 2016
    ## 2015-2016 are for training
    ## 2017 january is validation
    ## 2017 february is testing

    print("Filtering data")
    filter_f <- function(x) filter_full_xts_observations(x, "2015-07-01", "2017-01-31")
    del_filt <- lapply(del_orig, filter_f)
    con_filt <- lapply(con_orig, filter_f)

    ## Removing the null from the outputs
    f <- not %c% is.null
    ind <- sapply(del_filt, f) & sapply(con_filt, f)
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]

    ## Filter out customers that do not have a minimum of 1 observations in the second half
    ## 2015. These customers cannot be used for smoothing using Croston
    ## Run this filter after the first one since we have to make sure that the second half
    ## of 2015 is available
    filter_f_2 <- function(x) sum(x[xts_range("2015-07-01", "2015-12-31")] != 0) >= 1
    ind <- sapply(del_orig, filter_f_2)
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]

    ## Final kept data
    f <- function(x) x[xts_range("2015-07-01", "2017-01-31")]
    del_filt <- lapply(del_orig, f)
    con_filt <- lapply(con_orig, f)

    ## Structuring the final output so  that it's a list
    f <- function(cvd, del_orig, del_filt, con_orig, con_filt) {
        structure(
            list(
                cvd = cvd,
                con = list(orig = con_orig, filt = con_filt),
                del = list(orig = del_orig, filt = del_filt)
            ),
            class = "ConDelData"
        )
    }
    res <- mapply(f, cvd, del_orig, del_filt, con_orig, con_filt, SIMPLIFY = FALSE)
    return(res)
}


con_del_data <- con_del_data_1(cvd, source = "tel", time_scale = "day")
saveRDS(con_del_data, "../data/master/clus_tel_day_paper1_1.rds")


###############################
## Descriptive stats
###############################
## Number of clients
length(con_del_data)

## Croston decomposition
## Run these after in case of NA's produced by the time shift
tukey_decomp_table(pluck_list(con_del_data, "del", "filt"))
tukey_decomp_table(pluck_list(con_del_data, "con", "filt"))

## Time series categorisations
del_cat <- intermittent_categorisation(
    pluck_list(con_del_data, "del", "filt"),
    type = "PKa",
    outplot = "none"
)
del_cat

pos_cat <- intermittent_categorisation(
    pluck_list(con_del_data, "con", "filt"),
    type = "PKa",
    outplot = "none"
)
pos_cat


## Train/Test split
## Gotta make sure that the two sets of data maintain the intermittency
del_int <- vector(length = length(con_del_data))
for (i in seq_len(length(con_del_data))) {
    if ((i %in% del_cat$idx.croston) | (i %in% del_cat$idx.sba)) {
        del_int[[i]] <- TRUE
    } else if (i %in% del_cat$idx.ses) {
        del_int[[i]] <- FALSE
    } else {
        stop()
    }
}

pos_int <- vector(length = length(con_del_data))
for (i in seq_len(length(con_del_data))) {
    if ((i %in% pos_cat$idx.croston) | (i %in% pos_cat$idx.sba)) {
        pos_int[[i]] <- TRUE
    } else if (i %in% pos_cat$idx.ses) {
        pos_int[[i]] <- FALSE
    } else {
        stop()
    }
}

dps <- sapply(con_del_data, function(x) x$cvd$DP)
cat_table <- data.table(del = del_int, pos = pos_int, dp = dps)

## Split the data into 0.9 & 0.1 train+valid / test
set.seed(1626614)
train_test_split <- splitstackshape::stratified(cat_table, c("del", "pos"), size = 0.9, bothSets = TRUE)
## Split the train data into 8/9 & 1/9 train / valid
## This makes it so at the end we have 0.8, 0.1, 0.1 train/valid/test
set.seed(1626614)
train_valid_split <- splitstackshape::stratified(train_test_split$SAMP1, c("del", "pos"), size = 1-1/9, bothSets = TRUE)

summary(train_valid_split$SAMP1)
summary(train_valid_split$SAMP2)
summary(train_test_split$SAMP2)

train_con_del_data <- con_del_data[which(dps %in% train_valid_split$SAMP1$dp)]
valid_con_del_data <- con_del_data[which(dps %in% train_valid_split$SAMP2$dp)]
test_con_del_data <- con_del_data[which(dps %in% train_test_split$SAMP2$dp)]

saveRDS(train_con_del_data, "../data/master/paper1/train_con_del_data.rds")
saveRDS(valid_con_del_data, "../data/master/paper1/valid_con_del_data.rds")
saveRDS(test_con_del_data, "../data/master/paper1/test_con_del_data.rds")

train_con_del_data <- readRDS("../data/master/paper1/train_con_del_data.rds")
valid_con_del_data <- readRDS("../data/master/paper1/valid_con_del_data.rds")
test_con_del_data <- readRDS("../data/master/paper1/test_con_del_data.rds")

length(train_con_del_data)
length(valid_con_del_data)
length(test_con_del_data)


################################
## Forecasting
################################
forecast_consumption <- function(client, ffun, dfun) {
    #' Main general forecasting function
    train <- dfun(client)

    fmodel <- ffun(train)
    fres <- result_forecast(forecast(fmodel, h = 28))


    ## Compare the performance on the test data
    real <- test_data(client)

    error <- fcast_error(real, fres, train)

    structure(
        list(
            model = compact_forecast(fmodel),
            error = error
        )
    )
}


test_data <- function(client) {
    ## Last month of the data
    return(client$con$filt[xts_range("2017-01-01", "2017-01-28")])
}


fcast_error <- function(full_real, full_fcast, train) {
    #' Calculate the standard out-of-sample errors for the ndusl forecast
    #' for 3 different horizons : 7 days, 14 days, and 28 days
    if (length(full_fcast) < 28) {
        stop("Incorrect forecast length")
    }

    error <- matrix(nrow = 5, ncol = 3)
    rownames(error) <- c("sME", "sMAE", "sMSE", "MAAPE", "sPIS")
    colnames(error) <- c(7, 14, 28)

    scale <- mean(train)
    breakpoints <- c(7, 14, 28)

    for(i in seq_len(3)) {
        breakpoint <- breakpoints[i]
        real <- head(full_real, breakpoint)
        fcast <- head(full_fcast, breakpoint)

        error[,i] <- c(my_sME(real, fcast, scale),
                       my_sMAE(real, fcast, scale),
                       my_sMSE(real, fcast, scale),
                       my_MAAPE(real, fcast),
                       my_sPIS(real, fcast, scale))
    }

    return(error)
}


mean_fcast_error <- function(error_list, error_type, error_length) {
    return(mean(sapply(error_list, function(x) x$error[error_type, error_length])))
}


fcast_error_table <- function(fcast) {
    errors <-  c("sME", "sMAE", "sMSE", "MAAPE", "sPIS")
    times <- c("7","14","28")
    res <- cross_apply(errors, times, partial(mean_fcast_error, error_list = fcast))
    return(res)
}


fcast_list_error_table <- function(fcast_list) {
    errors <-  c("sME", "sMAE", "sMSE", "MAAPE", "sPIS")
    times <- c("7","14","28")

    row_labels <- c(cross_apply(errors, times, partial(paste, sep = "-")))

    res <- sapply(fcast_list, fcast_error_table)
    rownames(res) <- row_labels
    return(res)
}


order_error <- function(fcast_list, error_type, error_length) {
    foo <- sapply(fcast_list, function(x) {
        if(is.null(x)) {
            return(NA)
        } else {
            x[error_length, error_type]
        }
    })
    return(order(abs(foo)))
}


mean_error <- function(fcast_list) {
    sapply(fcast_list, function(x) mean(x[,"sMAE"]))
}


best_error <- function(fcast_list) {
    #' The mean of all three error measurments
    return(which.min(mean_error(fcast_list)))
}


################################################################################
## Scenario pos
## Historical telemetric point-of-sale data
################################################################################
train_data_pos <- function(client) {
    #' Training data for scenario 1
    return(client$con$filt[xts_range("2015-07-01", "2016-12-31")])
}

train_data_pos(test_con_del_data[[1]])

### SBA opt
f_pos_sba_opt <- partial(forecast_consumption,
                         ffun = partial(croston, f.type = "SBA.opt"),
                         dfun = train_data_pos)
pos_sba_opt_res <- mclapply(test_con_del_data, f_pos_sba_opt, mc.cores = 8)
saveRDS(pos_sba_opt_res, "../data/master/paper1/pred/pos_sba_opt.rds")

### SBA base
f_pos_sba_base <- partial(forecast_consumption,
                         ffun = partial(croston, f.type = "SBA.base"),
                         dfun = train_data_pos)
pos_sba_base_res <- mclapply(test_con_del_data, f_pos_sba_base, mc.cores = 8)
saveRDS(pos_sba_base_res, "../data/master/paper1/pred/pos_sba_base.rds")

### MAPA
f_pos_mapa <- partial(forecast_consumption,
                         ffun = partial(MAPA, agg = 7),
                         dfun = train_data_pos)
pos_mapa_res <- mclapply(test_con_del_data, f_pos_mapa, mc.cores = 8)
saveRDS(pos_mapa_res, "../data/master/paper1/pred/pos_mapa.rds")

### ETS
f_pos_ets <- partial(forecast_consumption,
                     ffun = ets,
                     dfun = train_data_pos)
pos_ets_res <- mclapply(test_con_del_data, f_pos_ets, mc.cores = 8)
saveRDS(pos_ets_res, "../data/master/paper1/pred/pos_ets.rds")

### ADIDA weekly
f_pos_ADIDA_weekly <- partial(forecast_consumption,
                              ffun = partial(ADIDA,
                                             binsize = 7 + 1,
                                             ffun = partial(croston, f.type = "SBA.opt")),
                              dfun = train_data_pos)
pos_ADIDA_weekly_res <- mclapply(test_con_del_data, f_pos_ADIDA_weekly, mc.cores = 8)
saveRDS(pos_ADIDA_weekly_res, "../data/master/paper1/pred/pos_ADIDA_weekly.rds")

### ADIDA monthly
f_pos_ADIDA_monthly <- partial(forecast_consumption,
                               ffun = partial(ADIDA,
                                              binsize = 30 + 1,
                                              ffun = partial(croston, f.type = "SBA.opt")),
                               dfun = train_data_pos)
pos_ADIDA_monthly_res <- mclapply(test_con_del_data, f_pos_ADIDA_monthly, mc.cores = 8)
saveRDS(pos_ADIDA_monthly_res, "../data/master/paper1/pred/pos_ADIDA_monthly.rds")

### ASACT weekly
f_pos_ASACT_weekly <- partial(forecast_consumption,
                              ffun = partial(ASACT,
                                             agg.time = 7,
                                             ffun = ets,
                                             h = 7),
                              dfun = train_data_pos)
pos_ASACT_weekly_res <- mclapply(test_con_del_data, f_pos_ASACT_weekly, mc.cores = 8)
saveRDS(pos_ASACT_weekly_res, "../data/master/paper1/pred/pos_ASACT_weekly.rds")

### ASACT monthly
f_pos_ASACT_monthly <- partial(forecast_consumption,
                               ffun = partial(ASACT2,
                                              agg.time = "month",
                                              ffun = ets,
                                              h = 28),
                               dfun = train_data_pos)

pos_ASACT_monthly_res <- mclapply(test_con_del_data, f_pos_ASACT_monthly, mc.cores = 8)
saveRDS(pos_ASACT_monthly_res, "../data/master/paper1/pred/pos_ASACT_monthly.rds")

### ARIMA
f_pos_ARIMA <- partial(forecast_consumption,
                       ffun = ARIMA,
                       dfun = train_data_pos)
pos_ARIMA_res <- mclapply(test_con_del_data, f_pos_ARIMA, mc.cores = 4)
saveRDS(pos_ARIMA_res, "../data/master/paper1/pred/pos_ARIMA.rds")

## SNAIVE
f_pos_snaive <- partial(forecast_consumption,
                        ffun = snaive,
                        dfun = train_data_pos)
pos_snaive_res <- mclapply(test_con_del_data, f_pos_snaive, mc.cores = 8)
saveRDS(pos_snaive_res, "../data/master/paper1/pred/pos_snaive.rds")


pos_sba_opt_res <- readRDS("../data/master/paper1/pred/pos_sba_opt.rds")
pos_sba_base_res <- readRDS("../data/master/paper1/pred/pos_sba_base.rds")
pos_mapa_res <- readRDS("../data/master/paper1/pred/pos_mapa.rds")
pos_ets_res <- readRDS("../data/master/paper1/pred/pos_ets.rds")
pos_ADIDA_weekly_res <- readRDS("../data/master/paper1/pred/pos_ADIDA_weekly.rds")
pos_ADIDA_monthly_res <- readRDS("../data/master/paper1/pred/pos_ADIDA_monthly.rds")
pos_ASACT_weekly_res <- readRDS("../data/master/paper1/pred/pos_ASACT_weekly.rds")
pos_ASACT_monthly_res <- readRDS("../data/master/paper1/pred/pos_ASACT_monthly.rds")

pos_model_list <- list(pos_sba_opt_res,
                       pos_sba_base_res,
                       pos_mapa_res,
                       pos_ets_res,
                       pos_ADIDA_weekly_res,
                       pos_ADIDA_monthly_res,
                       pos_ASACT_weekly_res,
                       pos_ASACT_monthly_res)

names(pos_model_list) <- c("SBA opt", "SBA base", "MAPA", "ets", "ADIDA weekly",
                           "ADIDA monthly", "ASACT weekly", "ASACT monthly")

## Error table
fcast_list_error_table(pos_model_list)


################################################################################
## Scenario 2
## Historical delivery data only
################################################################################
train_data_del <- function(client) {
    #' Training data for scenario 1
    return(client$del$filt[xts_range("2015-07-01", "2016-12-31")])
}

foo <- train_data_del(test_con_del_data[[1]])

### SBA opt
f_del_sba_opt <- partial(forecast_consumption,
                         ffun = partial(croston, f.type = "SBA.opt"),
                         dfun = train_data_del)
del_sba_opt_res <- mclapply(test_con_del_data, f_del_sba_opt, mc.cores = 8)
saveRDS(del_sba_opt_res, "../data/master/paper1/pred/del_sba_opt.rds")

### SBA base
f_del_sba_base <- partial(forecast_consumption,
                          ffun = partial(croston, f.type = "SBA.base"),
                          dfun = train_data_del)
del_sba_base_res <- mclapply(test_con_del_data, f_del_sba_base, mc.cores = 8)
saveRDS(del_sba_base_res, "../data/master/paper1/pred/del_sba_base.rds")

### MAPA
f_del_mapa <- partial(forecast_consumption,
                      ffun = partial(MAPA, agg = 7),
                      dfun = train_data_del)
del_mapa_res <- mclapply(test_con_del_data, f_del_mapa, mc.cores = 8)
saveRDS(del_mapa_res, "../data/master/paper1/pred/del_mapa.rds")

### ETS
f_del_ets <- partial(forecast_consumption,
                     ffun = ets,
                     dfun = train_data_del)
del_ets_res <- mclapply(test_con_del_data, f_del_ets, mc.cores = 8)
saveRDS(del_ets_res, "../data/master/paper1/pred/del_ets.rds")

### ADIDA weekly
f_del_ADIDA_weekly <- partial(forecast_consumption,
                              ffun = partial(ADIDA,
                                             binsize = 7 + 1,
                                             ffun = partial(croston, f.type = "SBA.opt")),
                              dfun = train_data_del)
del_ADIDA_weekly_res <- mclapply(test_con_del_data, f_del_ADIDA_weekly, mc.cores = 8)
saveRDS(del_ADIDA_weekly_res, "../data/master/paper1/pred/del_ADIDA_weekly.rds")

### ADIDA monthly
f_del_ADIDA_monthly <- partial(forecast_consumption,
                               ffun = partial(ADIDA,
                                              binsize = 30 + 1,
                                              ffun = partial(croston, f.type = "SBA.opt")),
                               dfun = train_data_del)
del_ADIDA_monthly_res <- mclapply(test_con_del_data, f_del_ADIDA_monthly, mc.cores = 8)
saveRDS(del_ADIDA_monthly_res, "../data/master/paper1/pred/del_ADIDA_monthly.rds")

### ASACT weekly
f_del_ASACT_weekly <- partial(forecast_consumption,
                              ffun = partial(ASACT,
                                             agg.time = 7,
                                             ffun = ets,
                                             h = 7),
                              dfun = train_data_del)
del_ASACT_weekly_res <- mclapply(test_con_del_data, f_del_ASACT_weekly, mc.cores = 8)
saveRDS(del_ASACT_weekly_res, "../data/master/paper1/pred/del_ASACT_weekly.rds")

### ASACT monthly
f_del_ASACT_monthly <- partial(forecast_consumption,
                               ffun = partial(ASACT2,
                                              agg.time = "month",
                                              ffun = ets,
                                              h = 28),
                               dfun = train_data_del)
del_ASACT_monthly_res <- mclapply(test_con_del_data, f_del_ASACT_monthly, mc.cores = 8)
saveRDS(del_ASACT_monthly_res, "../data/master/paper1/pred/del_ASACT_monthly.rds")

### ARIMA
f_del_ARIMA <- partial(forecast_consumption,
                       ffun = ARIMA,
                       dfun = train_data_del)
del_ARIMA_res <- mclapply(test_con_del_data, f_del_ARIMA, mc.cores = 4)
saveRDS(del_ARIMA_res, "../data/master/paper1/pred/del_ARIMA.rds")

## SNAIVE
f_del_snaive <- partial(forecast_consumption,
                        ffun = snaive,
                        dfun = train_data_del)
del_snaive_res <- mclapply(test_con_del_data, f_del_snaive, mc.cores = 8)
saveRDS(del_snaive_res, "../data/master/paper1/pred/del_snaive.rds")


del_sba_opt_res <- readRDS("../data/master/paper1/pred/del_sba_opt.rds")
del_sba_base_res <- readRDS("../data/master/paper1/pred/del_sba_base.rds")
del_mapa_res <- readRDS("../data/master/paper1/pred/del_mapa.rds")
del_ets_res <- readRDS("../data/master/paper1/pred/del_ets.rds")
del_ADIDA_weekly_res <- readRDS("../data/master/paper1/pred/del_ADIDA_weekly.rds")
del_ADIDA_monthly_res <- readRDS("../data/master/paper1/pred/del_ADIDA_monthly.rds")
del_ASACT_weekly_res <- readRDS("../data/master/paper1/pred/del_ASACT_weekly.rds")
del_ASACT_monthly_res <- readRDS("../data/master/paper1/pred/del_ASACT_monthly.rds")

del_model_list <- list(del_sba_opt_res,
                       del_sba_base_res,
                       del_mapa_res,
                       del_ets_res,
                       del_ADIDA_weekly_res,
                       del_ADIDA_monthly_res,
                       del_ASACT_weekly_res,
                       del_ASACT_monthly_res)

names(del_model_list) <- c("SBA opt", "SBA base", "MAPA", "ets", "ADIDA weekly",
                           "ADIDA monthly", "ASACT weekly", "ASACT monthly")

## Error table
fcast_list_error_table(del_model_list)


################################################################################
## Scenario 3
## Mixte point-of-sale telemetry and delivery data
## The originality of the paper
################################################################################
## Smoothing time series
## There was a paper that would do smoothing all at once. Is it necessary?
preproc_con_data <- function(client, fun) {
    con <- client$con$filt[xts_range("2015-07-01", "2016-12-31")]
    con_smooth <- fun(con)
    con_filt <- filter_full_year(con_smooth, "days")
    return(con_filt)
}


preproc_del_data <- function(client, fun) {
    del <- client$del$filt[xts_range("2015-07-01", "2016-12-31")]
    del_smooth <- fun(del)
    del_filt <- filter_full_year(del_smooth, "days")
    return(del_filt)
}


con_del_mv_series <- function(client, con_fun, del_fun) {
    con <- preproc_con_data(client, con_fun)
    del <- preproc_del_data(client, del_fun)
    mv <- mv_serie(con, del)
    colnames(mv) <- c("con", "del")
    return(mv)
}


## Multivariate clustering
## Smoothing functions
nothing_function <- function(x) {
    #' Returns the input, does nothing
    return(x)
}

###################################
## Nearest neighbors
### 1. Preprocessing
### Del : croston opt
### Con : nothing
train_nn_con_del_data <- c(train_con_del_data, valid_con_del_data)
test_nn_con_del_data <- test_con_del_data

nn_preproc_f <- partial(con_del_mv_series,
                        con_fun = nothing_function,
                        del_fun = partial(croston_smooth, f.type = "SBA.opt"))
train_nn_mv_series <- mclapply(train_nn_con_del_data, nn_preproc_f, mc.cores = 8)
names(train_nn_mv_series) <- sapply(train_nn_con_del_data, function(x) x$cvd$DP)

saveRDS(train_nn_mv_series, "../data/master/paper1/nn/train_nn_mv_series.rds")
train_nn_mv_series <- readRDS("../data/master/paper1/nn/train_nn_mv_series.rds")

### 2. Delivery distance matrix
train_nn_del_series <- lapply(train_nn_mv_series, function(x) x[, "del"])
train_nn_con_series <- lapply(train_nn_mv_series, function(x) x[, "con"])

nn_del_cluster <- my_clustering(train_nn_del_series, 1, distance = "dtw_basic")
train_nn_del_distmat <- nn_del_cluster@distmat

saveRDS(train_nn_del_distmat, "../data/master/paper1/nn/train_nn_del_distmat.rds")
train_nn_del_distmat <- readRDS("../data/master/paper1/nn/train_nn_del_distmat.rds")

### 3. Consumption forecast
train_nn_consumption <- function(client, con_series, distmat, k, shape_method, test = FALSE) {
    #' Nearest neighbor pos consumption forecast
    #' We first predict the nearest k neighbors for i individual's delivery data
    #' We then produce an estimate of the consumption based on a shape_method
    #' We then produce a 4 week forecast on this estimated consumption

    ## ID the client in the distmat
    dp <- client$cvd$DP
    i <- which(rownames(distmat) == dp)

    ## Determine if the distance matrix is insample or out of sample
    if (isSymmetric(distmat)) {
        ## Run it with 1 more
        neighbors <- my_nearest_neighbors(i, distmat, k+1L)
        if (test) {
            ## When testing the code return yourself
            ## You should always be first in the neighbors list
            neighbors <- head(neighbors, 1)
        } else {
            ## Remove the duplicate in sample
            ## It will always be the first in the neighbors list since you are always
            ## nearest to yourself
            neighbors <- tail(neighbors, -1)
        }
    } else {
        neighbors <- my_nearest_neighbors(i, distmat, k)
    }

    ## Combine the neighbors consumption series
    con_series_neighbors <- con_series[neighbors]
    con_train_estimate <- nn_shape(shape_method, con_series_neighbors)

    return(con_train_estimate)
}

all(train_nn_consumption(train_nn_con_del_data[[1]], train_nn_con_series, train_nn_del_distmat, 1, "mean", TRUE) == train_nn_con_series[[1]])

train_nn_consumption(train_nn_con_del_data[[1]], train_nn_con_series, train_nn_del_distmat, 3, "mean")


### 4. Optimizing the number of neighbors on the in sample error
nn_insample_error <- function(ffun) {
     nn_ks <- seq(1, 15, 1)
     nn_error_ks <- vector(mode = "list", length = length(nn_ks))
     for (k in nn_ks) {
         print(k)
         nn_f_error <- partial(forecast_consumption,
                               ffun = ffun,
                               dfun = partial(train_nn_consumption,
                                              con_series = train_nn_con_series,
                                              distmat = train_nn_del_distmat,
                                              k = k,
                                              shape_method = "mean"))
         nn_fcast <- mclapply(train_nn_con_del_data, nn_f_error, mc.cores = 8)
         nn_error_ks[[k]] <- fcast_error_table(nn_fcast)
     }
     return(nn_error_ks)
}

nn_error_ks_sba_opt <- nn_insample_error(partial(croston, f.type = "SBA.opt"))
saveRDS(nn_error_ks_sba_opt, "../data/master/paper1/nn/error_ks_sba_opt.rds")

nn_error_ks_sba_base <- nn_insample_error(partial(croston, f.type = "SBA.base"))
saveRDS(nn_error_ks_sba_base, "../data/master/paper1/nn/error_ks_sba_base.rds")

nn_error_ks_mapa <- nn_insample_error(partial(MAPA, agg = 7))
saveRDS(nn_error_ks_mapa, "../data/master/paper1/nn/error_ks_mapa.rds")

nn_error_ks_ets <- nn_insample_error(ets)
saveRDS(nn_error_ks_ets, "../data/master/paper1/nn/error_ks_ets.rds")

nn_error_ks_ADIDA_weekly <- nn_insample_error(partial(ADIDA,
                                                      binsize = 7 + 1,
                                                      ffun = partial(croston, f.type = "SBA.opt")))
saveRDS(nn_error_ks_ADIDA_weekly, "../data/master/paper1/nn/error_ks_ADIDA_weekly.rds")

nn_error_ks_ADIDA_monthly <- nn_insample_error(partial(ADIDA,
                                                      binsize = 30 + 1,
                                                      ffun = partial(croston, f.type = "SBA.opt")))
saveRDS(nn_error_ks_ADIDA_monthly, "../data/master/paper1/nn/error_ks_ADIDA_monthly.rds")

nn_error_ks_ASACT_weekly <- nn_insample_error(partial(ASACT,
                                                       agg.time = 7,
                                                       ffun = ets,
                                                       h = 7))
saveRDS(nn_error_ks_ASACT_weekly, "../data/master/paper1/nn/error_ks_ASACT_weekly.rds")

nn_error_ks_ASACT_monthly <- nn_insample_error(partial(ASACT,
                                                       agg.time = 30,
                                                       ffun = ets,
                                                       h = 7))
saveRDS(nn_error_ks_ASACT_monthly, "../data/master/paper1/nn/error_ks_ASACT_monthly.rds")


nn_error_ks_sba_opt <- readRDS("../data/master/paper1/nn/error_ks_sba_opt.rds")
nn_error_ks_sba_base <- readRDS("../data/master/paper1/nn/error_ks_sba_base.rds")
nn_error_ks_mapa <- readRDS("../data/master/paper1/nn/error_ks_mapa.rds")
nn_error_ks_ets <- readRDS("../data/master/paper1/nn/error_ks_ets.rds")
nn_error_ks_ADIDA_weekly <- readRDS("../data/master/paper1/nn/error_ks_ADIDA_weekly.rds")
nn_error_ks_ADIDA_monthly <- readRDS("../data/master/paper1/nn/error_ks_ADIDA_monthly.rds")
nn_error_ks_ASACT_weekly <- readRDS("../data/master/paper1/nn/error_ks_ASACT_weekly.rds")
nn_error_ks_ASACT_monthly <- readRDS("../data/master/paper1/nn/error_ks_ASACT_monthly.rds")

## Graph of the error as a function of k
df <- reshape2::melt(data.frame(sba_opt = mean_error(nn_error_ks_sba_opt),
                                sba_base = mean_error(nn_error_ks_sba_base),
                                mapa = mean_error(nn_error_ks_mapa),
                                ets = mean_error(nn_error_ks_ets),
                                ADIDA_weekly = mean_error(nn_error_ks_ADIDA_weekly),
                                ADIDA_monthly = mean_error(nn_error_ks_ADIDA_monthly),
                                ASACT_weekly = mean_error(nn_error_ks_ASACT_weekly),
                                ASACT_monthly = mean_error(nn_error_ks_ASACT_monthly)))
df$k <- rep(seq(1,15,1), times = 8)
df$variable <- factor(df$variable)
setnames(df, "variable", "Model")
levels(df$Model) <- list("SBA opt" = "sba_opt",
                         "SBA 0.05" = "sba_base",
                         "MAPA" = "mapa",
                         "ETS" = "ets",
                         "ADIDA(7+1, SBA opt, EQW)" = "ADIDA_weekly",
                         "ADIDA(30+1, SBA opt, EQW)" = "ADIDA_monthly",
                         "ASACT(week, ETS, EQW)" = "ASACT_weekly",
                         "ASACT(month, ETS, EQW)" = "ASACT_monthly")
df  <- df %>%
    group_by(Model) %>%
    mutate(size = ifelse(min(value) == value, 2, 1))


ggplot2::ggplot(aes(x = k, y = value, group = Model, color = Model, shape = Model), data = df) +
    scale_shape_manual(values=1:8) +
    geom_point(aes(stroke = size), size = 1) +
    geom_line() +
    xlab("k") +
    scale_x_continuous(breaks = c(1,5,10,15)) +
    ylab("Average sMAE") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme(legend.position = "top")

ggsave("./papers/paper1/nn_error_ks.eps", device = cairo_ps, width = 190, height = 190, units = "mm", dpi = 1000)
ggsave("./papers/paper1/nn_error_ks.png", device = "png", width = 190, height = 190, units = "mm", dpi = 1000)

best_error(nn_error_ks_sba_opt)
best_error(nn_error_ks_sba_base)
best_error(nn_error_ks_mapa)
best_error(nn_error_ks_ets)
best_error(nn_error_ks_ADIDA_weekly)
best_error(nn_error_ks_ADIDA_monthly)
best_error(nn_error_ks_ASACT_weekly)
best_error(nn_error_ks_ASACT_monthly)

## overal best
df[which.min(df$value),]


### 5. Out of sample forecast
## Preprocessing the test data
test_nn_mv_series <- mclapply(test_nn_con_del_data, nn_preproc_f, mc.cores = 8)
names(test_nn_mv_series) <- sapply(test_nn_con_del_data, function(x) x$cvd$DP)

saveRDS(test_nn_mv_series, "../data/master/paper1/nn/test_nn_mv_series.rds")
test_nn_mv_series <- readRDS("../data/master/paper1/nn/test_nn_mv_series.rds")

test_nn_del_series <- lapply(test_nn_mv_series, function(x) x[, "del"])

## Calculating the distance matrix between the test del series and the train del series
test_nn_distmat <- clus_calc_distance_matrix(test_nn_del_series, nn_del_cluster, train_nn_del_series)
saveRDS(test_nn_distmat, "../data/master/paper1/nn/test_nn_distmat.rds")

test_nn_distmat <- readRDS("../data/master/paper1/nn/test_nn_distmat.rds")

## Forecasting the consumption for the test data using NN and then calculating the forecast
## error
nn_outsample_error <- function(ffun, k) {
    test_nn_pred_f <- partial(forecast_consumption,
                              ffun = ffun,
                              dfun = partial(train_nn_consumption,
                                             con_series = train_nn_con_series,
                                             distmat = test_nn_distmat,
                                             k = k,
                                             shape_method = "mean"))
    test_nn_res <- mclapply(test_nn_con_del_data, test_nn_pred_f, mc.cores = 8)
    structure(
        list(
            table = fcast_error_table(test_nn_res),
            k = k,
            fcasts = test_nn_res
        )
    )
}

nn_test_sba_opt <- nn_outsample_error(ffun = partial(croston, f.type = "SBA.opt"),
                                      k = best_error(nn_error_ks_sba_opt))
saveRDS(nn_test_sba_opt, "../data/master/paper1/nn/test_sba_opt.rds")

nn_test_sba_base <- nn_outsample_error(ffun = partial(croston, f.type = "SBA.base"),
                                       k = best_error(nn_error_ks_sba_base))
saveRDS(nn_test_sba_base, "../data/master/paper1/nn/test_sba_base.rds")

nn_test_mapa <- nn_outsample_error(ffun = partial(MAPA, agg = 7),
                                   k = best_error(nn_error_ks_mapa))
saveRDS(nn_test_mapa, "../data/master/paper1/nn/test_mapa.rds")

nn_test_ets <- nn_outsample_error(ffun = ets,
                                  k = best_error(nn_error_ks_ets))
saveRDS(nn_test_ets, "../data/master/paper1/nn/test_ets")

nn_test_ADIDA_weekly <- nn_outsample_error(ffun = partial(ADIDA,
                                                          binsize = 7 + 1,
                                                          ffun = partial(croston, f.type = "SBA.opt")),
                                           k = best_error(nn_error_ks_ADIDA_weekly))
saveRDS(nn_test_ADIDA_weekly, "../data/master/paper1/nn/test_ADIDA_weekly.rds")

nn_test_ADIDA_monthly <- nn_outsample_error(ffun = partial(ADIDA,
                                                           binsize = 30 + 1,
                                                           ffun = partial(croston, f.type = "SBA.opt")),
                                            k = best_error(nn_error_ks_ADIDA_monthly))
saveRDS(nn_test_ADIDA_monthly, "../data/master/paper1/nn/test_ADIDA_monthly.rds")

nn_test_ASACT_weekly <- nn_outsample_error(ffun = partial(ASACT,
                                                          agg.time = 7,
                                                          ffun = ets,
                                                          h = 7),
                                           k = best_error(nn_error_ks_ASACT_weekly))
saveRDS(nn_test_ASACT_weekly, "../data/master/paper1/nn/test_ASACT_weekly.rds")

nn_test_ASACT_monthly <- nn_outsample_error(ffun = partial(ASACT,
                                                           agg.time = 30,
                                                           ffun = ets,
                                                           h = 7),
                                            k = best_error(nn_error_ks_ASACT_monthly))
saveRDS(nn_test_ASACT_monthly, "../data/master/paper1/nn/test_ASACT_monthly.rds")


nn_test_sba_opt <- readRDS("../data/master/paper1/nn/test_sba_opt.rds")
nn_test_sba_base <- readRDS("../data/master/paper1/nn/test_sba_base.rds")
nn_test_mapa <- readRDS("../data/master/paper1/nn/test_mapa.rds")
nn_test_ets <- readRDS("../data/master/paper1/nn/test_ets.rds")
nn_test_ADIDA_weekly <- readRDS("../data/master/paper1/nn/test_ADIDA_weekly.rds")
nn_test_ADIDA_monthly <- readRDS("../data/master/paper1/nn/test_ADIDA_monthly.rds")
nn_test_ASACT_weekly <- readRDS("../data/master/paper1/nn/test_ASACT_weekly.rds")
nn_test_ASACT_monthly <- readRDS("../data/master/paper1/nn/test_ASACT_monthly.rds")


nn_model_list <- list(nn_test_sba_opt$fcast,
                      nn_test_sba_base$fcast,
                      nn_test_mapa$fcast,
                      nn_test_ets$fcast,
                      nn_test_ADIDA_weekly$fcast,
                      nn_test_ADIDA_monthly$fcast,
                      nn_test_ASACT_weekly$fcast,
                      nn_test_ASACT_monthly$fcast)

names(nn_model_list) <- c("SBA opt", "SBA base", "MAPA", "ets", "ADIDA weekly",
                          "ADIDA monthly", "ASACT weekly", "ASACT monthly")

fcast_list_error_table(nn_model_list)

###################################
## Clustering approach
### 1. Preprocessing
### Del : croston base
### Con : nothing
train_clus_con_del_data <- train_con_del_data
valid_clus_con_del_data <- valid_con_del_data
test_clus_con_del_data <- test_con_del_data

clus_preproc_f <- partial(con_del_mv_series,
                          con_fun = nothing_function,
                          del_fun = partial(croston_smooth, f.type = "SBA.opt"))

train_clus_mv_series <- mclapply(train_clus_con_del_data, clus_preproc_f, mc.cores = 8)
names(train_clus_mv_series) <- sapply(train_clus_con_del_data, function(x) x$cvd$DP)
saveRDS(train_clus_mv_series, "../data/master/paper1/clus/train_clus_mv_series.rds")

valid_clus_mv_series <- mclapply(valid_clus_con_del_data, clus_preproc_f, mc.cores = 8)
names(valid_clus_mv_series) <- sapply(valid_clus_con_del_data, function(x) x$cvd$DP)
saveRDS(valid_clus_mv_series, "../data/master/paper1/clus/valid_clus_mv_series.rds")

test_clus_mv_series <- mclapply(test_clus_con_del_data, clus_preproc_f, mc.cores = 8)
names(test_clus_mv_series) <- sapply(test_clus_con_del_data, function(x) x$cvd$DP)
saveRDS(test_clus_mv_series, "../data/master/paper1/clus/test_clus_mv_series.rds")

train_clus_mv_series <- readRDS("../data/master/paper1/clus/train_clus_mv_series.rds")
valid_clus_mv_series <- readRDS("../data/master/paper1/clus/valid_clus_mv_series.rds")
test_clus_mv_series <- readRDS("../data/master/paper1/clus/test_clus_mv_series.rds")

### 2. Multivariate Clustering
train_clus_del_series <- lapply(train_clus_mv_series, function(x) x[, "del"])
train_clus_con_series <- lapply(train_clus_mv_series, function(x) x[, "con"])

init_train_mv_cluster <- my_clustering(train_clus_mv_series,
                                       2,
                                       distance = "dtw_basic",
                                       type = "hierarchical")

## Avoid recalculating the distance matrix
clus_control <- init_train_mv_cluster@control
clus_control$distmat <- init_train_mv_cluster@distmat

saveRDS(clus_control, "../data/master/paper1/clus/clus_control.rds")
clus_control <- readRDS("../data/master/paper1/clus/clus_control.rds")


### 3. Consumption forecast
train_clus_consumption <- function(client, clus, con_series, del_series, shape_method, insample) {
    #' Clustering approach pos consumption forecast
    #' 1. Cluster the multivariate time series
    #' 2. Forecast the consumption for each cluster by using a shape function
    #' 3. Assign client's delivery data to a cluster by minimizing the distance
    #' 4. Client's forecasted consumption to the client is based on which cluster assigned to him
    clusters <- clus@cluster

    ## The shaped consumption time series for each cluster
    con_series_clusters <- clus_shapes(clus, shape_method, con_series)

    dp <- client$cvd$DP

    if (insample) {
        ## If it's an insample observations, then it has already been clustered
        pred_clus <- clusters[dp]
    } else {
        ## If it is an out-of-sample observation, we assign the cluster only based
        ## on  distance of the delivery time series to the delivery centroids
        client_del_serie <- del_series[dp]

        ## Calculate the distance matrix with the cluster's distance function
        dist_mat <- clus_calc_distance_matrix(client_del_serie, clus, del_centroids(clus))

        ## Apply the cluster's clustering function
        pred_clus <- clus@family@cluster(distmat = dist_mat, m = clus@control$fuzziness)
    }

    ## Assign the cluster's consumption time series to the client's predicted cluster
    pred_con <- con_series_clusters[[pred_clus]]

    return(pred_con)
}

## With 1 cluster, the in sample indidivual should be given the mean of all con series
test_clus_1 <- my_clustering(train_clus_mv_series, 1, distance = "dtw_basic", type = "hierarchical", control = clus_control)
all(train_clus_consumption(train_clus_con_del_data[[1]], test_clus_1, train_clus_con_series, train_clus_del_series, "mean", TRUE) == mean_series(train_clus_con_series))

### 4. Optimizing the number of clusters on the validation set error
valid_clus_del_series <- lapply(valid_clus_mv_series, function(x) x[, "del"])

## Test
clus <- my_clustering(train_clus_mv_series, 2, trace = FALSE, distance = "dtw_basic", control = clus_control, type = "hierarchical")
clus_f_error <- partial(forecast_consumption,
                        ffun = partial(croston, f.type = "SBA.base"),
                        dfun = partial(train_clus_consumption,
                                       clus = clus,
                                       con_series = train_clus_con_series,
                                       del_series = valid_clus_del_series,
                                       shape_method = "mean",
                                       insample = FALSE))
clus_f_error(valid_clus_con_del_data[[1]])

## Iterating over many ks
clus_valid_error <- function(ffun, clus_ks = NULL) {
    if (is.null(clus_ks)) {
        clus_ks <- c(1, seq(5, 370, by= 5))
    }

    clus_error_ks <- vector(mode = "list", length = 400)

    for (k in clus_ks) {
        print(k)
        if (!is.null(clus_error_ks[[k]])) {
            next
        }
        clus <- my_clustering(train_clus_mv_series, k, trace = FALSE,
                              distance = "dtw_basic", control = clus_control,
                              type = "hierarchical")

        clus_f_error <- partial(forecast_consumption,
                                ffun = ffun,
                                dfun = partial(train_clus_consumption,
                                               clus = clus,
                                               con_series = train_clus_con_series,
                                               del_series = valid_clus_del_series,
                                               shape_method = "mean",
                                               insample = FALSE))
        clus_fcast <- mclapply(valid_clus_con_del_data, clus_f_error, mc.cores = 8)
        clus_error_ks[[k]] <- fcast_error_table(clus_fcast)
    }
    return(clus_error_ks)
}

clus_error_ks_sba_opt <- clus_valid_error(ffun = partial(croston, f.type = "SBA.opt"))
saveRDS(clus_error_ks_sba_opt, "../data/master/paper1/clus/error_ks_sba_opt.rds")

clus_error_ks_sba_base <- clus_valid_error(ffun = partial(croston, f.type = "SBA.base"))
saveRDS(clus_error_ks_sba_base, "../data/master/paper1/clus/error_ks_sba_base.rds")

clus_error_ks_mapa <- clus_valid_error(ffun = partial(MAPA, agg = 7))
saveRDS(clus_error_ks_mapa, "../data/master/paper1/clus/error_ks_mapa.rds")

clus_error_ks_ets <- clus_valid_error(ets)
saveRDS(clus_error_ks_ets, "../data/master/paper1/clus/error_ks_ets.rds")

clus_error_ks_ADIDA_weekly <- clus_valid_error(ffun = partial(ADIDA,
                                                              binsize = 7 + 1,
                                                              ffun = partial(croston, f.type = "SBA.opt")))
saveRDS(clus_error_ks_ADIDA_weekly, "../data/master/paper1/clus/error_ks_ADIDA_weekly.rds")

clus_error_ks_ADIDA_monthly <- clus_valid_error(ffun = partial(ADIDA,
                                                               binsize = 30 + 1,
                                                               ffun = partial(croston, f.type = "SBA.opt")))
saveRDS(clus_error_ks_ADIDA_monthly, "../data/master/paper1/clus/error_ks_ADIDA_monthly.rds")

clus_error_ks_ASACT_weekly <- clus_valid_error(partial(ASACT,
                                                       agg.time = 7,
                                                       ffun = ets,
                                                       h = 7))
saveRDS(clus_error_ks_ASACT_weekly, "../data/master/paper1/clus/error_ks_ASACT_weekly.rds")

clus_error_ks_ASACT_monthly <- clus_valid_error(partial(ASACT,
                                                        agg.time = 30,
                                                        ffun = ets,
                                                        h = 7))
saveRDS(clus_error_ks_ASACT_monthly, "../data/master/paper1/clus/error_ks_ASACT_monthly.rds")


clus_error_ks_sba_opt <- readRDS("../data/master/paper1/clus/error_ks_sba_opt.rds")
clus_error_ks_sba_base <- readRDS("../data/master/paper1/clus/error_ks_sba_base.rds")
clus_error_ks_mapa <- readRDS("../data/master/paper1/clus/error_ks_mapa.rds")
clus_error_ks_ets <- readRDS("../data/master/paper1/clus/error_ks_ets.rds")
clus_error_ks_ADIDA_weekly <- readRDS("../data/master/paper1/clus/error_ks_ADIDA_weekly.rds")
clus_error_ks_ADIDA_monthly <- readRDS("../data/master/paper1/clus/error_ks_ADIDA_monthly.rds")
clus_error_ks_ASACT_weekly <- readRDS("../data/master/paper1/clus/error_ks_ASACT_weekly.rds")
clus_error_ks_ASACT_monthly <- readRDS("../data/master/paper1/clus/error_ks_ASACT_monthly.rds")

## Cut it down to half the lenght of the training dataset
length(train_con_del_data)/2

clus_error_ks_sba_opt <- clus_error_ks_sba_opt[1:370]
clus_error_ks_sba_base <- clus_error_ks_sba_base[1:370]
clus_error_ks_mapa <- clus_error_ks_mapa[1:370]
clus_error_ks_ets <- clus_error_ks_ets[1:370]
clus_error_ks_ADIDA_weekly <- clus_error_ks_ADIDA_weekly[1:370]
clus_error_ks_ADIDA_monthly <- clus_error_ks_ADIDA_monthly[1:370]
clus_error_ks_ASACT_weekly <- clus_error_ks_ASACT_weekly[1:370]
clus_error_ks_ASACT_monthly <- clus_error_ks_ASACT_monthly[1:370]


## Plot of the error as a function of k
df <- reshape2::melt(data.frame(sba_opt = mean_error(clus_error_ks_sba_opt),
                                sba_base = mean_error(clus_error_ks_sba_base),
                                mapa = mean_error(clus_error_ks_mapa),
                                ets = mean_error(clus_error_ks_ets),
                                ADIDA_weekly = mean_error(clus_error_ks_ADIDA_weekly),
                                ADIDA_monthly = mean_error(clus_error_ks_ADIDA_monthly),
                                ASACT_weekly = mean_error(clus_error_ks_ASACT_weekly),
                                ASACT_monthly = mean_error(clus_error_ks_ASACT_monthly)))
df$k <- rep(seq(1,370,1), times = 8)
df <- na.omit(df)
df$variable <- factor(df$variable)
setnames(df, "variable", "Model")
levels(df$Model) <- list("SBA opt" = "sba_opt",
                         "SBA 0.05" = "sba_base",
                         "MAPA" = "mapa",
                         "ETS" = "ets",
                         "ADIDA(7+1, SBA opt, EQW)" = "ADIDA_weekly",
                         "ADIDA(30+1, SBA opt, EQW)" = "ADIDA_monthly",
                         "ASACT(week, ETS, EQW)" = "ASACT_weekly",
                         "ASACT(month, ETS, EQW)" = "ASACT_monthly")
df  <- df %>%
    group_by(Model) %>%
    mutate(size = ifelse(min(value) == value, 2, 1))


ggplot2::ggplot(aes(x = k, y = value, group = Model, color = Model, shape = Model), data = df) +
    scale_shape_manual(values=1:8) +
    geom_point(aes(stroke = size), size = 1) +
    geom_line() +
    xlab("k") +
    ylab("Average sMAE") +
    theme(legend.position = "top")


ggsave("./papers/paper1/clus_error_ks.eps", device = cairo_ps, width = 190, height = 190, units = "mm", dpi = 1000)
ggsave("./papers/paper1/clus_error_ks.png", device = "png", width = 190, height = 190, units = "mm", dpi = 1000)


best_error(clus_error_ks_sba_opt)
best_error(clus_error_ks_sba_base)
best_error(clus_error_ks_mapa)
best_error(clus_error_ks_ets)
best_error(clus_error_ks_ADIDA_weekly)
best_error(clus_error_ks_ADIDA_monthly)
best_error(clus_error_ks_ASACT_weekly)
best_error(clus_error_ks_ASACT_monthly)

## Best overal
df[which.min(df$value),]


### 5. Out of sample forecast
## Preprocessing the test data
test_clus_del_series <- lapply(test_clus_mv_series, function(x) x[, "del"])

## Forecasting the consumption for the test data using clustering and then calculating the forecast
## error
clus_outsample_error <- function(ffun, k) {
    opt_clus <- my_clustering(train_clus_mv_series, k, trace = FALSE,
                              distance = "dtw_basic", control = clus_control,
                              type = "hierarchical")

    test_clus_pred_f <- partial(forecast_consumption,
                                ffun = ffun,
                                dfun = partial(train_clus_consumption,
                                               clus = opt_clus,
                                               con_series = train_clus_con_series,
                                               del_series = test_clus_del_series,
                                               shape_method = "mean",
                                               insample = FALSE))

    test_clus_res <- mclapply(test_clus_con_del_data, test_clus_pred_f, mc.cores = 8)
    structure(
        list(
            table = fcast_error_table(test_clus_res),
            k = k,
            fcasts = test_clus_res
        )
    )
}

clus_test_sba_opt <- clus_outsample_error(ffun = partial(croston, f.type = "SBA.opt"),
                                          k = best_error(clus_error_ks_sba_opt))
saveRDS(clus_test_sba_opt, "../data/master/paper1/clus/test_sba_opt.rds")

clus_test_sba_base <- clus_outsample_error(ffun = partial(croston, f.type = "SBA.base"),
                                           k = best_error(clus_error_ks_sba_base))
saveRDS(clus_test_sba_base, "../data/master/paper1/clus/test_sba_base.rds")

clus_test_mapa <- clus_outsample_error(ffun = partial(MAPA, agg = 7),
                                       k = best_error(clus_error_ks_mapa))
saveRDS(clus_test_mapa, "../data/master/paper1/clus/test_mapa.rds")

clus_test_ets <- clus_outsample_error(ffun = ets,
                                  k = best_error(clus_error_ks_ets))
saveRDS(clus_test_ets, "../data/master/paper1/clus/test_ets")

clus_test_ADIDA_weekly <- clus_outsample_error(ffun = partial(ADIDA,
                                                              binsize = 7 + 1,
                                                              ffun = partial(croston, f.type = "SBA.opt")),
                                               k = best_error(clus_error_ks_ADIDA_weekly))
saveRDS(clus_test_ADIDA_weekly, "../data/master/paper1/clus/test_ADIDA_weekly.rds")

clus_test_ADIDA_monthly <- clus_outsample_error(ffun = partial(ADIDA,
                                                               binsize = 30 + 1,
                                                               ffun = partial(croston, f.type = "SBA.opt")),
                                                k = best_error(clus_error_ks_ADIDA_monthly))
saveRDS(clus_test_ADIDA_monthly, "../data/master/paper1/clus/test_ADIDA_monthly.rds")

clus_test_ASACT_weekly <- clus_outsample_error(ffun = partial(ASACT,
                                                              agg.time = 7,
                                                              ffun = ets,
                                                              h = 7),
                                               k = best_error(clus_error_ks_ASACT_weekly))
saveRDS(clus_test_ASACT_weekly, "../data/master/paper1/clus/test_ASACT_weekly.rds")

clus_test_ASACT_monthly <- clus_outsample_error(ffun = partial(ASACT,
                                                               agg.time = 30,
                                                               ffun = ets,
                                                               h = 7),
                                                k = best_error(clus_error_ks_ASACT_monthly))
saveRDS(clus_test_ASACT_monthly, "../data/master/paper1/clus/test_ASACT_monthly.rds")


clus_test_sba_opt <- readRDS("../data/master/paper1/clus/test_sba_opt.rds")
clus_test_sba_base <- readRDS("../data/master/paper1/clus/test_sba_base.rds")
clus_test_mapa <- readRDS("../data/master/paper1/clus/test_mapa.rds")
clus_test_ets <- readRDS("../data/master/paper1/clus/test_ets.rds")
clus_test_ADIDA_weekly <- readRDS("../data/master/paper1/clus/test_ADIDA_weekly.rds")
clust_test_ADIDA_monthly <- readRDS("../data/master/paper1/clus/test_ADIDA_monthly.rds")
clus_test_ASACT_weekly <- readRDS("../data/master/paper1/clus/test_ASACT_weekly.rds")
clus_test_ASACT_monthly <- readRDS("../data/master/paper1/clus/test_ASACT_monthly.rds")


clus_model_list <- list(clus_test_sba_opt$fcast,
                        clus_test_sba_base$fcast,
                        clus_test_mapa$fcast,
                        clus_test_ets$fcast,
                        clus_test_ADIDA_weekly$fcast,
                        clus_test_ADIDA_monthly$fcast,
                        clus_test_ASACT_weekly$fcast,
                        clus_test_ASACT_monthly$fcast)

names(clus_model_list) <- c("SBA opt", "SBA base", "MAPA", "ets", "ADIDA weekly",
                          "ADIDA monthly", "ASACT weekly", "ASACT monthly")


## Graph
mult_graph <- function(i) {
    con <- con_del_data[[i]]$con$filt
    raw_tel <- con_del_data[[i]]$cvd$tank[[1]]$telemetry.serie
    del <- con_del_data[[i]]$del$filt

    agg_tel <- apply.daily(xts(x=raw_tel$level, order.by = raw_tel$datetime), mean)
    print(i)
    tel <- xts(x=agg_tel, order.by = seq(from=as.Date(index(head(agg_tel,1)))-1, to=as.Date(index(tail(agg_tel, 1))), by="days"))
    stock <- tel[xts_range("2015-07-01", "2017-01-31")]

    merged = merge(demand=con, stock=stock, delivery=del)
    names(merged) <- c("demand", "stock", "delivery")
    plot.zoo(merged, plot.type = "multiple", xlab="Time", main="")
}

##70
png(filename="./papers/paper1/example_series.png")
mult_graph(70)
dev.off()

setEPS()
postscript("./papers/paper1/example_series.eps")
mult_graph(70)
dev.off()

mult_graph(70)
ggsave("./papers/paper1/example_series.png", device = "png", width = 190, height = 190, units = "mm", dpi = 1000)
