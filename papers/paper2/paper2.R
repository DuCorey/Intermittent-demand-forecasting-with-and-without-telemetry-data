#' title: paper2.R
#' comments: Code for the second paper
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' work directory
setwd("/home/corey/AL/code")
packrat::init()

#' packages
library(parallel)

#' imports
source("state_event.R")
source("data.R")
source("error.R")
source("matching.R")
source("ADIDA.R")
source("intermittent.R")
source("cluster.R")
source("ndusl.R")
source("forecast.R")
source("purrr.R")
source("dtwclust.R")
source("plots.R")

#' Load data
data_2 <- readRDS("../data/master/data_paper2.rds")
data_2 <- readRDS("../data/master/data_paper2_0ss.rds")


## Create the clus_tel_day data object for paper 2
## Filter time series with a minimun of 2 years
cvd <- readRDS("../data/master/client.rds") %>%
    filter_cvd

make_data_2 <- function(cvd, source, time_scale) {
    ## Get the data
    print("Fetching delivery series")
    del_orig <- lapply(cvd, client_del_xts, source = source, time_scale = time_scale)

    print("Fetching consumption series")
    con_orig <- lapply(cvd, client_con_xts, time_scale = time_scale)

    ## Combine both truth series of not null values for del and con
    f <- function(x) length(x) >= 732
    ind <- sapply(del_orig, f) & sapply(con_orig, f)
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]

    del_orig <- mapply(list, orig = del_orig, SIMPLIFY = FALSE)
    con_orig <- mapply(list, orig = con_orig, SIMPLIFY = FALSE)
    res <- mapply(list, cvd = cvd, del = del_orig, con = con_orig, SIMPLIFY = FALSE)
    return(res)
}


data_2 <- make_data_2(cvd, "tel", "days")
saveRDS(data_2, "../data/master/data_paper2.rds")
data_2 <- readRDS("../data/master/data_paper2.rds")


reattribute_data_2 <- function(client) {
    #' Move the cvd, del, and con attributes of the data under a "data"
    #' heading
    print(client$data$cvd$DP)
    if (is.list(client$data)) {
        ## No need to reattribute the data, it's already there
        return(client)
    } else {
        cvd <- client$cvd
        del <- client$del
        con <- client$con
        return(
            structure(
                list(
                    data = list(cvd = cvd, del = del, con = con)
                )
            )
        )
    }
}


data_2 <- lapply(data_2, reattribute_data_2)


real_next_day_under_safety_level_list <- function(del_list, start_tel_list, con,
                                                  safety_level) {
    #' Determine the ndusl if the input deliveries are a list
    #' This means that the starting telemetry must also be a list
    #' Con is a time series that covers the entire available time
    mapply(del_list, start_tel_list,
           FUN = function(del, starting_tel) {
               tryCatch({
                   amount <- 0
                   a <- 0
                   safety_stock <- starting_tel - safety_level
                   while (amount <= safety_stock) {
                       a <- a + 1
                       amount <- amount + as.numeric(con[del + a])
                   }
                   return(a)
               },
               ## If the loop goes out-of-bounds we catch and return NA
               ## The loop goes out of bounds if there isn't any consumption
               ## data available at that date
               error = function(cond) {
                   return(NA)
               })
           })
}


add_test_del_list <- function(client) {
    #' Add the client's delivery list in the test data
    del_real <- client$data$del$orig
    test <- tail(del_real, 365)
    del_list <- index(test[test > 0])
    client$test$del_list <- del_list

    return(client)
}


add_start_tel <- function(client) {
    ## Determine the starting telemetry amount used when calculating the NDUSL
    ## The starting amount should be the highest amount right after the delivery is complete

    ## Generally that will be the starting amount. Although with some complex deliveries
    ## that happen over multiple days it may not be always be the case
    ## as such there may be some small consumption that counted before the delivery is finished
    ## should be a minor approximation

    del_list <- client$test$del_list
    start_tel <- sapply(del_list,
                        FUN = function(del) {
                            tryCatch({
                                peak_tel_at_del(del, get_client_telemetry(client$data$cvd))
                            },
                            error = function(cond) {
                                ## we can get out of bonds of the telemetry list
                                ## when finding the peak. This catches it and
                                ## return NA
                                return(NA)
                            })
                        })

    ## Filter out the del_list if any are values are NA from the start_tel since
    ## we can't compare to the ground truth
    ind <- !is.na(start_tel)
    client$test$del_list <- del_list[ind]
    client$ndusl$start_tel <- start_tel[ind]

    return(client)
}


add_safety_level <- function(client) {
    safety_level <- get_client_safety_level(client$data$cvd, a = 0)
    client$ndusl$safety_level <- safety_level

    return(client)
}


add_ndusl_real <- function(client) {
    #' Forecast the ndusl using the true consumption data
    del_list <- client$test$del_list
    start_tel <- client$ndusl$start_tel
    real_con <- client$data$con$orig
    safety_level <- client$ndusl$safety_level

    ndusl_real <- real_next_day_under_safety_level_list(del_list, start_tel,
                                                        real_con, safety_level)

    ## Filter out the del_list if any are values are NA from the ndusl_real since
    ## we can't compare to the ground truth
    ind <- !is.na(ndusl_real)
    client$test$del_list <- del_list[ind]
    client$ndusl$start_tel <- start_tel[ind]
    client$ndusl$real <- ndusl_real[ind]

    return(client)
}


## Preparing for the forecasts
data_2 <- end_state(data_2, list(c(add_test_del_list, "apply"),
                                 c(add_start_tel, "parallel", 8),
                                 c(add_safety_level, "apply"),
                                 c(add_ndusl_real, "parallel", 8)))

## Filter out some customers because they cause errors that can't be fixed.
## There's some large underlying problem with their data
blacklist <- c("70346",  # Has deliveries that last multiple days
               "12903")  # Doesn't have any consumption after the deliveries

data_2 <- Filter(function(x) !(x$data$cvd$DP %in% blacklist), data_2)

remove_0_ndusl <- function(client) {
    ## Remove occurences of real ndusl with 0 time.
    ## These are really small bugs that can still occur.
    ndusl <- client$ndusl$real
    ind <- !(ndusl == 0)

    client$ndusl$real <- ndusl[ind]
    client$ndusl$start_tel <- client$ndusl$start_tel[ind]
    client$test$del_list <- client$test$del_list[ind]

    return(client)
}


add_initial_guess_list <- function(client) {
    client$ndusl$guess_list <- client$ndusl$real * 2
    return(client)
}


data_2 <- lapply(data_2, remove_0_ndusl)
data_2 <- lapply(data_2, add_initial_guess_list)

saveRDS(data_2, "../data/master/data_paper2_0ss.rds")


################################################################################
## NDUSL ##
################################################################################

con_real_forecast_train_data <- function(client, del, extra = 0) {
    #' For a given delivery date what is the forecast data
    ## Forecasts on the real consumption data.
    ## This is the best case scenario using all available historical telemetry
    con_real <- client$data$con$orig
    train <- con_real[xts_range(start(con_real), del + extra)]
    return(train)
}


AL_forecast_train_data <- function(client, del, extra = 0) {
    #' For a given delivery date what is the forecast data
    ## Forecasts on 1 month of the real forecast data.
    ## This is the current AL stategy.
    train <- tail(con_real_forecast_train_data(client, del + extra), 31 + extra)

    return(train)
}


del_real_forecast_train_data <- function(client, del, extra = 0) {
    #' For a given delivery date what is the forecast data
    ## Forecasts on the historical delivery data only
    ## This uses our modls for transforming intermittent delivery data into
    ## consumption data.
    del_real <- client$data$del$orig
    train <- del_real[xts_range(start(del_real), del + extra)]

    return(train)
}


try_ndusl_forecast <- function(fmodel, guess, start_tel, safety_level) {
    #' Sometimes the guess provided isn't good enough to find the ndusl on the
    #' forcasted horizon. This recursively finds the ndusl by doubling  the
    #' guess horizon.
    #' This is probably a really dangerous way to do this, because R doesn't
    #' provide very good error handling. Every built-in error is a simpleError
    #' which doesn't allow us to catch specific errors. Which means that any
    #' other kind of error that could be cause when calling this function will
    #' not be passed and instead will be catched by this recursion. Luckily
    #' because the guess grows exponentially. The C stack usage will reach its
    #' limit and cause the process to stop.
    #' Other options to improve this would be to create our own error handlers
    #' as shown in Advanced R by Wickham. However, we'd have to call the error
    #' in the next_day_under_safety_level while loop. Meaning we'd have to
    #' check we aren't out-of-bounds every time we perform a loop. This is
    #' pretty slow.
    #' Another option without using a tryCatch could be to break the while loop
    #' in the next_day_under_safety_level function before we are out-of-bounds
    #' and return a NA value. And wrap the beahvior underneath in it's own
    #' while loop and the exit condition would be until it doesn't return NA.
    #' Similarly to the custom error functions, this is more expensive since we
    #' check for the out-of-bounds condition on every loop of
    #' next_day_under_safety_level.

    tryCatch({
        res <- result_forecast(forecast(fmodel, guess))
        error <- next_day_under_safety_level(res, start_tel, safety_level)
        return(
            structure(
                list(h = guess, ndusl = error, model = compact_forecast(fmodel))
            )
        )
    },
    error = function(cond) {
        new_guess <- guess * 2
        if (new_guess > 365) {
            ## Break here if the ndusl is greater than a year
            ## This can't ever happen since the training data is less than 1 year
            ## Clearly the forecasting model is bad.
            return(
                structure(
                    list(h = 365, ndusl = NA, model = compact_forecast(fmodel))
                )
            )
        } else {
            return(try_ndusl_forecast(fmodel, new_guess, start_tel, safety_level))
        }
    })
}


get_client_ndusl_forecast <- function(client, ffun, dfun, ...) {
    #' Add a forecast for the ndusl
    #' ffun : forecasting function
    #' dfun : training data function
    #' ... : a list of parameters arguments to iterate over when updating the model
    del_list <- client$test$del_list
    safety_level <- client$ndusl$safety_level
    start_tel_list <- client$ndusl$start_tel

    guess_list <- client$ndusl$guess_list

    ## Initial forecast model
    dots <- list(...)
    if (length(dots) == 0) {
        init_model <- ffun(dfun(client, del_list[[1]]))
    } else {
        init_model <- ffun(dfun(client, del_list[[1]]), ...[[1]])
    }

    res <- mapply(del_list, start_tel_list, guess_list, ...,
                  FUN = function(del, start_tel, guess, ...) {
                      train <- dfun(client, del)
                      tryCatch({
                          fmodel <- update(init_model, train, ...)
                      },
                      error = {
                          ## Possible that the new data causes the model to have an error
                          ## As such we recalculate the whole model. This new
                          ## model then becomes our init_model for the rest of
                          ## the forecasts.
                          init_model <<- ffun(train, ...)
                          fmodel <- init_model
                      })
                      error <- try_ndusl_forecast(fmodel, guess, start_tel, safety_level)
                      return(error)
                  },
                  SIMPLIFY = TRUE,
                  USE.NAMES = FALSE)

    return(res)
}

## Example NTUSS series
client <- data_2[[1]]
del <- client$test$del_list[[1]]
start_tel <- client$ndusl$start_tel[[1]]
guess <- client$ndusl$guess_list[[1]]
safety_level <- client$ndusl$safety_level

real_ndusl <- client$ndusl$real[[1]]
real_ndusl
del + real_ndusl

con_data <- tail(con_real_forecast_train_data(client, del, 15), 20)
del_data <- tail(del_real_forecast_train_data(client, del, 15), 20)

foo <- convert_xts_daily(get_client_telemetry(client$data$cvd), first)
tel_data <- foo[xts_range(del, del + 15)]

data <- join_all(list(fortify(con_data), fortify(del_data), fortify(tel_data)))


data$future <- NA
data$future[15] <- data$tel_data[15]
data$future[16] <- data$tel_data[15] - data$con_data[15] - 1.5
data$future[17] <- data$future[16] - data$con_data[16] - 1.5
data$future[18] <- data$future[17] - data$con_data[17]

breaks <- sort(c(del, del + real_ndusl))

ggplot(data = data, aes(x = Index)) +
    geom_bar(aes(y = del_data, fill = "Delivery"), stat = "identity", width = 0.5) +
    geom_line(aes(y = con_data, linetype = "Consumption", colour = "Consumption")) +
    geom_line(aes(y = tel_data, linetype = "Telemetry", colour = "Telemetry")) +
    geom_line(aes(y = safety_level, linetype = "Safety Stock level", colour = "Safety Stock level")) +
    geom_line(aes(y = future, linetype = "Projected Telemetry", color = "Projected Telemetry")) +
    theme_bw() +
    scale_linetype_manual(name = "Legend", values = c("Consumption" = "longdash", "Telemetry" = "solid", "Safety Stock level" = "solid", "Projected Telemetry" = "solid")) +
    scale_fill_manual(name = "Legend", values = c("Delivery" = "grey")) +
    scale_color_manual(name = "Legend", values = c("Consumption" = "grey50", "Telemetry" = "black", "Safety Stock level" = "red", "Projected Telemetry" = "blue")) +
    theme(legend.key = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.position = "top") +
    scale_x_date(breaks = breaks, date_labels =c("Date 1", "Date 2")) +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 9)) +
    ylab("Amount") +
    annotate("errorbarh", y = 25, xmin = del, xmax = del + real_ndusl, height = 5) +
    annotate("text", x = del + real_ndusl/2, y = 30, label = "NTUSS", size = 3)

ggsave("./papers/paper2/series.eps", device = cairo_ps, width = 190, height = 80, units = "mm", dpi = 1000)
ggsave("./papers/paper2/series.png", device = "png", width = 190, height = 80, units = "mm", dpi = 1000)


## In POSIXct
client <- data_2[[1]]
del <- client$test$del_list[[1]]
start_tel <- client$ndusl$start_tel[[1]]
guess <- client$ndusl$guess_list[[1]]
safety_level <- client$ndusl$safety_level

real_ndusl <- client$ndusl$real[[1]]
real_ndusl
del + real_ndusl

con_data <- tail(con_real_forecast_train_data(client, del, 15), 20)
del_data <- tail(del_real_forecast_train_data(client, del, 15), 20)

foo <- convert_xts(get_client_telemetry(client$data$cvd))

tel_data <- head(foo[xts_range(start(con_data), end(con_data))], -20)

data <- join_all(list(fortify(con_data), fortify(del_data)))

data$Index <- as.POSIXct(as.character(data$Index))

data$future <- NA
data$future[15] <- 47.10
data$future[16] <- data$future[15] - data$con_data[15] - 1.5
data$future[17] <- data$future[16] - data$con_data[16] - 1.5
data$future[18] <- data$future[17] - data$con_data[17]

breaks <- sort(c(as.POSIXct(as.character(del)), as.POSIXct(as.character(del + real_ndusl))))

ggplot(data = data, aes(x = Index)) +
    geom_col(aes(y = del_data, fill = "Delivery")) +
    geom_line(aes(y = con_data, linetype = "Consumption", colour = "Consumption")) +
    geom_line(data = fortify(tel_data), aes(x = Index, y = tel_data, linetype = "Telemetry", colour = "Telemetry")) +
    geom_line(aes(y = safety_level, linetype = "Safety Stock level", colour = "Safety Stock level")) +
    geom_line(aes(y = future, linetype = "Projected Telemetry", color = "Projected Telemetry")) +
    theme_bw() +
    scale_linetype_manual(name = "Legend", values = c("Consumption" = "longdash", "Telemetry" = "solid", "Safety Stock level" = "solid", "Projected Telemetry" = "solid")) +
    scale_fill_manual(name = "Legend", values = c("Delivery" = "grey")) +
    scale_color_manual(name = "Legend", values = c("Consumption" = "grey50", "Telemetry" = "black", "Safety Stock level" = "red", "Projected Telemetry" = "blue")) +
    theme(legend.key = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.position = "top") +
    scale_x_datetime(breaks = breaks, date_labels = c("Date 1", "Date 2")) +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 9)) +
    ylab("Amount") +
    annotate("errorbarh", y = 25, xmin = as.POSIXct(as.character(del)), xmax = as.POSIXct(as.character(del + real_ndusl)), height = 5) +
    annotate("text", x = as.POSIXct(as.character(del + real_ndusl/2)), y = 30, label = "NTUSS", size = 3)

ggsave("./papers/paper2/series.eps", device = cairo_ps, width = 190, height = 80, units = "mm", dpi = 1000)
ggsave("./papers/paper2/series.png", device = "png", width = 190, height = 80, units = "mm", dpi = 1000)

ggplot(data = data, aes(x = Index)) +
    geom_col(aes(y = del_data, fill = "Delivery")) +
    geom_line(aes(y = con_data, linetype = "Consumption", colour = "Consumption")) +
    geom_line(data = fortify(tel_data), aes(x = Index, y = tel_data, linetype = "Telemetry", colour = "Telemetry")) +
    geom_line(aes(y = safety_level, linetype = "Safety Stock level", colour = "Safety Stock level")) +
    geom_line(aes(y = future, linetype = "Projected Telemetry", color = "Projected Telemetry")) +
    theme_bw() +
    scale_linetype_manual(name = "Legend", values = c("Consumption" = "longdash", "Telemetry" = "solid", "Safety Stock level" = "solid", "Projected Telemetry" = "solid")) +
    scale_fill_manual(name = "Legend", values = c("Delivery" = "grey")) +
    scale_color_manual(name = "Legend", values = c("Consumption" = "grey50", "Telemetry" = "black", "Safety Stock level" = "red", "Projected Telemetry" = "blue")) +
    theme(legend.key = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.position = "top") +
    scale_x_datetime(breaks = breaks, date_labels = c("Date 1", "Date 2")) +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 9)) +
    ylab("Amount")

ggsave("./papers/paper2/series_no_annotate.png", device = "png", width = 190, height = 80, units = "mm", dpi = 1000)


ggplot(data = data, aes(x = Index)) +
    geom_col(aes(y = del_data, fill = "Delivery")) +
    geom_line(aes(y = con_data, linetype = "Consumption", colour = "Consumption")) +
    geom_line(data = fortify(tel_data), aes(x = Index, y = tel_data, linetype = "Telemetry", colour = "Telemetry")) +
    geom_line(aes(y = safety_level, linetype = "Safety Stock level", colour = "Safety Stock level")) +
    theme_bw() +
    scale_linetype_manual(name = "Legend", values = c("Consumption" = "longdash", "Telemetry" = "solid", "Safety Stock level" = "solid")) +
    scale_fill_manual(name = "Legend", values = c("Delivery" = "grey")) +
    scale_color_manual(name = "Legend", values = c("Consumption" = "grey50", "Telemetry" = "black", "Safety Stock level" = "red")) +
    theme(legend.key = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.position = "top") +
    scale_x_datetime(breaks = breaks, date_labels = c("Date 1", "Date 2")) +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 9)) +
    ylab("Amount")

ggsave("./papers/paper2/series_no_projection.png", device = "png", width = 190, height = 80, units = "mm", dpi = 1000)


## Descriptive statistics on the data
length(data_2)

## Tukey
## Croston decomposition
## Run these after in case of NA's produced by the time shift
decomps_del <- lapply(pluck_list(data_2, "data", "del", "orig"),
                      crost_decomp)
decomps_con <- lapply(pluck_list(data_2, "data", "con", "orig"),
                      crost_decomp)

tukey_decomp_table(pluck_list(data_2, "data", "del", "orig"))
tukey_decomp_table(pluck_list(data_2, "data", "con", "orig"))

## Time series categorisations
intermittent_categorisation(
    pluck_list(data_2, "data", "del", "orig"),
    type = "PKa",
    outplot = "none"
)

intermittent_categorisation(
    pluck_list(data_2, "data", "con", "orig"),
    type = "PKa",
    outplot = "none"
)


## Distribution of the number of deliveries for each client
n_dels <- sapply(data_2, function(x) length(x$test$del_list))
sum(n_dels)
mean(n_dels)

### Sum of all the outliers
sum(n_dels > quantile(n_dels, 0.75) + 1.5*IQR(n_dels))
foo <- sum(n_dels[n_dels > quantile(n_dels, 0.75) + 1.5*IQR(n_dels)])
foo/sum(n_dels)


### Bottom whisker is negative so not necessary
quantile(n_dels, 0.25) - 1.5*IQR(n_dels)

### Pareto plot
### Make buckets of based on the divisors 26, 38, 52
pareto_curve(n_dels, xlab = "Customers", ylab = "Deliveries") +
    theme(text = element_text(size = 7))

ggsave("./papers/paper2/dels_pareto.eps", device = cairo_ps, width = 90, height = 90, units = "mm", dpi = 1000)
ggsave("./papers/paper2/dels_pareto.png", device = "png", width = 90, height = 90, units = "mm", dpi = 1000)

## Real NDUSL
foo <- do.call(c, (sapply(data_2, function(x) x$ndusl$real)))
min(foo)
max(foo)
mean(foo)

ggplot(mapping = aes(foo)) +
    geom_histogram(binwidth = 3, bins = 30) +
    xlab("NTUSS (days)") +
    theme_bw() +
    theme(text = element_text(size = 7))

ggsave("./papers/paper2/ndusl_dist.eps", device = cairo_ps, width = 90, height = 90, units = "mm", dpi = 1000)
ggsave("./papers/paper2/ndusl_dist.png", device = "png", width = 90, height = 90, units = "mm", dpi = 1000)


## NDUSL forecasts
### Consumption models
f_con_ses <- partial(get_client_ndusl_forecast,
                     ffun = ses,
                     dfun = con_real_forecast_train_data)
con_ses_ndusl <- mclapply(data_2, f_con_ses, mc.cores = 8)

saveRDS(con_ses_ndusl, "../data/master/paper2_forecast/con_ses.rds")

saveRDS(con_ses_ndusl, "../data/master/paper2_forecast/con_ses_0ss.rds")


f_con_croston_base <- partial(get_client_ndusl_forecast,
                              ffun = partial(croston, f.type = "SBA.base"),
                              dfun = con_real_forecast_train_data)
con_croston_base_ndusl <- mclapply(data_2, f_con_croston_base, mc.cores = 8)

saveRDS(con_croston_base_ndusl, "../data/master/paper2_forecast/con_crostonbase.rds")

saveRDS(con_croston_base_ndusl, "../data/master/paper2_forecast/con_crostonbase_0ss.rds")


f_con_croston_opt <- partial(get_client_ndusl_forecast,
                             ffun = partial(croston, f.type = "SBA.opt"),
                             dfun = con_real_forecast_train_data)
con_croston_opt_ndusl <- mclapply(data_2, f_con_croston_opt, mc.cores = 8)

saveRDS(con_croston_opt_ndusl, "../data/master/paper2_forecast/con_crostonopt.rds")

saveRDS(con_croston_opt_ndusl, "../data/master/paper2_forecast/con_crostonopt_0ss.rds")


f_con_ets <- partial(get_client_ndusl_forecast,
                     ffun = ets,
                     dfun = con_real_forecast_train_data)
con_ets_ndusl <- mclapply(data_2, f_con_ets, mc.cores = 8)

saveRDS(con_ets_ndusl, "../data/master/paper2_forecast/con_ets.rds")

saveRDS(con_ets_ndusl, "../data/master/paper2_forecast/con_ets_0ss.rds")


f_con_ADIDA_weekly <- partial(get_client_ndusl_forecast,
                              ffun = partial(ADIDA,
                                             binsize = 7 + 1,
                                             ffun = partial(croston, f.type = "SBA.opt")),
                              dfun = con_real_forecast_train_data)
con_ADIDA_weekly_ndusl <- mclapply(data_2, f_con_ADIDA_weekly, mc.cores = 8)

saveRDS(con_ADIDA_weekly_ndusl, "../data/master/paper2_forecast/con_ADIDA_weekly.rds")

saveRDS(con_ADIDA_weekly_ndusl, "../data/master/paper2_forecast/con_ADIDA_weekly_0ss.rds")


f_con_ADIDA_monthly <- partial(get_client_ndusl_forecast,
                              ffun = partial(ADIDA,
                                             binsize = 30 + 1,
                                             ffun = partial(croston, f.type = "SBA.opt")),
                              dfun = con_real_forecast_train_data)

con_ADIDA_monthly_ndusl <- mclapply(data_2, f_con_ADIDA_monthly, mc.cores = 8)

saveRDS(con_ADIDA_monthly_ndusl, "../data/master/paper2_forecast/con_ADIDA_monthly.rds")

saveRDS(con_ADIDA_monthly_ndusl, "../data/master/paper2_forecast/con_ADIDA_monthly_0ss.rds")


f_con_ASACT_weekly <- partial(get_client_ndusl_forecast,
                              ffun = partial(ASACT,
                                             agg.time = 7,
                                             ffun = ets,
                                             h = 7),
                              dfun = con_real_forecast_train_data)
con_smooth_crost_opt_models <- lapply(con_croston_opt_ndusl, function(x) x["model",])
con_ASACT_weekly_ndusl <- mcmapply(data_2,
                                   con_smooth_crost_opt_models,
                                   FUN = f_con_ASACT_weekly,
                                   mc.cores = 8)

saveRDS(con_ASACT_weekly_ndusl, "../data/master/paper2_forecast/con_ASACT_weekly.rds")

saveRDS(con_ASACT_weekly_ndusl, "../data/master/paper2_forecast/con_ASACT_weekly_0ss.rds")


f_con_ASACT_monthly <- partial(get_client_ndusl_forecast,
                               ffun = partial(ASACT,
                                              agg.time = 30,
                                              ffun = ets,
                                              h = 7),
                               dfun = con_real_forecast_train_data)
con_smooth_crost_opt_models <- lapply(con_croston_opt_ndusl, function(x) x["model",])
con_ASACT_monthly_ndusl <- mcmapply(data_2,
                                    con_smooth_crost_opt_models,
                                    FUN = f_con_ASACT_monthly,
                                    mc.cores = 8)

saveRDS(con_ASACT_monthly_ndusl, "../data/master/paper2_forecast/con_ASACT_monthly.rds")

saveRDS(con_ASACT_monthly_ndusl, "../data/master/paper2_forecast/con_ASACT_monthly_0ss.rds")


f_con_MAPA <- partial(get_client_ndusl_forecast,
                      ffun = partial(MAPA,
                                     agg = 7),
                      dfun = con_real_forecast_train_data)
con_MAPA_ndusl <- mclapply(data_2, f_con_MAPA, mc.cores = 8)

saveRDS(con_MAPA_ndusl, "../data/master/paper2_forecast/con_MAPA.rds")

saveRDS(con_MAPA_ndusl, "../data/master/paper2_forecast/con_MAPA_0ss.rds")


### Delivery models
f_del_ses <- partial(get_client_ndusl_forecast,
                     ffun = ses,
                     dfun = del_real_forecast_train_data)
del_ses_ndusl <- mclapply(data_2, f_del_ses, mc.cores = 8)

saveRDS(del_ses_ndusl, "../data/master/paper2_forecast/del_ses.rds")

saveRDS(del_ses_ndusl, "../data/master/paper2_forecast/del_ses_0ss.rds")


f_del_croston_base <- partial(get_client_ndusl_forecast,
                              ffun = partial(croston, f.type = "SBA.base"),
                              dfun = del_real_forecast_train_data)
del_croston_base_ndusl <- mclapply(data_2, f_del_croston_base, mc.cores = 8)

saveRDS(del_croston_base_ndusl, "../data/master/paper2_forecast/del_crostonbase.rds")

saveRDS(del_croston_base_ndusl, "../data/master/paper2_forecast/del_crostonbase_0ss.rds")


f_del_croston_opt <- partial(get_client_ndusl_forecast,
                             ffun = partial(croston, f.type = "SBA.opt"),
                             dfun = del_real_forecast_train_data)
del_croston_opt_ndusl <- mclapply(data_2, f_del_croston_opt, mc.cores = 8)

saveRDS(del_croston_opt_ndusl, "../data/master/paper2_forecast/del_crostonopt.rds")

saveRDS(del_croston_opt_ndusl, "../data/master/paper2_forecast/del_crostonopt_0ss.rds")


f_del_ets <- partial(get_client_ndusl_forecast,
                     ffun = ets,
                     dfun = del_real_forecast_train_data)
del_ets_ndusl <- mclapply(data_2, f_del_ets, mc.cores = 8)

saveRDS(del_ets_ndusl, "../data/master/paper2_forecast/del_ets.rds")

saveRDS(del_ets_ndusl, "../data/master/paper2_forecast/del_ets_0ss.rds")


f_del_ADIDA_weekly <- partial(get_client_ndusl_forecast,
                              ffun = partial(ADIDA,
                                             binsize = 7 + 1,
                                             ffun = partial(croston, f.type = "SBA.opt")),
                              dfun = del_real_forecast_train_data)
del_ADIDA_weekly_ndusl <- mclapply(data_2, f_del_ADIDA_weekly, mc.cores = 8)

saveRDS(del_ADIDA_weekly_ndusl, "../data/master/paper2_forecast/del_ADIDA_weekly.rds")

saveRDS(del_ADIDA_weekly_ndusl, "../data/master/paper2_forecast/del_ADIDA_weekly_0ss.rds")


f_del_ADIDA_monthly <- partial(get_client_ndusl_forecast,
                               ffun = partial(ADIDA,
                                              binsize = 30 + 1,
                                              ffun = partial(croston, f.type = "SBA.opt")),
                               dfun = del_real_forecast_train_data)
del_ADIDA_monthly_ndusl <- mclapply(data_2, f_del_ADIDA_monthly, mc.cores = 8)

saveRDS(del_ADIDA_monthly_ndusl, "../data/master/paper2_forecast/del_ADIDA_monthly.rds")

saveRDS(del_ADIDA_monthly_ndusl, "../data/master/paper2_forecast/del_ADIDA_monthly_0ss.rds")


f_del_ASACT_weekly <- partial(get_client_ndusl_forecast,
                              ffun = partial(ASACT,
                                             agg.time = 7,
                                             ffun = ets,
                                             h = 7),
                              dfun = del_real_forecast_train_data)
del_smooth_crost_opt_models <- lapply(del_croston_opt_ndusl, function(x) x["model",])
del_ASACT_weekly_ndusl <- mcmapply(data_2,
                                   del_smooth_crost_opt_models,
                                   FUN = f_del_ASACT_weekly,
                                   mc.cores = 8)

saveRDS(del_ASACT_weekly_ndusl, "../data/master/paper2_forecast/del_ASACT_weekly.rds")

saveRDS(del_ASACT_weekly_ndusl, "../data/master/paper2_forecast/del_ASACT_weekly_0ss.rds")

f_del_ASACT_monthly <- partial(get_client_ndusl_forecast,
                               ffun = partial(ASACT,
                                              agg.time = 30,
                                              ffun = ets,
                                              h = 7),
                               dfun = del_real_forecast_train_data)
del_smooth_crost_opt_models <- lapply(del_croston_opt_ndusl, function(x) x["model",])
del_ASACT_monthly_ndusl <- mcmapply(data_2,
                                    del_smooth_crost_opt_models,
                                    FUN = f_del_ASACT_monthly,
                                    mc.cores = 8)

saveRDS(del_ASACT_monthly_ndusl, "../data/master/paper2_forecast/del_ASACT_monthly.rds")

saveRDS(del_ASACT_monthly_ndusl, "../data/master/paper2_forecast/del_ASACT_monthly_0ss.rds")


f_del_MAPA <- partial(get_client_ndusl_forecast,
                      ffun = partial(MAPA,
                                     agg = 7),
                      dfun = del_real_forecast_train_data)
del_MAPA_ndusl <- mclapply(data_2, f_del_MAPA, mc.cores = 8)

saveRDS(del_MAPA_ndusl, "../data/master/paper2_forecast/del_MAPA.rds")

saveRDS(del_MAPA_ndusl, "../data/master/paper2_forecast/del_MAPA_0ss.rds")


## Loading models
## 10% SS level
### Consumption
con_ets_ndusl <- readRDS("../data/master/paper2_forecast/con_ets.rds")
con_ses_ndusl <- readRDS("../data/master/paper2_forecast/con_ses.rds")
con_croston_base_ndusl <- readRDS("../data/master/paper2_forecast/con_crostonbase.rds")
con_croston_opt_ndusl <- readRDS("../data/master/paper2_forecast/con_crostonopt.rds")
con_ADIDA_weekly_ndusl <- readRDS("../data/master/paper2_forecast/con_ADIDA_weekly.rds")
con_ADIDA_monthly_ndusl <- readRDS("../data/master/paper2_forecast/con_ADIDA_monthly.rds")
con_ASACT_weekly_ndusl <- readRDS("../data/master/paper2_forecast/con_ASACT_weekly.rds")
con_ASACT_monthly_ndusl <- readRDS("../data/master/paper2_forecast/con_ASACT_monthly.rds")
con_MAPA_ndusl <- readRDS("../data/master/paper2_forecast/con_MAPA.rds")

### Delivery
del_ets_ndusl <- readRDS("../data/master/paper2_forecast/del_ets.rds")
del_ses_ndusl <- readRDS("../data/master/paper2_forecast/del_ses.rds")
del_croston_base_ndusl <- readRDS("../data/master/paper2_forecast/del_crostonbase.rds")
del_croston_opt_ndusl <- readRDS("../data/master/paper2_forecast/del_crostonopt.rds")
del_ADIDA_weekly_ndusl <- readRDS("../data/master/paper2_forecast/del_ADIDA_weekly.rds")
del_ADIDA_monthly_ndusl <- readRDS("../data/master/paper2_forecast/del_ADIDA_monthly.rds")
del_ASACT_weekly_ndusl <- readRDS("../data/master/paper2_forecast/del_ASACT_weekly.rds")
del_ASACT_monthly_ndusl <- readRDS("../data/master/paper2_forecast/del_ASACT_monthly.rds")
del_MAPA_ndusl <- readRDS("../data/master/paper2_forecast/del_MAPA.rds")

#### 0 SS level
### Consumption
con_ets_ndusl <- readRDS("../data/master/paper2_forecast/con_ets_0ss.rds")
con_ses_ndusl <- readRDS("../data/master/paper2_forecast/con_ses_0ss.rds")
con_croston_base_ndusl <- readRDS("../data/master/paper2_forecast/con_crostonbase_0ss.rds")
con_croston_opt_ndusl <- readRDS("../data/master/paper2_forecast/con_crostonopt_0ss.rds")
con_ADIDA_weekly_ndusl <- readRDS("../data/master/paper2_forecast/con_ADIDA_weekly_0ss.rds")
con_ADIDA_monthly_ndusl <- readRDS("../data/master/paper2_forecast/con_ADIDA_monthly_0ss.rds")
con_ASACT_weekly_ndusl <- readRDS("../data/master/paper2_forecast/con_ASACT_weekly_0ss.rds")
con_ASACT_monthly_ndusl <- readRDS("../data/master/paper2_forecast/con_ASACT_monthly_0ss.rds")
con_MAPA_ndusl <- readRDS("../data/master/paper2_forecast/con_MAPA_0ss.rds")

### Delivery
del_ets_ndusl <- readRDS("../data/master/paper2_forecast/del_ets_0ss.rds")
del_ses_ndusl <- readRDS("../data/master/paper2_forecast/del_ses_0ss.rds")
del_croston_base_ndusl <- readRDS("../data/master/paper2_forecast/del_crostonbase_0ss.rds")
del_croston_opt_ndusl <- readRDS("../data/master/paper2_forecast/del_crostonopt_0ss.rds")
del_ADIDA_weekly_ndusl <- readRDS("../data/master/paper2_forecast/del_ADIDA_weekly_0ss.rds")
del_ADIDA_monthly_ndusl <- readRDS("../data/master/paper2_forecast/del_ADIDA_monthly_0ss.rds")
del_ASACT_weekly_ndusl <- readRDS("../data/master/paper2_forecast/del_ASACT_weekly_0ss.rds")
del_ASACT_monthly_ndusl <- readRDS("../data/master/paper2_forecast/del_ASACT_monthly_0ss.rds")
del_MAPA_ndusl <- readRDS("../data/master/paper2_forecast/del_MAPA_0ss.rds")

## Robustness
## What to do with the NA values in the forecasting method
## Removing them changes the bias
## If we produce a mean DNDUSL and remove the NA value is that biased
## Or methods with a lot of NA's will also have a lot of bad ones
ndusl_robustness <- function(ndusl_data) {
    return(sum(sapply(ndusl_data, function(x) sum(is.na(x["ndusl",])))))
}

ndusl_robustness(con_ets_ndusl)
ndusl_robustness(con_ses_ndusl)
ndusl_robustness(con_croston_base_ndusl)
ndusl_robustness(con_croston_opt_ndusl)
ndusl_robustness(con_ADIDA_weekly_ndusl)
ndusl_robustness(con_ADIDA_monthly_ndusl)
ndusl_robustness(con_ASACT_weekly_ndusl)
ndusl_robustness(con_ASACT_monthly_ndusl)
ndusl_robustness(con_MAPA_ndusl)

ndusl_robustness(del_ets_ndusl)
ndusl_robustness(del_ses_ndusl)
ndusl_robustness(del_croston_base_ndusl)
ndusl_robustness(del_croston_opt_ndusl)
ndusl_robustness(del_ADIDA_weekly_ndusl)
ndusl_robustness(del_ADIDA_monthly_ndusl)
ndusl_robustness(del_ASACT_weekly_ndusl)
ndusl_robustness(del_ASACT_monthly_ndusl)
ndusl_robustness(del_MAPA_ndusl)


dfun <- con_real_forecast_train_data
ndusl_mat <- con_ets_ndusl[[1]]

na_series <- function(ndusl_mat, client, dfun) {
    del_list <- client$test$del_list
    ncols <- ncol(ndusl_mat)

    train_list <- list()

    for(i in seq_len(ncols)) {
        ndusl <- ndusl_mat[["ndusl", i]]
        if (is.na(ndusl)) {
            train <- dfun(client, del_list[[i]])
            train_list <- c(train_list, list(train))
        }
    }
    return(train_list)
}

foo <- mapply(na_series, con_ets_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data))
bar <- unlist(foo, recursive = FALSE)
intermittent_categorisation(bar, type = "PKa", outplot = "none")


na_models <- function(ndusl_mat) {
    ncols <- ncol(ndusl_mat)

    model_list <- list()

    for(i in seq_len(ncols)) {
        ndusl <- ndusl_mat[["ndusl", i]]
        if (is.na(ndusl)) {
            model <- ndusl_mat[["model", i ]]
            modelcomponents <- paste(model$components[1], model$components[2],
                                     model$components[3], sep = "")
            model_list <- c(model_list, list(modelcomponents))
        }
    }
    return(model_list)
}

foo <- lapply(con_ets_ndusl, na_models)
bar <- unlist(foo)
table(bar)

ets_models <- function(ndusl_mat) {
    ncols <- ncol(ndusl_mat)

    model_list <- list()

    for(i in seq_len(ncols)) {
        ndusl <- ndusl_mat[["ndusl", i]]

        model <- ndusl_mat[["model", i ]]
        modelcomponents <- paste(model$components[1], model$components[2],
                                     model$components[3], sep = "")
        model_list <- c(model_list, list(modelcomponents))

    }
    return(model_list)
}

foo <- lapply(con_ets_ndusl, ets_models)
bar <- unlist(foo)
table(bar)

## TODO Can we do a p-value test on this result

## Remove the nas when calulcating the ndusl metric from all the forecasts
ndusl_models_list <- list(con_ets_ndusl,
                          con_ses_ndusl,
                          con_croston_base_ndusl,
                          con_croston_opt_ndusl,
                          con_ADIDA_weekly_ndusl,
                          con_ADIDA_monthly_ndusl,
                          con_ASACT_weekly_ndusl,
                          con_ASACT_monthly_ndusl,
                          con_MAPA_ndusl,
                          del_ets_ndusl,
                          del_ses_ndusl,
                          del_croston_base_ndusl,
                          del_croston_opt_ndusl,
                          del_ADIDA_weekly_ndusl,
                          del_ADIDA_monthly_ndusl,
                          del_ASACT_weekly_ndusl,
                          del_ASACT_monthly_ndusl,
                          del_MAPA_ndusl)

true_list <- lapply(con_ets_ndusl, function(x) !vector(length = ncol(x)))

compare_model_true_list <- function(true_list, model) {
    model_true_list <- lapply(model, function(x) !is.na(x["ndusl",]))
    combined_list <- mapply(true_list, model_true_list, FUN = `&`, SIMPLIFY = FALSE)
    return(combined_list)
}

new_true_list <- Reduce(compare_model_true_list, x = ndusl_models_list, init = true_list)
saveRDS(new_true_list, "../data/master/paper2_forecast/master_true_list.rds")
saveRDS(new_true_list, "../data/master/paper2_forecast/master_true_list_0ss.rds")
new_true_list <- readRDS("../data/master/paper2_forecast/master_true_list.rds")

## Error
real_ndusl <- lapply(data_2, function(x) x$ndusl$real)


## Do the mean value of the true NTUSS for the non converging forecasts using the new_true_list
foo <- mapply(real_ndusl, new_true_list, FUN = function(x, y) x[!y])
length(unlist(foo))
mean(unlist(foo))


asymmetric_scoring <- function(x) {
    if(x < 0) {
        return(abs(0.5*x))
    } else {
        return(x)
    }
}


ndusl_score <- function(ndusl_mat, real_ndusl, score_type, true_list) {
    score_type <- match.arg(score_type, c("dndusl", "abs", "squared", "asymmetric"))
    f_ndusl <- unlist(ndusl_mat["ndusl",])[true_list]
    real_ndusl <- real_ndusl[true_list]
    dndusl <- f_ndusl - real_ndusl

    if(length(dndusl) == 0) {
        return(NULL)
    }

    res <- switch(score_type, dndusl = {
        dndusl
    }, abs = {
        abs(dndusl)
    }, squared = {
        dndusl**2
    }, asymmetric = {
        sapply(dndusl, asymmetric_scoring)
    })

    return(res)
}


## Example NTUSS, DNTUSS, NTUSS score for a customer
table(unlist(con_ets_ndusl[[1]]["ndusl",]) - real_ndusl[[1]])
ndusl_score(con_ets_ndusl[[2]], real_ndusl[[2]], "dndusl", new_true_list[[2]])


## Errors over all our forecasts
ndusl_fcast_error <- function(ndusl_mat, client, dfun) {
    #' Calculate the standard out-of-sample errors for the ndusl forecast
    del_list <- client$test$del_list
    ncols <- ncol(ndusl_mat)
    error <- matrix(nrow = 8, ncol = ncols)
    rownames(error) <- c("sME", "sMAE", "sMSE", "MASE", "MAAPE", "mMAPE", "sPIS", "sAPIS")

    for(i in seq_len(ncols)) {
        train <- dfun(client, del_list[[i]])
        scale <- mean(train)
        model <- ndusl_mat[["model", i]]
        ndusl <- ndusl_mat[["ndusl", i]]
        if (is.na(ndusl)) {
            error[,i] <- rep(NA, times = 8)
            next
        }

        fcast <- result_forecast(forecast(model, x = train, h = ndusl))
        real <- as.numeric(tail(dfun(client, del_list[[i]], ndusl), n = ndusl))

        error[,i] <- c(my_sME(real, fcast, scale),
                       my_sMAE(real, fcast, scale),
                       my_sMSE(real, fcast, scale),
                       my_MASE(fcast, train, real),
                       my_MAAPE(real, fcast),
                       my_mMAPE(real, fcast),
                       my_sPIS(real, fcast, scale),
                       my_sAPIS(real, fcast, scale))
    }

    return(error)
}


### Consumption
con_ets_ndusl_errors <- mcmapply(ndusl_fcast_error, con_ets_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_ets_ndusl_errors, "../data/master/paper2_forecast/con_ets_error.rds")
saveRDS(con_ets_ndusl_errors, "../data/master/paper2_forecast/con_ets_error_0ss.rds")

con_ses_ndusl_errors <- mcmapply(ndusl_fcast_error, con_ses_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_ses_ndusl_errors, "../data/master/paper2_forecast/con_ses_error.rds")
saveRDS(con_ses_ndusl_errors, "../data/master/paper2_forecast/con_ses_error_0ss.rds")

con_croston_base_ndusl_errors <- mcmapply(ndusl_fcast_error, con_croston_base_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_croston_base_ndusl_errors, "../data/master/paper2_forecast/con_croston_base_error.rds")
saveRDS(con_croston_base_ndusl_errors, "../data/master/paper2_forecast/con_croston_base_error_0ss.rds")

con_croston_opt_ndusl_errors <- mcmapply(ndusl_fcast_error, con_croston_opt_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_croston_opt_ndusl_errors, "../data/master/paper2_forecast/con_croston_opt_error.rds")
saveRDS(con_croston_opt_ndusl_errors, "../data/master/paper2_forecast/con_croston_opt_error_0ss.rds")

con_ADIDA_weekly_ndusl_errors <- mcmapply(ndusl_fcast_error, con_ADIDA_weekly_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_ADIDA_weekly_ndusl_errors, "../data/master/paper2_forecast/con_ADIDA_weekly_error.rds")
saveRDS(con_ADIDA_weekly_ndusl_errors, "../data/master/paper2_forecast/con_ADIDA_weekly_error_0ss.rds")

con_ADIDA_monthly_ndusl_errors <- mcmapply(ndusl_fcast_error, con_ADIDA_monthly_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_ADIDA_monthly_ndusl_errors, "../data/master/paper2_forecast/con_ADIDA_monthly_error.rds")
saveRDS(con_ADIDA_monthly_ndusl_errors, "../data/master/paper2_forecast/con_ADIDA_monthly_error_0ss.rds")

con_ASACT_weekly_ndusl_errors <- mcmapply(ndusl_fcast_error, con_ASACT_weekly_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_ASACT_weekly_ndusl_errors, "../data/master/paper2_forecast/con_ASACT_weekly_error.rds")
saveRDS(con_ASACT_weekly_ndusl_errors, "../data/master/paper2_forecast/con_ASACT_weekly_error_0ss.rds")

con_ASACT_monthly_ndusl_errors <- mcmapply(ndusl_fcast_error, con_ASACT_monthly_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data), mc.cores = 8)
saveRDS(con_ASACT_monthly_ndusl_errors, "../data/master/paper2_forecast/con_ASACT_monthly_error.rds")
saveRDS(con_ASACT_monthly_ndusl_errors, "../data/master/paper2_forecast/con_ASACT_monthly_error_0ss.rds")

con_MAPA_ndusl_errors <- mapply(ndusl_fcast_error, con_MAPA_ndusl, data_2, MoreArgs = list(dfun = con_real_forecast_train_data))
saveRDS(con_MAPA_ndusl_errors, "../data/master/paper2_forecast/con_MAPA_error.rds")
saveRDS(con_MAPA_ndusl_errors, "../data/master/paper2_forecast/con_MAPA_error_0ss.rds")


### Deliveries
del_ets_ndusl_errors <- mcmapply(ndusl_fcast_error, del_ets_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_ets_ndusl_errors, "../data/master/paper2_forecast/del_ets_error.rds")
saveRDS(del_ets_ndusl_errors, "../data/master/paper2_forecast/del_ets_error_0ss.rds")

del_ses_ndusl_errors <- mcmapply(ndusl_fcast_error, del_ses_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_ses_ndusl_errors, "../data/master/paper2_forecast/del_ses_error.rds")
saveRDS(del_ses_ndusl_errors, "../data/master/paper2_forecast/del_ses_error_0ss.rds")

del_croston_base_ndusl_errors <- mcmapply(ndusl_fcast_error, del_croston_base_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_croston_base_ndusl_errors, "../data/master/paper2_forecast/del_croston_base_error.rds")
saveRDS(del_croston_base_ndusl_errors, "../data/master/paper2_forecast/del_croston_base_error_0ss.rds")

del_croston_opt_ndusl_errors <- mcmapply(ndusl_fcast_error, del_croston_opt_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_croston_opt_ndusl_errors, "../data/master/paper2_forecast/del_croston_opt_error.rds")
saveRDS(del_croston_opt_ndusl_errors, "../data/master/paper2_forecast/del_croston_opt_error_0ss.rds")

del_ADIDA_weekly_ndusl_errors <- mcmapply(ndusl_fcast_error, del_ADIDA_weekly_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_ADIDA_weekly_ndusl_errors, "../data/master/paper2_forecast/del_ADIDA_weekly_error.rds")
saveRDS(del_ADIDA_weekly_ndusl_errors, "../data/master/paper2_forecast/del_ADIDA_weekly_error_0ss.rds")

del_ADIDA_monthly_ndusl_errors <- mcmapply(ndusl_fcast_error, del_ADIDA_monthly_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_ADIDA_monthly_ndusl_errors, "../data/master/paper2_forecast/del_ADIDA_monthly_error.rds")
saveRDS(del_ADIDA_monthly_ndusl_errors, "../data/master/paper2_forecast/del_ADIDA_monthly_error_0ss.rds")

del_ASACT_weekly_ndusl_errors <- mcmapply(ndusl_fcast_error, del_ASACT_weekly_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_ASACT_weekly_ndusl_errors, "../data/master/paper2_forecast/del_ASACT_weekly_error.rds")
saveRDS(del_ASACT_weekly_ndusl_errors, "../data/master/paper2_forecast/del_ASACT_weekly_error_0ss.rds")

del_ASACT_monthly_ndusl_errors <- mcmapply(ndusl_fcast_error, del_ASACT_monthly_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data), mc.cores = 8)
saveRDS(del_ASACT_monthly_ndusl_errors, "../data/master/paper2_forecast/del_ASACT_monthly_error.rds")
saveRDS(del_ASACT_monthly_ndusl_errors, "../data/master/paper2_forecast/del_ASACT_monthly_error_0ss.rds")

del_MAPA_ndusl_errors <- mapply(ndusl_fcast_error, del_MAPA_ndusl, data_2, MoreArgs = list(dfun = del_real_forecast_train_data))
saveRDS(del_MAPA_ndusl_errors, "../data/master/paper2_forecast/del_MAPA_error.rds")
saveRDS(del_MAPA_ndusl_errors, "../data/master/paper2_forecast/del_MAPA_error_0ss.rds")


## 10 SS level
con_ets_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ets_error.rds")
con_ses_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ses_error.rds")
con_croston_base_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_croston_base_error.rds")
con_croston_opt_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_croston_opt_error.rds")
con_ADIDA_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ADIDA_weekly_error.rds")
con_ADIDA_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ADIDA_monthly_error.rds")
con_ASACT_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ASACT_weekly_error.rds")
con_ASACT_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ASACT_monthly_error.rds")
con_MAPA_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_MAPA_error.rds")

del_ets_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ets_error.rds")
del_ses_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ses_error.rds")
del_croston_base_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_croston_base_error.rds")
del_croston_opt_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_croston_opt_error.rds")
del_ADIDA_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ADIDA_weekly_error.rds")
del_ADIDA_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ADIDA_monthly_error.rds")
del_ASACT_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ASACT_weekly_error.rds")
del_ASACT_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ASACT_monthly_error.rds")
del_MAPA_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_MAPA_error.rds")

### 0ss
con_ets_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ets_error_0ss.rds")
con_ses_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ses_error_0ss.rds")
con_croston_base_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_croston_base_error_0ss.rds")
con_croston_opt_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_croston_opt_error_0ss.rds")
con_ADIDA_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ADIDA_weekly_error_0ss.rds")
con_ADIDA_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ADIDA_monthly_error_0ss.rds")
con_ASACT_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ASACT_weekly_error_0ss.rds")
con_ASACT_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_ASACT_monthly_error_0ss.rds")
con_MAPA_ndusl_errors <- readRDS("../data/master/paper2_forecast/con_MAPA_error_0ss.rds")

del_ets_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ets_error_0ss.rds")
del_ses_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ses_error_0ss.rds")
del_croston_base_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_croston_base_error_0ss.rds")
del_croston_opt_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_croston_opt_error_0ss.rds")
del_ADIDA_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ADIDA_weekly_error_0ss.rds")
del_ADIDA_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ADIDA_monthly_error_0ss.rds")
del_ASACT_weekly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ASACT_weekly_error_0ss.rds")
del_ASACT_monthly_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_ASACT_monthly_error_0ss.rds")
del_MAPA_ndusl_errors <- readRDS("../data/master/paper2_forecast/del_MAPA_error_0ss.rds")


ndusl_models_errors_list <- list(con_ets_ndusl_errors,
                                 con_ses_ndusl_errors,
                                 con_croston_base_ndusl_errors,
                                 con_croston_opt_ndusl_errors,
                                 con_ADIDA_weekly_ndusl_errors,
                                 con_ADIDA_monthly_ndusl_errors,
                                 con_ASACT_weekly_ndusl_errors,
                                 con_ASACT_monthly_ndusl_errors,
                                 con_MAPA_ndusl_errors,
                                 del_ets_ndusl_errors,
                                 del_ses_ndusl_errors,
                                 del_croston_base_ndusl_errors,
                                 del_croston_opt_ndusl_errors,
                                 del_ADIDA_weekly_ndusl_errors,
                                 del_ADIDA_monthly_ndusl_errors,
                                 del_ASACT_weekly_ndusl_errors,
                                 del_ASACT_monthly_ndusl_errors,
                                 del_MAPA_ndusl_errors)


## Error Table showing the average score and fcast errors for a model
ndusl_fcast_error_measurments <- function(ndusl_model, ndusl_model_errors, real_ndusl, true_list) {
    f1 <- function(error) mean(unlist(mapply(FUN = ndusl_score, ndusl_model, real_ndusl, true_list, MoreArgs = list(score_type = error))))

    dndusl <- f1("dndusl")
    ndusl_abs_score <- f1("abs")
    ndusl_squared_score <- f1("square")
    ndusl_asymmetric_score <- f1("asymmetric")

    f2 <- function(error) mean(unlist(lapply(ndusl_model_errors, function(x) x[error,]))[unlist(true_list)])

    sme <- f2("sME")
    smae <- f2("sMAE")
    smse <- f2("sMSE")
    mase <- f2("MASE")
    maape <- f2("MAAPE")
    spis <- f2("sPIS")
    sapis <- f2("sAPIS")


    data.frame(dndusl = dndusl,
               ndusl.abs.score = ndusl_abs_score,
               ndusl.squared.score = ndusl_squared_score,
               ndusl.asymmetric.score = ndusl_asymmetric_score,
               sme = sme,
               smae = smae,
               smse = smse,
               mase = mase,
               maape = maape,
               spis = spis,
               sapis = sapis)
}


foo <- t(mapply(FUN = ndusl_fcast_error_measurments, ndusl_models_list, ndusl_models_errors_list, MoreArgs = list(real_ndusl = real_ndusl, true_list = new_true_list)))

## Be very careful if you change the order of the forecast models in other objects
## There's no way to make this automatic if you are using an mapply to match the call
rownames(foo) <- c("con ETS", "con SES", "con SBA", "con SBA opt",
                   "con ADIDA weekly", "con ADIDA monthly", "con ASACT weekly",
                   "con ASACT monthly", "con MAPA",
                   "del ETS", "del SES", "del SBA", "del SBA opt",
                   "del ADIDA weekly", "del ADIDA monthly", "del ASACT weekly",
                   "del ASACT monthly", "del MAPA")

foo

## Ranked correlation between the error measurment and NDUSL
bar <- as.data.frame(foo)

cor.test(abs(as.numeric(bar$sme)), abs(as.numeric(bar$dndusl)), method = "spearman")
cor.test(as.numeric(bar$smae), abs(as.numeric(bar$dndusl)), method = "spearman")
cor.test(as.numeric(bar$smse), abs(as.numeric(bar$dndusl)), method = "spearman")
cor.test(as.numeric(bar$mase), abs(as.numeric(bar$dndusl)), method = "spearman")
cor.test(as.numeric(bar$maape), abs(as.numeric(bar$dndusl)), method = "spearman")
cor.test(as.numeric(bar$spis), abs(as.numeric(bar$dndusl)), method = "spearman")
cor.test(as.numeric(bar$sapis), abs(as.numeric(bar$dndusl)), method = "spearman")

cor.test(abs(as.numeric(bar$sme)), as.numeric(bar$ndusl.abs.score), method = "spearman")
cor.test(as.numeric(bar$smae), as.numeric(bar$ndusl.abs.score), method = "spearman")
cor.test(as.numeric(bar$smse), as.numeric(bar$ndusl.abs.score), method = "spearman")
cor.test(as.numeric(bar$mase), as.numeric(bar$ndusl.abs.score), method = "spearman")
cor.test(as.numeric(bar$maape), as.numeric(bar$ndusl.abs.score), method = "spearman")
cor.test(as.numeric(bar$spis), as.numeric(bar$ndusl.abs.score), method = "spearman")
cor.test(as.numeric(bar$sapis), as.numeric(bar$ndusl.abs.score), method = "spearman")

cor.test(abs(as.numeric(bar$sme)), as.numeric(bar$ndusl.squared.score), method = "spearman")
cor.test(as.numeric(bar$smae), as.numeric(bar$ndusl.squared.score), method = "spearman")
cor.test(as.numeric(bar$smse), as.numeric(bar$ndusl.squared.score), method = "spearman")
cor.test(as.numeric(bar$mase), as.numeric(bar$ndusl.squared.score), method = "spearman")
cor.test(as.numeric(bar$maape), as.numeric(bar$ndusl.squared.score), method = "spearman")
cor.test(as.numeric(bar$spis), as.numeric(bar$ndusl.squared.score), method = "spearman")
cor.test(as.numeric(bar$sapis), as.numeric(bar$ndusl.squared.score), method = "spearman")

cor.test(abs(as.numeric(bar$sme)), as.numeric(bar$ndusl.asymmetric.score), method = "spearman")
cor.test(as.numeric(bar$smae), as.numeric(bar$ndusl.asymmetric.score), method = "spearman")
cor.test(as.numeric(bar$smse), as.numeric(bar$ndusl.asymmetric.score), method = "spearman")
cor.test(as.numeric(bar$mase), as.numeric(bar$ndusl.asymmetric.score), method = "spearman")
cor.test(as.numeric(bar$maape), as.numeric(bar$ndusl.asymmetric.score), method = "spearman")
cor.test(as.numeric(bar$spis), as.numeric(bar$ndusl.asymmetric.score), method = "spearman")
cor.test(as.numeric(bar$sapis), as.numeric(bar$ndusl.asymmetric.score), method = "spearman")


if (FALSE) {
    data_2 <- readRDS("../data/master/data_paper2.rds")
    client <- add_test_del_list(client)
    data_2 <- update_state(data_2, add_test_del_list, "apply")
    client$test$del_list == data_2[[1]]$test$del_list
    client <- data_2[[1]]

    client <- add_start_tel(client)
    data_2 <- update_state(data_2, add_start_tel, "parallel")
    client$ndusl$start_tel == data_2[[1]]$ndusl$start_tel
    client <- data_2[[1]]

    client <- add_safety_level(client)
    data_2 <- update_state(data_2, add_safety_level, "apply")
    client$ndusl$safety_level == data_2[[1]]$ndusl$safety_level
    client <- data_2[[1]]

    client <- add_ndusl_real(client)
    data_2 <- update_state(data_2, add_ndusl_real, "parallel")
    client$ndusl$real == data_2[[1]]$ndusl$real

    client <- data_2[[1]]
    del <- client$test$del_list[[1]]
    start_tel <- client$ndusl$start_tel[[1]]
    guess <- client$ndusl$guess_list[[1]]
    safety_level <- client$ndusl$safety_level

    real_ndusl <- client$ndusl$real[[1]]
    real_ndusl
    start_tel - safety_level


    data <- con_real_forecast_train_data(client, del)
    data <- del_real_forecast_train_data(client, del)

    sum(tail(con_real_forecast_train_data(client, del, 12), 12))

    sum(result_forecast(forecast(ses(con_real_forecast_train_data(client, del)), 12)))
    sum(result_forecast(forecast(ets(con_real_forecast_train_data(client, del)), 15)))
    sum(result_forecast(forecast(croston(del_real_forecast_train_data(client, del), "SBA.base"), 30)))
    sum(result_forecast(forecast(ADIDA(con_real_forecast_train_data(client, del),
                                       30+1,
                                       partial(croston, f.type = "SBA.opt")), 12)))

    sum(result_forecast(forecast(ets(del_real_forecast_train_data(client, del)), 34)))
    sum(result_forecast(forecast(croston(del_real_forecast_train_data(client, del), "SBA.opt"), 30)))
    sum(result_forecast(forecast(ADIDA(del_real_forecast_train_data(client, del),
                                       30+1,
                                       partial(croston, f.type = "SBA.opt")), 13)))

    sum(result_forecast(forecast(ASACT(del_real_forecast_train_data(client, del),
                                       7,
                                       ets), 12)))

    sum(result_forecast(forecast(MAPA(data, 7), 12)))

}
