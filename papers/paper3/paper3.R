#' title: paper3.R
#' comments: Code for the third paper
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' Work directory
setwd("/home/corey/AL/code")
packrat::init()

#' Packages
library(ggplot2)

#' Sourcing
source("data.R")
source("con_del_models.R")
source("state_event.R")
source("ndusl.R")
source("forecast.R")
source("intermittent.R")

#' Load data
data_3 <- readRDS("../data/master/data_paper3.rds")

## Create the clus_tel_day data object for paper 3
## Filter time series with a minimun of 3 years
cvd <- filter_cvd(readRDS("../data/master/client.rds"))

con_del_data_3 <- function(cvd, source, time_scale) {
    ## Get the data
    print("Fetching delivery series")
    del_orig <- lapply(cvd, client_del_xts, source = source, time_scale = time_scale)

    print("Fetching consumption series")
    con_orig <- lapply(cvd, client_con_xts, time_scale = time_scale)

    ## Filter out customers that do not have a full year for 2016 and the second half of 2015
    print("Filtering data")
    filter_f_1 <- function(x) filter_full_year(x, "days", "2016")
    del_filt_1 <- lapply(del_orig, filter_f_1)
    con_filt_1 <- lapply(con_orig, filter_f_1)

    filter_f_2 <- function(x) filter_full_quarter(x, 3, "2015")
    del_filt_2 <- lapply(del_orig, filter_f_2)
    con_filt_2 <- lapply(con_orig, filter_f_2)

    filter_f_3 <- function(x) filter_full_quarter(x, 4, "2015")
    del_filt_3 <- lapply(del_orig, filter_f_3)
    con_filt_3 <- lapply(con_orig, filter_f_3)

    ## Removing the null from the outputs
    f <- not %c% is.null
    ind <- sapply(del_filt_1, f) & sapply(con_filt_1, f) & sapply(del_filt_2, f) & sapply(con_filt_2, f) & sapply(del_filt_3, f) & sapply(con_filt_3, f)
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]

    ## Filter out customers that do not have a minimum of 2 observations in the second half
    ## 2015. These customers cannot be used for forecasting using Croston's method.
    ## Run this filter after the first one since we have to make sure that the second half
    ## of 2015 is available
    filter_f_4 <- function(x) sum(x[xts_range("2015-07-01", "2015-12-31")] != 0) >= 2
    ind <- sapply(del_orig, filter_f_4)
    cvd <- cvd[ind]
    del_orig <- del_orig[ind]
    con_orig <- con_orig[ind]

    ## Keep data from 2nd half of 2015 to 2016 inclusively of everyone who has a full year in 2016
    f <- function(x) x[xts_range("2015-07-01", "2016-12-31")]
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

data_3 <- con_del_data_3(cvd, "tel", "days")
saveRDS(data_3, "../data/master/data_paper3.rds")


## Adding data about each clients deliveries and the NDUSL following paper 2
add_test_del_list <- function(client) {
    #' Add the client's delivery list
    #' The whole year of 2016
    del_real <- client$del$filt
    test <- tail(del_real, 366)  # leap year
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
                                peak_tel_at_del(del, get_client_telemetry(client$cvd))
                            },
                            error = function(cond) {
                                ## we can get out of bonds of the telemetry list
                                ## when finding the peak. This catches it and
                                ## return NA
                                return(NA)
                            })
                        })

    client$ndusl$start_tel <- start_tel

    return(client)
}


data_3 <- end_state(data_3, list(c(add_test_del_list, "apply"),
                                 c(add_start_tel, "parallel", 8)))

## Check the NAs in the start_tel
sapply(data_3, function(x) any(is.na(x$ndusl$start_tel)))

saveRDS(data_3, "../data/master/data_paper3.rds")


## Determine the actual max_level that can be sent to a client
## Maximum between the highest measured level at a customer's site and the
## max level of the tank
## Filtering based on inconsistent max tank levels
## Errors between the max tank level and the read telemetry amount

relative_difference <- function(x, ref) {
    (x - ref)/ref
}

sapply(data_3, function(x) relative_difference(max(x$ndusl$start_tel), get_client_max_level(x$cvd)))
data_3 <- Filter(function(x) relative_difference(max(x$ndusl$start_tel), get_client_max_level(x$cvd)) < 0.2, data_3)


add_max_tel_level <- function(client) {
    client$ndusl$max_tel <- max(max(client$ndusl$start_tel), get_client_max_level(client$cvd))

    return(client)
}

data_3 <- lapply(data_3, add_max_tel_level)

saveRDS(data_3, "../data/master/data_paper3.rds")

###############################
## Descriptive stats
###############################
## Number of clients
length(data_3)

## Tukey
## Croston decomposition
## Run these after in case of NA's produced by the time shift
tukey_decomp_table(pluck_list(data_3, "del", "filt"))
tukey_decomp_table(pluck_list(data_3, "con", "filt"))

## Time series categorisations
intermittent_categorisation(
    pluck_list(data_3, "del", "filt"),
    type = "PKa",
    outplot = "none"
)

intermittent_categorisation(
    pluck_list(data_3, "con", "filt"),
    type = "PKa",
    outplot = "none"
)


#############################
## Simultation functions
#############################
perfect_ndusl_simulation <- function(client, safety_level = 0) {
    #' Simulate the NDUSL for a customer where we delivery the max amount in the
    #' tank the day before we would be under the safety level.

    #' Algorithm function
    ##' After the delivery at t, we calculate the NDUSL.
    ##' The day right before the NDUSL is our new delivery date.
    ##' The amount delivered on that date is the consummed amount including the
    ##' delivery day.
    ##' The telemetry of the day after the delivery is the max amount allowed in stock.
    ##' We then calculate the NDUSL following the new delivery at the new telemetry.

    ## We start the telemetry first day of 2016-01-01
    ## Then calculate the deliveries when they happen at the day before NDUSL
    ## Calculate the time for the next delivery and the delivered amount
    ## We deliver the day right before we reach the ndusl.
    ## We always deliver up to the max stock.
    ## The amount we deliver includes the  amount that would have been consummed
    ## the day of the delivery.

    max_level <- client$ndusl$max_tel
    safety_level <- safety_level * max_level

    ## First iteration
    con <- client$con$filt
    del <- as.Date("2015-12-31")
    start_tel <- last_tel_reading_of_day(del, client$cvd)
    res <- day_before_safety_level(con, del, start_tel, safety_level)

    next_del <- res$date
    del_amount <- res$amount
    ## Fix the first delivery amount by adding up to the max
    del_amount <- del_amount + max_level - start_tel

    ## init result list
    ## The first ndusl is biased since it was not done at max
    ## but the first del_amount is not biased
    dels <- c(next_del)
    dels_amount <- c(del_amount)

    ## Set the starting telementry to be the max level
    start_tel <- max_level
    i <- 2

    while (TRUE) {
        start <- dels[[i-1]]
        res <- day_before_safety_level(con, start, start_tel, safety_level)

        if(is.na(res)) {
            break
        }

        dels[[i]] <- res$date
        dels_amount[[i]] <- res$amount
        i <- i+1
    }

    ## Remove the first ndusl since it is biased
    ## dels_date <- tail(dels_date, -1)
    ## dels_amount <- tail(dels_amount, -1)
    ## real_dates <- tail(client$test$del_list, -1)

    ## Determine the real delivery amounts
    del_real <- client$del$orig
    real_dates <- client$test$del_list
    real_amounts <- as.numeric(del_real[index(del_real) %in% real_dates])


    structure(
        list(
            dates = dels,
            amounts = dels_amount,
            real_dates = real_dates,
            real_amounts = real_amounts
        ),
        class = "NDUSLsimulation"
    )
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
        error <- day_before_safety_level(res, start_tel, safety_level)
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


con_real_forecast_train_data <- function(client, del, extra = 0) {
    #' For a given delivery date what is the forecast data
    ## Forecasts on the real consumption data.
    ## This is the best case scenario using all available historical telemetry
    con_real <- client$con$filt
    train <- con_real[xts_range(start(con_real), del + extra)]
    return(train)
}


del_real_forecast_train_data <- function(client, del, extra = 0) {
    #' For a given delivery date what is the forecast data
    ## Forecasts on the historical delivery data only
    ## This uses our modls for transforming intermittent delivery data into
    ## consumption data.
    del_real <- client$del$filt
    train <- del_real[xts_range(start(del_real), del + extra)]

    return(train)
}


compare_sim_results <- function(max_level, start_tel, overfilling, start_date, safety_lead_time, real = NA, sim = NA, emer = NA) {
    ## Compare the result of our predicted out-of-stock to the real out-of-stock
    ## The delivery done is for our forecasted amount, unless there is an out-of-stock
    ## in that case we emergency deliver the max amount
    ## The type of delivery
    ## 0L : standard delivery
    ## 1L : emergency delivery
    ## 2L : service interuption delivery
    type <- NA  # defaut
    if (is.na(sim) & is.na(real) & is.na(emer)) {
        stop("Not comparing any results")
    } else if (!is.na(emer)) {
        ## We have to perform an emergency delivery to go back up above the
        ## safety level
        del_date <- emer$date
        del_amount_pred <- emer$pred_stock + max_level - start_tel
        del_amount_real <- emer$real_stock + max_level - start_tel
        type <- 1L
        next_start_tel <- start_tel - emer$real_stock + del_amount_pred
    } else if (is.na(sim) & !is.na(real)) {
        ## We are late, our simulation went out-of-bounds, but the reality is
        ## we needed to deliver earlier
        del_date <- real + 1
        del_amount_pred <- max_level
        del_amount_real <- max_level
        type  <- 2L
        next_start_tel <- max_level
    } else if (!is.na(sim) & is.na(real)) {
        ## Our simulation exists, but the real value is out-of-bonds, we are early
        del_date <- sim$date
        del_amount_pred <- sim$pred_stock_con + max_level - start_tel
        del_amount_real <- sim$real_stock_con + max_level - start_tel
        next_start_tel <- start_tel - sim$real_stock_con + del_amount_pred
    } else if (sim$date > real) {
        ## We are late, that's bad.
        ## The customer calls us to tell us that he's out of stock.
        ## We deliver the same day
        del_date <- real + 1
        del_amount_pred <- max_level
        del_amount_real <- max_level
        type <- 2L
        next_start_tel <- max_level
    } else if (sim$date <= real) {
        ## We are either early, or exactly on time
        ## So we deliver following our forecast
        del_date <- sim$date
        del_amount_pred <- sim$pred_stock_con + max_level - start_tel
        del_amount_real <- sim$real_stock_con + max_level - start_tel
        next_start_tel <- start_tel - sim$real_stock_con + del_amount_pred
    } else {
        stop('Unspecified case when comparing forecasted and real results')
    }

    if (is.na(type)) {
        ## We haven't set the type of delivery, but we know the delivery is on time
        ## but it could be that the delivery has to be done quicker than the
        ## safety_lead_time. In this case we consider the delievery to be an
        ## emergency
        if ((del_date - start_date) < safety_lead_time) {
            type <- 1L
        } else {
            type <- 0L
        }
    }

    if (overfilling) {
        next_start_tel <- max_level
    } else {
        ## Can't store more in the tank than the max capacity
        next_start_tel <- min(max_level, next_start_tel)
    }

    return(
        list(
            date = del_date,
            amount_pred = del_amount_pred,
            amount_real = del_amount_real,
            next_start_tel = next_start_tel,
            type = type
        )
    )
}


consummed_stock <- function(client, start, end) {
    #' Return the consummed stock for a client between start and end inclusively
    return(sum(client$con$orig[xts_range(start, end)]))
 }


lead_time_ndusl_forecast <- function(safety_lead_time, client, dfun, ffun, safety_level, start_date, start_tel) {
    #' NDUSL forecast while improving the forecast with recomputing results
    #' until the lead time is reached. We increment the start_date for our forecast

    del_time <- safety_lead_time + 1  # Dummy init to ensure loop begins
    guess <- 365  # Large guess because it doesn't the calculation speed

    train_data <- dfun(client, start_date)
    init_model <- ffun(train_data)
    init_start_date <- start_date

    while (del_time > safety_lead_time) {
        train_data <- dfun(client, start_date)
        ## Speed up the loop by updating the forcasting model instead of recomputing it
        tryCatch({
            fmodel <- update(init_model, train_data)
        },
        error = {
            ## Possible that the new data causes the model to have an error
            ## As such we recalculate the whole model. This new
            ## model then becomes our init_model for the rest of
            ## the forecasts.
            init_model <<- ffun(train_data)
            fmodel <- init_model
        })

        sim_res <- try_ndusl_forecast(fmodel, guess, start_tel, safety_level)
        del_time <- sim_res$ndusl[[1]]

        if (is.na(del_time)) {
            ## We are simulating past the 2016 date range for our next delivery
            ## so we end the function here
            return(NA)
        }

        start_date <- start_date + 1

        ## Remove the days consummed stock, that's now our new start_tel for the loop
        start_tel <- start_tel - as.numeric(client$con$filt[start_date])
    }
    ## Fix the final amounts and dates
    ## We need to remve 1 to the date since start_date was increased by one before
    ## breaking the loop
    sim_res$date <- sim_res$ndusl[[1]] + start_date - 1

    if (sim_res$date > as.Date("2016-12-31")) {
        ## Our forecasted simulate is out-of-bonds of our simulation
        return(NA)
    }

    if (del_time == 0) {
        ## We forecast the NDUSL. It returns 1. I.E. tomorrow we will be under
        ## the safety level. Now when we want to get the day before the safety
        ## level. That day ends up being today. The day from which we did the
        ## forecast. The del_time is 0. Since we can't deliver today,
        ## we deliver tomorrow (the earliest time possible to not be under
        ## safety stock as much possible). The  amount delivered is our measured
        ## stock used between the init_start to the forecast date + the predicted consummed stock
        ## of the delivery date which is tomorrow.
        sim_res$date <- sim_res$date + 1  # 0 (today) +1 gets us tomorrow
        sim_res$real_stock_con <- consummed_stock(client, init_start_date, sim_res$date)
        sim_res$pred_stock_con <- consummed_stock(client, init_start_date, sim_res$date - 1) + result_forecast(forecast(fmodel, 1))
    } else if (del_time == -1) {
        ## We forecast the NDUSL. It returns 0. I.E. we are already under
        ## the safety level. Now when we want to get the day before the safety
        ## level. That day ends up being yesterday. The day from which we did the
        ## forecast. The del_time is -1. Since we can't deliver today,
        ## we deliver tomorrow (the earliest time possible to not be under
        ## safety stock as much possible). The  amount delivered is our measured
        ## stock used between the init_start to the forecast date + the predicted consummed stock
        ## of the delivery date which is tomorrow.
        sim_res$date <- sim_res$date + 2 # -1 (yesterday) + 2 gets us tomorrow
        sim_res$real_stock_con <- consummed_stock(client, init_start_date, sim_res$date)
        sim_res$pred_stock_con <- consummed_stock(client, init_start_date, sim_res$date - 1) + result_forecast(forecast(fmodel, 1))
    } else if (del_time > 0) {
        ## The stock consumption predicted and real
        ## For the real amount it's the consummed stock from the start of the loop
        ## we add one because this function is inclusive, whereas the loop is exclusive
        ## The loop begins the day we delivered
        ## the consummed stock starts the day after
        ## The predicted amount is the consummed stock up to the day we do our last forecast
        ## plus the forecasted amount
        sim_res$real_stock_con <- consummed_stock(client, init_start_date + 1, sim_res$date)
        sim_res$pred_stock_con <- consummed_stock(client, init_start_date + 1, sim_res$date - del_time) + sim_res$ndusl[2]
    } else {
        stop("NDUSL less than -1. This shouldn't be possible.")
    }
    sim_res$ndusl <- NULL
    return(sim_res)
}


forecast_ndusl_simulation <- function(client, safety_stock, safety_lead_time, ffun, data_type, overfilling) {
    #' Simulate the NDUSL for a customer where we are attemption to forecast the NDUSL
    #' We will fill the tank the day before our forecasted NDUSL

    #' Algorithm function
    ##' After the delivery at t, we forecast a NDUSL.
    ##' The forecasted NDUSL is recomputed.
    ##' The day right before the NDUSL is our new delivery date.
    ##' The amount delivered on that date is the consummed amount including the
    ##' delivery day.
    ##' The telemetry of the day after the delivery is the max amount allowed in stock.
    ##' We then calculate the NDUSL following the new delivery at the new telemetry.
    ##' Measure both the forecasted consumption and what we actually refill for.
    ##' If overfilling then the deliveries are done to the max capacity each time
    ##' if not then use the predicted amounts for the delivered amount
    ##' either values are then used for the starting telemetry of the next delivery.

    ## We start the telemetry on 2015-12-31, but deliveries only start on the 2016-01-01
    ## We deliver the day right before we reach the out-of-stock.
    ## We always deliver up to the max stock.
    ## The amount we deliver includes the  amount that would have been consummed
    ## the day of the delivery.

    data_type <- match.arg(data_type, c("del", "con"))
    dfun <- switch(data_type,
                   con = con_real_forecast_train_data,
                   del = del_real_forecast_train_data)

    max_level <- client$ndusl$max_tel
    safety_level <- safety_stock * max_level

    ## First iteration
    ## Assume that the last delivery was done on the last day of 2015, we now
    ## simulate the following year
    start_date <- as.Date("2015-12-31")
    start_tel <- last_tel_reading_of_day(start_date, client$cvd)

    if (start_tel < safety_level) {
        ## We are already under the safety level, we perform an emergency
        ## delivery to bring us back up above the safety level
        ## We deliver enough to bring us back to max + the days predicted consummed
        ## stock
        del_date <- start_date + 1

        ## Forecast tomorrows consummed stock
        fmodel <- ffun(dfun(client, start_date))
        res <- result_forecast(forecast(fmodel, 1))
        del_amount_pred <- res
        del_amount_real <- consummed_stock(client, del_date, del_date)

        emer_del <- list(
            date = del_date,
            pred_stock = del_amount_pred,
            real_stock = del_amount_real
        )
        final_res <- compare_sim_results(max_level, start_tel, overfilling, start_date, safety_lead_time, emer = emer_del)
    } else {
        ## We are above the safety level, let's do the simulation and compare it to
        ## the truth
        ## Real oos is fixed at 0 safety_level, we want to know when an out-of-stock
        ## occurs, thus 0 safety level.
        oos_real <- start_date + next_day_under_safety_level(client$con$filt, start_date, start_tel, safety_level = 0)

        ## The delivery simulation is run with a very high safety_lead_time so that the loop
        ## only runs once
        oos_sim <- switch(data_type,
                          con = lead_time_ndusl_forecast(safety_lead_time, client, dfun, ffun, safety_level, start_date, start_tel),
                          del = lead_time_ndusl_forecast(1000000, client, dfun, ffun, safety_level, start_date, start_tel))

        final_res <- compare_sim_results(max_level, start_tel, overfilling, start_date, safety_lead_time, real = oos_real, sim = oos_sim)
    }

    ## init result list
    dels_date <- c(final_res$date)
    dels_amount_pred <- c(final_res$amount_pred)
    dels_amount_real <- c(final_res$amount_real)
    types <- c(final_res$type)

    #' The main forward loop
    i <- 2

    while (TRUE) {
        start_date <- dels_date[[i-1]]
        start_tel <- final_res$next_start_tel

        if (start_tel < safety_level) {
            ## We are already under the safety level, we perform an emergency
            ## delivery to bring us back up above the safety level
            ## We deliver enough to bring us back to max + the days predicted consummed
            ## stock
            del_date <- start_date + 1

            ## Forecast tomorrows consummed stock
            fmodel <- ffun(dfun(client, start_date))
            res <- result_forecast(forecast(fmodel, 1))
            del_amount_pred <- res
            del_amount_real <- consummed_stock(client, del_date, del_date)

            emer_del <- list(
                date = del_date,
                pred_stock = del_amount_pred,
                real_stock = del_amount_real
            )
            final_res <- compare_sim_results(max_level, start_tel, overfilling, start_date, safety_lead_time, emer = emer_del)
        } else {
            ## We are above the safety level, let's do the simulation and compare it to
            ## the truth
            ## Real oos is fixed at 0 safety_level, we want to know when an out-of-stock
            ## occurs, thus 0 safety level.
            oos_real <- start_date + next_day_under_safety_level(client$con$filt, start_date, start_tel, safety_level = 0)

            ## The delivery simulation is run with a very high safety_lead_time so that the loop
            ## only runs once
            oos_sim <- switch(data_type,
                              con = lead_time_ndusl_forecast(safety_lead_time, client, dfun, ffun, safety_level, start_date, start_tel),
                              del = lead_time_ndusl_forecast(1000000, client, dfun, ffun, safety_level, start_date, start_tel))

            if(is.na(oos_sim) & is.na(oos_real)) {
                ## Both the real results and the simulation are out-of-bounds
                break
            }

            final_res <- compare_sim_results(max_level, start_tel, overfilling, start_date, safety_lead_time, real = oos_real, sim = oos_sim)
        }

        dels_date[[i]] <- final_res$date
        dels_amount_pred[[i]] <- final_res$amount_pred
        dels_amount_real[[i]] <- final_res$amount_real
        types[[i]] <- final_res$type
        i <- i+1
    }

    ## Determine the real delivery amounts
    del_real <- client$del$orig
    true_dates <- client$test$del_list
    true_amounts <- as.numeric(del_real[index(del_real) %in% true_dates])


    structure(
        list(
            dates = dels_date,
            pred_amounts = dels_amount_pred,
            real_amounts = dels_amount_real,
            types = types,
            true_dates = true_dates,
            true_amounts = true_amounts,
            safety_stock = safety_stock,
            safety_lead_time = safety_lead_time,
            data = data_type,
            overfilling = overfilling
        ),
        class = "NDUSLsimulation"
    )
}


service_level <- function(sim) {
    #' Return the service level for a customer simulation
    foo <- sim$types == 2
    return(1 - sum(foo)/length(foo))
}


emergency_level <- function(sim) {
    #' Return the proportion of emergency deliveries for a customer simulation
    foo <- sim$types == 1
    return(sum(foo)/length(foo))
}


service_level_optimisation <- function(client, sl, safety_lead_time, ffun, data_type, overfilling) {
    ## Assuming a certain desired service_level can we optimise the safety stock
    ## required to achieve that service level using our simulated ndusl forecasts

    ## The opimisation procedure runs the simulation and evaluates the results
    ## and then uses bissection to determine the optimal safety_stock

    ## Some initial values to prepare before simulating
    data_type <- match.arg(data_type, c("del", "con"))
    a <- 0  # lower bound
    b <- 0.5  # greedy upper bound
    f1 <- partial(forecast_ndusl_simulation,
                 client = client,
                 safety_lead_time = safety_lead_time,
                 ffun = ffun,
                 data_type = data_type,
                 overfilling = overfilling)
    f2 <- service_level

    ## Lower bound
    sim_a <- f1(a)
    res_a <- f2(sim_a)

    if (res_a >= sl) {
        ## The lower bound works We're done no point in continuing
        return(list(opt_safety_level = a, sim = sim_a, service_level = res_a))
    }

    ## Upper bound
    sim_b <- f1(b)
    res_b <- f2(sim_b)

    if (res_b < sl) {
        ## We tried to be greedy with a small upper bound
        ## Now we have a slower optimization
        a <- 0.5  # reuse the previous b as the lower bound
        sim_a <- sim_b
        res_a <- res_b

        b <- 1  # new upper bound
        sim_b <- f1(b)
        res_b <- f2(sim_b)

        if (res_b < sl) {
            stop("Can't satisfy service level with 100% safety stock.")
        }
    }

    tol <- 0.02
    while (b - a > tol) {
        mid_point <- (a + b)/2
        sim_mid <- f1(mid_point)
        res_mid <- f2(sim_mid)

        if (res_mid >= sl) {
            b <- mid_point
        } else {
            a <- mid_point
        }
    }

    ## When the bissection loop is over we have a couple of situations
    if (res_mid >= sl) {
        ## The smallest value is the one found by the bissection
        return(list(opt_safety_level = mid_point, sim = sim_mid, service_level = res_mid))
    } else {
        ## Our last middle point in the bissection is under the sl
        ## However, we know that the b upper bound was the last value >= sl
        ## Unfortunateyl, we have to recompute the simulation
        ## We could cache all simulation values but that would get a little involved
        sim <- f1(b)
        res <- f2(sim)
        return(list(opt_safety_level = b, sim = sim, service_level = res))
    }
}


###################################
## Running the simulations
###################################
## Chosen model is SBA.opt
client <- data_3[[1]]
ffun <- partial(croston, f.type = "SBA.base")

## 100% service level not studying lead time
safety_lead_time <- 7
sl <- 1
overfilling <- FALSE

foo <- service_level_optimisation(client, sl, safety_lead_time, ffun, "del", TRUE)
bar <- service_level_optimisation(client, sl, safety_lead_time, ffun, "del", FALSE)


## Applying the models over every customer
## names are sim_*model*_*service level*_*safety lead time*_*data type*
ffun <- partial(croston, f.type = "SBA.base")
sim_crost_1_7_del <- mclapply(data_3, service_level_optimisation,
                              sl = 1,
                              safety_lead_time = 7,
                              ffun = ffun,
                              data_type = "del",
                              overfilling = FALSE,
                              mc.cores = 8)
saveRDS(sim_crost_1_7_del, "../data/master/paper3_sim/crost_1_7_del")

sim_crost_95_7_del <- mclapply(data_3, service_level_optimisation,
                               sl = 0.95,
                               safety_lead_time = 7,
                               ffun = ffun,
                               data_type = "del",
                               overfilling = FALSE,
                               mc.cores = 8)
saveRDS(sim_crost_95_7_del, "../data/master/paper3_sim/crost_95_7_del")

sim_crost_9_7_del <- mclapply(data_3, service_level_optimisation,
                              sl = 0.9,
                              safety_lead_time = 7,
                              ffun = ffun,
                              data_type = "del",
                              overfilling = FALSE,
                              mc.cores = 8)
saveRDS(sim_crost_9_7_del, "../data/master/paper3_sim/crost_9_7_del")

sim_crost_85_7_del <- mclapply(data_3, service_level_optimisation,
                               sl = 0.85,
                               safety_lead_time = 7,
                               ffun = ffun,
                               data_type = "del",
                               overfilling = FALSE,
                               mc.cores = 8)
saveRDS(sim_crost_85_7_del, "../data/master/paper3_sim/crost_85_7_del")

sim_crost_1_7_con <- mclapply(data_3, service_level_optimisation,
                              sl = 1,
                              safety_lead_time = 7,
                              ffun = ffun,
                              data_type = "con",
                              overfilling = FALSE,
                              mc.cores = 8)
saveRDS(sim_crost_1_7_con, "../data/master/paper3_sim/crost_1_7_con")

sim_crost_95_7_con <- mclapply(data_3, service_level_optimisation,
                               sl = 0.95,
                               safety_lead_time = 7,
                               ffun = ffun,
                               data_type = "con",
                               overfilling = FALSE,
                               mc.cores = 8)
saveRDS(sim_crost_95_7_con, "../data/master/paper3_sim/crost_95_7_con")

sim_crost_9_7_con <- mclapply(data_3, service_level_optimisation,
                              sl = 0.9,
                              safety_lead_time = 7,
                              ffun = ffun,
                              data_type = "con",
                              overfilling = FALSE,
                              mc.cores = 8)
saveRDS(sim_crost_9_7_con, "../data/master/paper3_sim/crost_9_7_con")

sim_crost_85_7_con <- mclapply(data_3, service_level_optimisation,
                               sl = 0.85,
                               safety_lead_time = 7,
                               ffun = ffun,
                               data_type = "con",
                               overfilling = FALSE,
                               mc.cores = 8)
saveRDS(sim_crost_85_7_con, "../data/master/paper3_sim/crost_85_7_con")

### Smaller lead time
sim_crost_1_3_con <- mclapply(data_3, service_level_optimisation,
                              sl = 1,
                              safety_lead_time = 3,
                              ffun = ffun,
                              data_type = "con",
                              overfilling = FALSE,
                              mc.cores = 8)
saveRDS(sim_crost_1_3_con, "../data/master/paper3_sim/crost_1_3_con")

sim_crost_95_3_con <- mclapply(data_3, service_level_optimisation,
                               sl = 0.95,
                               safety_lead_time = 3,
                               ffun = ffun,
                               data_type = "con",
                               overfilling = FALSE,
                               mc.cores = 8)
saveRDS(sim_crost_95_3_con, "../data/master/paper3_sim/crost_95_3_con")

sim_crost_9_3_con <- mclapply(data_3, service_level_optimisation,
                              sl = 0.9,
                              safety_lead_time = 3,
                              ffun = ffun,
                              data_type = "con",
                              overfilling = FALSE,
                              mc.cores = 8)
saveRDS(sim_crost_9_3_con, "../data/master/paper3_sim2/crost_9_3_con")

sim_crost_85_3_con <- mclapply(data_3, service_level_optimisation,
                               sl = 0.85,
                               safety_lead_time = 3,
                               ffun = ffun,
                               data_type = "con",
                               overfilling = FALSE,
                               mc.cores = 8)
saveRDS(sim_crost_85_3_con, "../data/master/paper3_sim/crost_85_3_con")

### Overfilling
## sim_crost_1_7_del_over <- mclapply(data_3, service_level_optimisation,
##                                    sl = 1,
##                                    safety_lead_time = 7,
##                                    ffun = ffun,
##                                    data_type = "del",
##                                    overfilling = TRUE,
##                                    mc.cores = 8)
## saveRDS(sim_crost_1_7_del_over, "../data/master/paper3_sim/crost_1_7_del_over")

## sim_crost_95_7_del_over <- mclapply(data_3, service_level_optimisation,
##                                     sl = 0.95,
##                                     safety_lead_time = 7,
##                                     ffun = ffun,
##                                     data_type = "del",
##                                     overfilling = TRUE,
##                                     mc.cores = 8)
## saveRDS(sim_crost_95_7_del_over, "../data/master/paper3_sim/crost_95_7_del_over")

## sim_crost_9_7_del_over <- mclapply(data_3, service_level_optimisation,
##                                    sl = 0.9,
##                                    safety_lead_time = 7,
##                                    ffun = ffun,
##                                    data_type = "del",
##                                    overfilling = TRUE,
##                                    mc.cores = 8)
## saveRDS(sim_crost_9_7_del_over, "../data/master/paper3_sim/crost_9_7_del_over")

## sim_crost_85_7_del_over <- mclapply(data_3, service_level_optimisation,
##                                     sl = 0.85,
##                                     safety_lead_time = 7,
##                                     ffun = ffun,
##                                     data_type = "del",
##                                     overfilling = TRUE,
##                                     mc.cores = 8)
## saveRDS(sim_crost_85_7_del_over, "../data/master/paper3_sim/crost_85_7_del_over")

## sim_crost_1_7_con_over <- mclapply(data_3, service_level_optimisation,
##                                    sl = 1,
##                                    safety_lead_time = 7,
##                                    ffun = ffun,
##                                    data_type = "con",
##                                    overfilling = TRUE,
##                                    mc.cores = 8)
## saveRDS(sim_crost_1_7_con_over, "../data/master/paper3_sim/crost_1_7_con_over")

## sim_crost_95_7_con_over <- mclapply(data_3, service_level_optimisation,
##                                     sl = 0.95,
##                                     safety_lead_time = 7,
##                                     ffun = ffun,
##                                     data_type = "con",
##                                     overfilling = TRUE,
##                                     mc.cores = 8)
## saveRDS(sim_crost_95_7_con_over, "../data/master/paper3_sim/crost_95_7_con_over")

## sim_crost_9_7_con_over <- mclapply(data_3, service_level_optimisation,
##                                    sl = 0.9,
##                                    safety_lead_time = 7,
##                                    ffun = ffun,
##                                    data_type = "con",
##                                    overfilling = TRUE,
##                                    mc.cores = 8)
## saveRDS(sim_crost_9_7_con_over, "../data/master/paper3_sim/crost_9_7_con_over")

## sim_crost_85_7_con_over <- mclapply(data_3, service_level_optimisation,
##                                     sl = 0.85,
##                                     safety_lead_time = 7,
##                                     ffun = ffun,
##                                     data_type = "con",
##                                     overfilling = TRUE,
##                                     mc.cores = 8)
## saveRDS(sim_crost_85_7_con_over, "../data/master/paper3_sim/crost_85_7_con_over")

## Loading data
sim_crost_1_7_con <- readRDS("../data/master/paper3_sim/crost_1_7_con")
sim_crost_95_7_con <- readRDS("../data/master/paper3_sim/crost_95_7_con")
sim_crost_9_7_con <- readRDS("../data/master/paper3_sim/crost_9_7_con")
sim_crost_85_7_con <- readRDS("../data/master/paper3_sim/crost_85_7_con")

sim_crost_1_7_del <- readRDS("../data/master/paper3_sim/crost_1_7_del")
sim_crost_95_7_del <- readRDS("../data/master/paper3_sim/crost_95_7_del")
sim_crost_9_7_del <- readRDS("../data/master/paper3_sim/crost_9_7_del")
sim_crost_85_7_del <- readRDS("../data/master/paper3_sim/crost_85_7_del")


sim_crost_1_3_con <- readRDS("../data/master/paper3_sim/crost_1_3_con")
sim_crost_95_3_con <- readRDS("../data/master/paper3_sim/crost_95_3_con")
sim_crost_9_3_con <- readRDS("../data/master/paper3_sim/crost_9_3_con")
sim_crost_85_3_con <- readRDS("../data/master/paper3_sim/crost_85_3_con")


## simulated_ndusls_10 <- mclapply(new_data, ndusl_simulation, safety_level = 0.1, mc.cores = 8)
## simulated_ndusls_12 <- mclapply(new_data, ndusl_simulation, safety_level = 0.12, mc.cores = 8)
## simulated_ndusls_14 <- mclapply(new_data, ndusl_simulation, safety_level = 0.14, mc.cores = 8)
## simulated_ndusls_16 <- mclapply(new_data, ndusl_simulation, safety_level = 0.16, mc.cores = 8)
## simulated_ndusls_18 <- mclapply(new_data, ndusl_simulation, safety_level = 0.18, mc.cores = 8)
## simulated_ndusls_20 <- mclapply(new_data, ndusl_simulation, safety_level = 0.2, mc.cores = 8)

## total_real_dels <- sum(sapply(simulated_ndusls_10, function(x) length(x$real_dates)))

## total_opt_dels <- sum(sapply(simulated_ndusls_10, function(x) length(x$dates)))

#####################################
## Evaluting the simulation results
#####################################
## Graph data
f <- function(x) sapply(x, function(x) x$opt_safety_level)
f2 <- function(x) sapply(x, function(x) length(x$sim$dates))

graph_data <- rbind(data.frame(safety_stock = f(sim_crost_1_7_del),
                               dels = f2(sim_crost_1_7_del),
                               service_level = 1,
                               type = "Delivery LT=7"),
                    data.frame(safety_stock = f(sim_crost_95_7_del),
                               dels = f2(sim_crost_95_7_del),
                               service_level = 0.95,
                               type = "Delivery LT=7"),
                    data.frame(safety_stock = f(sim_crost_9_7_del),
                               dels = f2(sim_crost_9_7_del),
                               service_level = 0.9,
                               type = "Delivery LT=7"),
                    data.frame(safety_stock = f(sim_crost_85_7_del),
                               dels = f2(sim_crost_85_7_del),
                               service_level = 0.85,
                               type = "Delivery LT=7"),
                    data.frame(safety_stock = f(sim_crost_1_7_con),
                               dels = f2(sim_crost_1_7_con),
                               service_level = 1,
                               type = "Telemetry LT=7"),
                    data.frame(safety_stock = f(sim_crost_95_7_con),
                               dels = f2(sim_crost_95_7_con),
                               service_level = 0.95,
                               type = "Telemetry LT=7"),
                    data.frame(safety_stock = f(sim_crost_9_7_con),
                               dels = f2(sim_crost_9_7_con),
                               service_level = 0.9,
                               type = "Telemetry LT=7"),
                    data.frame(safety_stock = f(sim_crost_85_7_con),
                               dels = f2(sim_crost_85_7_con),
                               service_level = 0.85,
                               type = "Telemetry LT=7"),
                    data.frame(safety_stock = f(sim_crost_1_3_con),
                               dels = f2(sim_crost_1_3_con),
                               service_level = 1,
                               type = "Telemetry LT=3"),
                    data.frame(safety_stock = f(sim_crost_95_3_con),
                               dels = f2(sim_crost_95_3_con),
                               service_level = 0.95,
                               type = "Telemetry LT=3"),
                    data.frame(safety_stock = f(sim_crost_9_3_con),
                               dels = f2(sim_crost_9_3_con),
                               service_level = 0.9,
                               type = "Telemetry LT=3"),
                    data.frame(safety_stock = f(sim_crost_85_3_con),
                               dels = f2(sim_crost_85_3_con),
                               service_level = 0.85,
                               type = "Telemetry LT=3"))

graph_data$service_level <- as.factor(graph_data$service_level)

graph_data$stock <- graph_data$safety_stock * sapply(data_3, function(x) x$ndusl$max_tel)

ggplot(graph_data, aes(x=service_level, y=safety_stock, fill=type)) +
    geom_boxplot(outlier.size = 0.5) +
    xlab("Service Level") +
    ylab("Safety Stock") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          text = element_text(size = 8)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_x_discrete(labels = c("85%", "90%", "95%", "100%"))

ggsave("./papers/paper3/dist.png", device = "png", width = 90, height = 90, units = "mm", dpi = 1000)
ggsave("./papers/paper3/dist.eps", device = cairo_ps, width = 90, height = 90, units = "mm", dpi = 1000)

## Average safety stock for the types
graph_data %>% group_by(service_level, type) %>% summarise(mean_safety_stock = mean(safety_stock))

graph_data %>% group_by(type) %>% summarise(mean_safety_stock = mean(safety_stock))

## Trade-off curves
trade_data <- graph_data %>%
    group_by(type, service_level) %>%
    summarise(total_stock = sum(stock), total_del = sum(dels))

trade_data$service_level <- as.numeric(as.character(trade_data$service_level))

ggplot(data = trade_data, aes(x=total_stock, y = service_level, linetype = type, color = type)) +
    geom_line() +
    geom_point() +
    xlab("Σ Inventory Stock (units)") +
    ylab("Service Level") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          text = element_text(size = 8)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))

ggsave("./papers/paper3/inventory.png", device = "png", width = 90, height = 90, units = "mm", dpi = 1000)
ggsave("./papers/paper3/inventory.eps", device = cairo_ps, width = 90, height = 90, units = "mm", dpi = 1000)


ggplot(data = trade_data, aes(x=total_del, y = service_level, linetype = type, color = type)) +
    geom_line() +
    geom_point() +
    xlab("Σ Deliveries") +
    ylab("Service Level") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          text = element_text(size = 8)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))

ggsave("./papers/paper3/deliveries.png", device = "png", width = 90, height = 90, units = "mm", dpi = 1000)
ggsave("./papers/paper3/deliveries.eps", device = cairo_ps, width = 90, height = 90, units = "mm", dpi = 1000)


## delivery_gain <- function(sim_ndusl) {
##     ## Gain of delivery as a funciton of the safety stock
##     total_real_dels <- sum(sapply(sim_ndusl, function(x) length(x$real_dates)))
##     total_opt_dels <- sum(sapply(sim_ndusl, function(x) length(x$dates)))

##     gain <- total_real_dels - total_opt_dels

##     return(gain)
## }

## gains <- sapply(list(simulated_ndusls_10,
##          simulated_ndusls_12,
##          simulated_ndusls_14,
##          simulated_ndusls_16,
##          simulated_ndusls_18,
##          simulated_ndusls_20),
##        delivery_gain)

## png(filename="./papers/paper3/gain_opt.png")
## plot(x = c(10, 12, 14, 16, 18, 20), y = gains)
## dev.off()

## Maybe we want to evaluate the smothness of total deliveries as a function of safety stock

## Graph of the number and amount of deliveries over time
## dates <- unlist(lapply(simulated_ndusls_10, function(x) x$dates))

## table(dates)


## The dates aren't the same for everyone

## foo <- lapply(new_data, function(x) tail(x$data$con$orig, 365))
