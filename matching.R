#' title: matching.R
#' comments: provides functions for matching together deliveries and telemetry
#'           data
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(magrittr)
library(lubridate)
library(dplyr)
library(pryr)

#' imports
source("operators.R")

#' functions
deliveries_from_telemetry <- function(telemetry, threshold = 10, max_ratio = NULL) {
    #' loop that looks over all the telemetry and find sequences of positive amounts
    #' Initialize the looping variables
    telemetry %<>% as.data.frame  # Indexing is faster as a data.frame
    j <- 1
    date <- vector(mode = "raw", length = 0)
    amount <- vector(mode = "numeric", length = 0)
    telemetry$first_difference <- c(0, diff(telemetry$level))

    while (j <= nrow(telemetry)) {
        a <- 0

        ## If we are over the threshold we consider it the beginning of a refil
        if (telemetry[j, "first_difference"] > threshold) {
            while (telemetry[j + a, "first_difference"] >= 0) {
                if ((j+a) == nrow(telemetry)) {
                    a <- a + 1
                    break
                }
                a <- a + 1  # continuous refill counter
            }

            ## Appending to vectors is the fastest way in R to store results
            ## from explicit loops.
            date <- c(date, telemetry[j-1, 1])  # Start date of loop
            amount <- c(amount, sum(telemetry[j:(j + a - 1), "first_difference"]))

            j <- j + a  # Continue the loop but after our continuous refill counter
        } else {
            j <- j + 1
        }
    }

    if (length(date) == 0) {
        ## If we didn't find any deliveries in our telemetry data
        return(NULL)
    } else {
        ## The date has to be converted back into POSIXct from numeric as the raw
        ## vector containing the dates is the numeric values. Still need to supply
        ## the unix origin for POSIXct.
        res <- data.frame(Date=as.POSIXct.numeric(date, origin = '1970-01-01 00:00:00'),
                          Amount=amount) %>%
            plyr::arrange(., Date)

        if (!is.null(max_ratio)) {
            ## Max ratio is used to filter out deliveries by comparing it to the
            ## maximum delivery in the serie
            min_del <- max(res$Amount) * max_ratio
            res <- res[res$Amount > min_del,]
        }
        rownames(res) <- NULL
        return(res)
    }
}


is_Date <- function(x) {
    return(inherits(x, 'Date'))
}


is_POSIXct <- function(x) {
    return(inherits(x, 'POSIXct'))
}


match_time_series <- function(a, b, time_window=10) {
    #' Match two time series time index together
    #' Input - (a,b): two dataframes containing the time series
    #'         time_window: the window of time to search around for a match
    #' Output - A dataframe containing both initial time series with matches beeing
    #'          next to each other.


    ## Check if the input data is properly formatted data.frame should have two
    ## columns. First column should be convertible to dates or POSIXct.
    if (!all(sapply(a[[1]], is_Date)) & !all(sapply(a[[1]], is_POSIXct))) {
        stop("Incorrect date format in the first column of dataframe a. Expected date or POSIXct")
    } else if (!all(sapply(b[[1]], is_Date)) & !all(sapply(b[[1]], is_POSIXct))) {
        stop("Incorrect date format in the first column of dataframe b. Expected date or POSIXct")
    }


    a %<>% dplyr::arrange(.[[1]])
    b %<>% dplyr::arrange(.[[1]])

    ## To improve the speed as well as reduce the
    flipped <- FALSE
    if (nrow(b) < nrow(a)) {
        new_a <- b
        new_b <- a
        a <- new_a
        b <- new_b
        flipped <- TRUE
    }

    matches <- vector(mode = "logical", length = nrow(b))

    get_a_match <- function(x, time_list, time_window) {
        #' Determines if x (a time stamp) can be matched to any of dates in time_list
        #' If a match is found return the closest match.
        #' Also update our list of found values.

        updated_time_list <- time_list[matches==FALSE]

        ## Don't continue the function if we have completed every possible match
        if (length(updated_time_list) == 0) {
            return(list(as.POSIXct(NA), NA, NA))
        }

        f <- pryr::partial(difftime, time2 = x, units = "hours")

        res <- sapply(updated_time_list, abs %c% as.double %c% f)

        small_ind <- which.min(res)

        if (res[small_ind] < time_window) {
            matches[matches==FALSE][small_ind] <<- TRUE
            date <- updated_time_list[small_ind]
            b_value <- b[[2]][which(b[[1]]==date)]
            return(list(date, b_value, res[small_ind]))
        } else {
            return(list(as.POSIXct(NA), NA, NA))
        }
    }


    f <- pryr::partial(get_a_match, time_list = b[[1]], time_window = time_window)
    res <- lapply(a[[1]], f)

    matching_dates <- do.call(c, lapply(res, function(x) x[[1]]))
    matching_values <- do.call(c, lapply(res, function(x) x[[2]]))
    time_diffs <- do.call(c, lapply(res, function(x) x[[3]]))

    #' Merging both dataframes and lining up the matches
    #' Start by merging the data with the matches.
    #' Then add the missing dates from b
    res_df <- data.frame(a[[1]], matching_dates, a[[2]], matching_values, time_diffs) %>%
        merge(.,
              b[matches==FALSE,],
              by.x = c("matching_dates", "matching_values"),
              by.y = names(b),
              all = TRUE)

    colnames(res_df) <- c(names(b), names(a), "time_diff")

    ## Order the df with both dates column merged together
    merged_time <- res_df[,1]
    merged_time[is.na(merged_time)] <- res_df[,3][is.na(res_df[,1])]
    res_df <- res_df[order(merged_time),]
    rownames(res_df) <- NULL

    if (flipped) {
        res_df <- res_df[,c(3,4,1,2,5)]
    }

    ##class(res_df) <- c("MatchedTimeSeries", class(res_df))

    return(res_df)
}


cor_matched_time_series <- function(df) {
    return(cor(df[[2]], df[[4]], use = "pairwise.complete.obs", method = "pearson"))
}


matching_ratio <- function(df) {
    return(sum(complete.cases(df))/nrow(df))
}


best_start_matching <- function(df) {
    #' Return the best start row index of the matches dataframe.
    #' i.e. return the index of first row that does not have any NA values
    #' starting from the top of dataframe

    na_list <- complete.cases(df)
    for (i in seq_along(na_list)) {
        if (na_list[i]) {
            return(i)
        }
    }
}


best_end_matching <- function(df) {
    #' Return the best end row index of the matches dataframe.
    #' i.e. return the index of the first row that does not have nay NA values
    #' starting from the end of the df.

    na_list <- complete.cases(df)
    for (i in rev(seq_along(na_list))) {
        if (na_list[i]) {
            return(i)
        }
    }
}


peak_tel_at_del <- function(del, tel) {
    #' Return the peak telemetry during a refill starting at time of delivery
    #' This code only works on the processed telemetry data
    start <- which(tel$datetime == del)  # Start of the day
    i <- start
    j <- i+1
    counter <- 1  # counter so we don't go over a whole day during our search

    ## We have a -3 here in the check this avoids stopping when reaching small
    ## fluctuations which may still persist even after cleaning the data.
    ##        50 50.2 49.8
    ## diff   -0.2  0.4
    ## a >= 0 is equivalent to tel$level[[i]] >= tel$level[[j]]

    if (tel$level[[i]] - tel$level[[j]] >= -3) {
        ## The level is decreasing at the start of the day.
        ## 51 50 49 70 90 80
        ## i  j
        ## The refil was done later in the day.
        ## Let's loop to find to find when the refil starts.
        while (tel$level[[i]] - tel$level[[j]] >= -3 && counter < 25) {
            i <- j
            j <- i+1
            counter <- counter+1
            ## Happens if the delivery was done at exactly 00:00:00 the start of the day
            ## 50 | 80 79 78 77 ...
            ##      i  j
            ## Our first loop would continue until it found a next peak maybe days later
        }
        ## This loops ends when we find the beginning of the delivery
        ## but not necessarely the max level during the delivery
        ## This can happen if for example it goes 51 50 49 70 90 80
        ## We will break with pointers at               i  j
    }

    if (tel$level[[i]] <= tel$level[[j]]) {
        ## We are at a delivery
        ## Either we started the day with a delivery or we got here from the
        ## previous loop.
        ## 51 50 49 70 90 80
        ##       i  j

        ## Keep searching until we find the max
        ## Max is found when the following level is smaller
        ## when this inequality doesn't hold
        while (tel$level[[i]] <= tel$level[[j]]) {
            i <- j
            j <- i+1
        }
        ## Break happens
        ## 51 50 49 70 90 80
        ##             i  j
    }
    if (counter < 25) {
        ## Our peak is now at pointer i
        peak <- tel$level[[i]]
    } else {
        ## We hit the counter we went a whole day with decreasing amounts
        ## the first day was the peak refill
        peak <- tel$level[[start]]
    }

    return(peak)
}

#' main
if (FALSE) {  # Prevent it from being run when sourcing the file

    ## has duplicate and zeros DQ_DAILYHOUN_TX_11168
    client <- cvd[[2]]
    tank <- client$tank[[1]]
    sample_tel <- tank$telemetry
    tel_deliveries <- deliveries_from_telemetry(sample_tel, threshold = 10)

    fetch_serie_time(client, "tel", "2015-06-23", "2015-06-24")

    system.time(deliveries_from_telemetry(sample_tel, threshold = 10))

    actual_del <- ts_for_delivery(deliveries_for_tank(tank, client))
    match_time_series(tel_deliveries, actual_del, 10)
    system.time(match_time_series(tel_deliveries, actual_del, 10))

    ## Check for NA ShiftRealStartDateTime
    f <- function(client) {
        actual_del <- ts_for_delivery(client$delivery[[1]])
        if(any(is.na(actual_del[["ShiftRealStartDateTime"]]))) {
            return(client$UID)
        } else {
            return(NULL)
        }
    }
    lapply(sample, f) %>% plyr::compact()

}
