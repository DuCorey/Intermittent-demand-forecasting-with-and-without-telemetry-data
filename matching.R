library(magrittr)
library(lubridate)
library(dplyr)
library(pryr)
"%o%" <- pryr::compose


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
                a <- a + 1  # continuous refill  counter
            }

            ## Appending to vectors is the fastest way in R to store results
            ## from explicit loops.
            date <- c(date, telemetry[j, 1])
            amount <- c(amount, sum(telemetry[j:(j + a - 1), "first_difference"]))

            j <- j + a  # Continue the loop but after our continuous refill  counter
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

        res <- sapply(updated_time_list, abs %o% as.double %o% f)

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

    matching_dates <- do.call(c, lapply(res, . %>% .[[1]]))
    matching_values <- do.call(c, lapply(res, . %>% .[[2]]))
    time_diffs <- do.call(c, lapply(res, . %>% .[[3]]))

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


if (FALSE) {  # Prevent it from being run when sourcing the file

    ## has duplicate and zeros DQ_DAILYHOUN_TX_11168

    client <- sample[[2]]
    tank <- client$tank[[1]]
    sample_tel <- tank$telemetry
    tel_deliveries <- deliveries_from_telemetry(sample_tel, threshold = 20)
    system.time(deliveries_from_telemetry(sample_tel, threshold = 20))
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
