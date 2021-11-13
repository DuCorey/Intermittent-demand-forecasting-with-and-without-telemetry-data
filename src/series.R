#' title: series.R
#' comments: time series related manipulation functions
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(xts)

#' imports

#' functions
convert_xts <- function(serie) {
    if (is(serie, "data.frame")) {
        serie <- as.xts(serie[[2]], order.by = as.POSIXct(serie[[1]], tz = tz(serie[[1]][[1]])))
    }

    return(serie)
}


convert_xts_weekly <- function(serie) {
    if (is(serie, "data.frame")) {
        serie <- as.xts(serie[[2]], order.by = as.Date(serie[[1]], tz = tz(serie[[1]][[1]])))
    }

    res <- xts::apply.weekly(serie, sum)
    attr(res, 'frequency') <- 52

    ## Hard setting endpoints to be a full week
    ## myTs <- clus_tel_day$del$orig[[5]]
    ## ep <- endpoints(myTs,'weeks')
    ## ts.weeks <- apply.monthly(myTs, sum)
    ## attributes(ts.weeks)$index[length(ts.weeks)] <- as.POSIXct(last(index(myTs)) + 7-last(floor(diff(ep))))

    return(res)
}


convert_xts_daily <- function(serie, FUN = sum) {
    if (is(serie, "data.frame")) {
        serie <- as.xts(serie[[2]], order.by = as.Date(serie[[1]], tz = tz(serie[[1]][[1]])))
    }

    res <- xts::apply.daily(serie, FUN)
    attr(res, 'frequency') <- 7
    return(res)
}


convert_xts_monthly <- function(serie) {
    if (is(serie, "data.frame")) {
        serie <- as.xts(serie[[2]], order.by = as.Date(serie[[1]], tz = tz(serie[[1]][[1]])))
    }

    res <- xts::apply.monthly(serie, sum)
    attr(res, 'frequency') <- 12
    return(res)
}


trim_ts <- function(serie, n, how = "both") {
    #' Remove n values in a time series object.
    #' Series can be trimed from the start, the end and both.

    how  <- match.arg(how, c("both", "start", "end"))

    ## Don't trim if there's 0 length
    if (n == 0) {
        return(serie)
    }
    if (how %in% c("start", "both")) {
        serie <- tail(serie, -n)
    }
    if (how %in% c("end", "both")) {
        serie <- head(serie, -n)
    }
    return(serie)
}


merge_xts_list <- function(l) {
    #' Merge multiple xts series contianed in a list
    return(Reduce(merge, l[2:length(l)], l[[1]]))
}


match_ends <- function(a, b, how) {
    #' Takes two xts and returns them so that both ends match
    #' It cuts off the extra in the larger time series.
    how <- match.arg(how, c("start", "end"))

    if (how == "end") {
        min_end <- min(end(a), end(b))
        a <- a[xts_range(start(a), min_end)]
        b <- b[xts_range(start(b), min_end)]
    } else if (how == "start") {
        max_start <- max(start(a), start(b))
        a <- a[xts_range(max_start, end(a))]
        b <- b[xts_range(max_start, end(b))]
    }

    return(list(a,b))
}


xts_range <- function(start, end) {
    #' Create an xts range from start and end dates
    return(paste(start, end, sep = "::"))
}


generate_xts <- function(len) {
    #' Generate a random xts series of length len
    return(xts(runif(len), seq(from = Sys.Date(), by = 1, length.out = len)))
}


make_xts <- function(data) {
    #' Attempt to convert the data into xts data
    if (is.list(data)) {
        return(lapply(data, make_xts))
    } else {
        return(as.xts(ts(data)))
    }
}


filter_year <- function(serie, year) {
    #' Return a time serie with the only the specified year time stamp
    #' year can be a string or list to
    #' if year is list will return time serie with time stamps which match to
    #' any in the list
    if (is.null(serie)) {
        return(NULL)
    }

    if (length(year) > 1) {
        filt <- serie[format(index(serie), "%Y") %in% year]
    } else {
        filt <- serie[format(index(serie), "%Y") == as.character(year)]
    }

    return(filt)
}


filter_quarter <- function(serie, quarters) {
    if (is.null(serie)) {
        return(NULL)
    }

    filt <- serie[format(as.yearqtr(index(serie)), "%q") %in% quarters]

    return(filt)
}


days_in_quarter <- function(quarter, year) {
    switch(quarter,
           "1" = ifelse(leap_year(year), 91, 90),
           "2" = 91,
           "3" = 92,
           "4" = 92)
}


filter_full_quarter <- function(serie, quarter, year) {
    if (is.null(serie)) {
        return(NULL)
    }

    filt <- filter_quarter(filter_year(serie, year), quarter)
    n_periods <- days_in_quarter(quarter, year)

    if (nrow(filt) == n_periods) {
        return(filt)
    } else {
        return(NULL)
    }
}


days_in_year <- function(x) {
    if (leap_year(as.numeric(x))) {
        n_days <- 366
    } else {
        n_days <- 365
    }
    return(n_days)
}


filter_full_year <- function(serie, time_scale, year = "2016") {
    #' Return a series with only the specified year of data if the year has
    #' full observations of data
    if (is.null(serie)) {
        return(NULL)
    }

    time_scale <- match.arg(time_scale, c("weeks", "days", "months"))

    #' Filter time series for a specific year
    filt <- filter_year(serie, year)
    n_periods <- switch(time_scale,
                        weeks = 52, days = days_in_year(year), months = 12)

    if (nrow(filt) >= n_periods) {
        return(filt)
    } else {
        return(NULL)
    }
}


number_of_days_between_dates <- function(start, end) {
    start <- as.Date(start)
    end <- as.Date(end)

    if (start > end) {
        stop("Start date cannot be larger than end date.")
    }

    count <- 0

    while (start <= end) {
        count <- count + 1
        start <- start + 1
    }

    return(count)
}


number_of_months_for_days <- function(h, start) {
    ## Determine the number of months to forecast to achieve the an horizon of h in days
    ## Excluding the start month
    ## TODO leap years
    start <- as.yearmon(date(start))

    format(start,
    a <- h
    cur_month <- month(start)
    cur_year <- year(start)
    c <- 0
    while (a > 0) {
        c <- c + 1
        cur_month <- mod(cur_month + 1, 12)
        days <- as.numeric(days_in_month(cur_month))
        a  <- a - days
    }
    return(c)
}


filter_full_xts_observations <- function(serie, start, end) {
    #' Return the time series between the specified start and end dates
    #' but only if all observations are present
    if (is.null(serie)) {
        return(NULL)
    }

    tryCatch({
        filt <- serie[xts_range(start, end)]
    },
    ## If the function doesn't even exist in those ranges
    error = function(cond) {
        return(NULL)
    })

    n_periods <- number_of_days_between_dates(start, end)

    if (nrow(filt) >= n_periods) {
        return(filt)
    } else {
        return(NULL)
    }
}


mv_serie <- function(x, y, pad = 0) {
    #' Return a multivate serie when given a x and y series
    merged <- merge(x, y)

    merged <- na.omit(merged)

    ## Add some 0 padding for the mv clustering method
    if (pad > 0) {
        if (is.Date(start(merged))) {
            ## Start padding
            dates <- as.Date((start(merged)-pad):(start(merged)-1L))
            zeros <- xts::xts(x = matrix(0, nrow = pad, ncol = 2), order.by = dates)
            merged <- rbind(merged, zeros)

            ## End padding
            dates <- as.Date((end(merged)+1L):(end(merged)+pad))
            zeros <- xts::xts(x = matrix(0, nrow = pad, ncol = 2), order.by = dates)
            merged <- rbind(merged, zeros)

        } else {
            stop("Unsupported date format for xts padding.")
        }
    }
    return(merged)
}


## mv_serie <- function(x, y, pad = 0) {
##     #' Return a multivate serie when given a x and y series
##     merged <- merge(x, y) %>%
##         as.matrix(.)  # We can't add padding to an xts object so we convert to matrix

##     ## Remove NAs, can't use drop NA since this is a matrix
##     merged <- merged[complete.cases(merged),]

##     ## Add some 0 padding for the mv clustering method
##     if (pad > 0) {
##         zeros <- rep(0, times = pad)
##         padding <- as.matrix(data.frame(x = zeros, y = zeros))
##         merged <- rbind(padding, merged, padding)
##     }

##     return(merged)
## }
