#' title: series.R
#' comments: time series related manipulation functions
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(xts)

#' imports

#' functions
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


convert_xts_daily <- function(serie) {
    if (is(serie, "data.frame")) {
        serie <- as.xts(serie[[2]], order.by = as.Date(serie[[1]], tz = tz(serie[[1]][[1]])))
    }

    res <- xts::apply.daily(serie, sum)
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


filter_year <- function(serie, time_scale, year = "2016") {
    if (is.null(serie)) {
        return(NULL)
    }

    time_scale <- match.arg(time_scale, c("weeks", "days", "months"))

    #' Filter time series for a specific year
    filt <- serie[format(index(serie), "%Y") == as.character(year)]
    n_periods <- switch(time_scale,
                        weeks = 52, days = 365, months = 12)

    if (nrow(filt) >= n_periods) {
        return(filt)
    } else {
        return(NULL)
    }
}
