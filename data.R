#' title: data.R
#' comments: this file creates the data objects necessary for
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(plyr) # load plyr before dplyr
library(dplyr)
library(pryr)
library(tidyr)
library(magrittr)
library(readxl)
library(data.table)
library(lubridate)
#library(xts)
library(imputeTS)

#' imports
source("operators.R")
source("matching.R")
#source("convert.R")

#' functions
## blacklist DPs that have very odd problems that simply aren't worth the time
## to fix
blacklist <- c("70835")


read_csv_skip_second_line <- function(file)
{
    data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
    return(data[-1,])
}


fix_raw_delivery_column_names <- function(data)
{
    colnames(data)[which(names(data) == "ExternalDeliveryNote")] <- "ExtDeliveryNote"
    return(data)
}


### data objects
## Delivery data
DeliveryData <- function()
{
    sources <- c("../data/internship data/Reports/2015",
                 "../data/internship data/Reports/2016",
                 "../data/internship data/Reports")
    f <- pryr::partial(list.files, pattern = "*.csv", full.names = TRUE)
    files <- do.call(c, lapply(sources, f))

    merged_data <-
        ## Don't use rbindlist it will cause errors later when converting the dates
        do.call(rbind,
                lapply(files, fix_raw_delivery_column_names %o% read_csv_skip_second_line)) %>%
        tidyr::drop_na(., X, DPNumber) %>%

        ## We only keep distinct deliveries based on ShiftRealStartDateTime
        dplyr::distinct(., ShiftRealStartDateTime, DPNumber, .keep_all = TRUE)

    ## Convert strings to dates
    ## First convert empty strings into NA so that the conversion to POSIXct can work
    merged_data$RealStartDateTime[merged_data$RealStartDateTime == ""]           <- NA
    merged_data$ShiftRealStartDateTime[merged_data$ShiftRealStartDateTime == ""] <- NA
    merged_data$ConfirmationDate[merged_data$ConfirmationDate == ""]             <- NA

    merged_data$RealStartDateTime      %<>% as.POSIXct(., tz = "America/New_York")
    merged_data$ShiftRealStartDateTime %<>% as.POSIXct(., tz = "America/New_York")
    merged_data$ConfirmationDate       %<>% as.POSIXct(., tz = "America/New_York")

    ## Speed up subsetting using data.table
    merged_data %<>% data.table::as.data.table() %>%
        ## Remove rows with 0 delivered quantity
        .[DeliveredQuantity > 0] %>%
        ## Remove rows without ShiftRealStartDateTime
        tidyr::drop_na(., ShiftRealStartDateTime)

    ## Remove low delivery amounts for a product
    merged_data %<>% .[!(RawProduct %in% c("LO2", "LAR", "LN2") & DeliveredQuantity <= 8000)]


    # Determine unique client depot numbers
    dps <- unique(merged_data[["DPNumber"]])


    structure(
        list(
            sources = sources,
            files = files,
            data = merged_data,
            DPNumbers = dps
        ),
        class = "DeliveryData"
    )
}


print.DeliveryData <- function(x, ...)
{
    invisible(x)
}


summary.DeliveryData <- function(x, ...)
{
    summary(x$data)
}


head.DeliveryData <- function(x, ...)
{
    head(x$data)
}


## deliveries_for_dp_nums <- function(delivery_data, dp_nums)
##{

    ##     f <- function(key, value)
    ##{
##          delivery_subset <- delivery_data[DPNumber == key]
##          return(Delivery(delivery_subset, key, value))
##     }

##     res <- mapply(f, key=names(dp_nums), value=dp_nums, SIMPLIFY = FALSE, USE.NAMES = FALSE)
##     return(res)
## }


Delivery <- function(df, dp_num)
{
    data <- subset(df, select = c("DispatchZoneCode",
                                  "DispatchCenterCode",
                                  "DeliveringDispatchZoneCode",
                                  "DeliveringDispatchCenterCode",
                                  "SalesOrganizationName",
                                  ##"AccountNumber",
                                  ##"SubAccountNumber",
                                  "DefaultSource",
                                  "RawProduct",
                                  "Grade",
                                  ##"ProductCode",
                                  ##"ProductName",
                                  "MonitoringType",
                                  ## "ShiftNumber",
                                  ## "ShiftType",
                                  ## "StopNumber",
                                  ## "ShiftStatus",
                                  "RealStartDateTime",
                                  "ShiftRealStartDateTime",
                                  "ConfirmationDate",
                                  "ConfirmationDelay",
                                  "DeliveredQuantity",
                                  "ReturnedQuantity",
                                  "DeliveryUnit",
                                  "TractorNumber",
                                  "ActualSourceCode",
                                  "TotalTripDistanceKm",
                                  "ReturnedTrailer",
                                  "DeliveredTrailer",
                                  "TotalTripDistanceMile",
                                  "DeliveryTripDistanceMile"
                                  ## "ShiftLoadingQty",
                                  ## "ShiftDNQty",
                                  ## "ShiftEventQty",
                                  ##"ActualSourceCode"
                                  )
                   )
    rownames(data) <- NULL


    structure(
        list(
            DPNumber = dp_num,
            df = data
            ## tanks = tanks
        ),
        class = "Delivery"
    )
}

## Telemetry data
TelemetryData <- function()
{
    files <- c("../data/internship data/VW_LEVEL_AND_PRESSURE_IM.csv")

    data <- read.csv(files, header = TRUE, stringsAsFactors=FALSE)

    ## Convert time to eastern time
    ## Our time is a raw string with a timezone attached to it.
    ## To convert to eastern time, we first take our time and subtract
    ## its timezone, thus creating a UTC (GMT) time. We then convert from UTC to
    ## America/New_York the standard eastern time zone.
    data$DATETIME %<>% as.POSIXct(., tz="GMT") %>%
        subtract(., lubridate::hm(paste(data$TIMEZONE, ":00", sep=""))) %>%
        lubridate::with_tz(., tzone = "America/New_York")

    sites <- unique(data[, "SITENAME"])

    structure(
        list(
            files = files,
            data = data.table::as.data.table(data),
            sites = sites
        ),
        class = "TelemetryData"
    )
}


print.TelemetryData <- function(x, ...)
{
    invisible(x)
}


summary.TelemetryData <- function(x, ...)
{
    summary(x$data)
}


head.TelemtryData <- function(x, ...)
{
    head(x$data)
}


## Tank level data
Tank <- function(tank, tank_info_sub_tank, telemetry_subset, sitename)
{
    ## Tank information
    product <- tank_info_sub_tank[, "Product"]
    max_level <- tank_info_sub_tank[, "MaxLevel"]
    max_level_unit <- tank_info_sub_tank[, "Abbreviation__1"]
    orientation <- tank_info_sub_tank[, "Name"]
    capacity <- tank_info_sub_tank[, "Capacity"]
    capacity_unit <- tank_info_sub_tank[, "Abbreviation"]
    timezone <- telemetry_subset[1, "TIMEZONE"]

    unit <- telemetry_subset[1, "CUSTOMERUNIT"]
    rtu <- telemetry_subset[1, "RTUTYPE"]


    ## Telemetry time serie
    telemetry_serie <- telemetry_subset[, c("DATETIME", "CUSTOMERVALUE")] %>%
        plyr::arrange(., DATETIME) %>%
        dplyr::distinct(., DATETIME, .keep_all = TRUE) %>%
        plyr::rename(., c("DATETIME"="datetime", "CUSTOMERVALUE"="level")) %>%
        remove_fluctuations %>%
        add_missing_hours %>%
        best_subsequence(., consec = 24)
    telemetry_serie[[2]] %<>% imputeTS::na.interpolation()
    telemetry_serie %<>% drop_non_rounded_hours

    ## Daily consumption time serie
    consumption_serie <- daily_consumption_from_telemetry_serie(telemetry_serie)


    structure(
        list(
            id = as.character(tank),
            sitename = sitename,
            product = as.character(product),
            max.level = as.numeric(max_level),
            max.level.unit = as.character(max_level_unit),
            orientation = as.character(orientation),
            capacity = as.numeric(capacity),
            capacity.unit = as.character(capacity_unit),

            timezone = as.numeric(timezone),
            unit = as.character(unit),
            rtu = as.character(rtu),

            telemetry.serie = as.data.frame(telemetry_serie),
            consumption.serie = as.data.frame(consumption_serie)
        ),
        class = "Tank"
    )
}


remove_fluctuations <- function(serie)
{
    #' Remove fluctuations in telemetry data
    level <- serie[[2]]
    right <- 2*abs(diff(level, lag = 2))
    foo <- abs(diff(level[1:length(level)], lag = 1))
    left <- head(foo, -1) + tail(foo, -1)
    serie[[2]][which(left>right) + 1] <- NA
    return(serie)
}


add_missing_hours <- function(serie)
{
    #' Add missing hours to serie
    start <- round(serie[[1]][[1]], units = "hours")
    end <- round(serie[[1]][[nrow(serie)]], units = "hours")
    hours <- data.frame(datetime=seq(start, end, by = "hours"))
    serie <- merge(serie, hours, by=c("datetime"), all = TRUE)
    return(serie)
}


add_missing_days <- function(serie)
{
    #' Add missing days to serie
    start <- round(serie[[1]][[1]], units = "days")
    end <- round(serie[[1]][[nrow(serie)]], units = "days")
    days <- data.frame(datetime=seq(start, end, by = "days"))
    serie <- merge(serie, days, by=c("datetime"), all = TRUE) %>%
        imputeTS::na.replace(., fill = 0)
    return(serie)
}


best_subsequence <- function(serie, consec)
{
    #' Find longuest continuous subset that does not have more than x consecutive
    #' missing values

    ## Change NA values with a number we know isn't in the data
    unique_value <- max(serie[[2]], na.rm = TRUE) + 1
    serie[[2]][is.na(serie[[2]])] <- unique_value

    ## Convert the serie to a run lenght encoding (rle).
    ## We will use the properties of rle to more easily find the longuest subsequence.
    ## Also using rle directly avoid us having to use slower looping algorithms.
    ## (slower in R that is).
    ## Replacing Nas with a unique value is necessary since NA are treated as
    ## unique values in the rle.
    final_rle <- rle(serie[[2]])

    ## Find values in the rle encoding which are longuer than our consecutive limit
    foo <- which(final_rle$lengths > consec & final_rle$values == unique_value)

    ## Convert our unique_values back to NA
    serie[[2]][serie[[2]] == unique_value] <- NA

    if (length(foo) == 0L) {
        ## we didn't find any values longuer than our consecutive limit
        return(serie)
    } else {
        ## For each of the points we found we iterate to find the length of that subset
        ## Add beginning and end to the iteration
        split_points <- c(0, foo, length(final_rle$lengths)+1)
        split_res <- vector("list", (length(split_points)-1))
        for (i in seq(1, (length(split_points)-1))) {
            a <- split_points[i]+1
            b <- split_points[i+1]-1
            res <- sum(final_rle$lengths[a:b])
            split_res[[i]] <- list(a=a, b=b, length=res)
        }
        c <- which.max(lapply(split_res, function(x) x$length))

        ## Converting the rle indexes into our original series
        start <- sum(final_rle$lengths[1:split_res[[c]]$a])
        end   <- sum(final_rle$lengths[1:split_res[[c]]$b])
        final_serie <- serie[start:end,]

        return(final_serie)
    }
}


drop_non_rounded_hours <- function(serie)
{
    #' Remove non rounded hours
    res <- serie[,datetime:=as.POSIXct(round(datetime, units = "hours"))] %>%
        dplyr::distinct(.data = serie, datetime, .keep_all = TRUE)
    return(res)
}


daily_consumption_from_telemetry_serie <- function(serie)
{
    serie$first_difference <- c(0, diff(serie[[2]]))

    ## Positive values in the first difference are both fluctuations or deliveries
    ## We fix these values by imputing around the mean of consumption around that point
    serie$first_difference[serie$first_difference > 0] <- NA
    serie$first_difference %<>% imputeTS::na.ma(., k = 2, weighting = "simple")

    ## Aggregation into daily bins
    serie %<>% as.data.frame
    res <- stats::aggregate(serie["first_difference"],
                            format(serie["datetime"], "%Y-%m-%d"),
                            function(x) -sum(x))
    res$datetime <- as.Date.character(res$datetime)
    colnames(res) <- c("datetime", "value")

    return(res)
}


tank_data_for_sitename <- function(telemetry_data, tank_info_client, sitename,
                                   tanks)
{
    telem_sub <- telemetry_data[SITENAME == sitename]

    ## Fix telemetry variable names
    modified_tel_tank_name <- FALSE
    if ("LIN_LEVEL_INCH" %in% tanks) {
        new_tel_tank <- "LIN_Level_Inch"
        modified_tel_tank_name <- TRUE
    }

    f <- function(tank)
    {
        if (modified_tel_tank_name) {
            telemetry_tank <- telem_sub[VARNAME == new_tel_tank]
        } else {
            telemetry_tank <- telem_sub[VARNAME == tank]
        }
        tank_info_sub_tank <- tank_info_client[TelemetryLevelVariable == tank &
                                               TelemetrySitename == sitename]

        return(Tank(tank, tank_info_sub_tank, telemetry_tank, sitename))
    }

    res <- lapply(tanks, f)
    names(res) <- NULL
    return(res)
}


## Client view data objects and functions
ClientData <- function(depot_number, telemetry_data, delivery_data,
                       sites_tanks, tank_info_client)
{
    ## Site and tank data for the sites_tanks linked list
    ## We can have multiple sites and 1 site can also have multiple tanks
    f <- pryr::partial(tank_data_for_sitename,
                       telemetry_data = telemetry_data,
                       tank_info_client = tank_info_client)

    ## When the case is a site with multiple tanks the output is a matrix with 1 column.
    ## In the case with multiple sites we return a list.
    ## Wrapping the result in `c` makes the output consistent in those cases.
    tanks_data <- c(mapply(f, sitename = names(sites_tanks), tanks = sites_tanks,
                           USE.NAMES = FALSE))


    ## Get the delivery data for the depot_number
    deliverie_data <- Delivery(delivery_data[DPNumber==depot_number], depot_number)

    ## Matching the time series
    matched <- match_deliverie_tanks(deliverie_data, tanks_data, depot_number) %>%
        plyr::compact()  # Should we fail to do a matching remove it from the result

    ## higher level information about client
    ## Even if there may be multiple sitenames for each depot_number, they share
    ## the same address.
    state <- tank_info_client[1, "State"]
    city <- tank_info_client[1, "City"]
    zip <- tank_info_client[1, "Zip"]

    # Some manual fixes for address names
    if ((depot_number == "140041") | (depot_number == "140043")) {
        address <- tank_info_client[1, "Address Line2"]
    } else if (depot_number == "12580") {
        address <- "911 Hansen Way"
    } else if (depot_number == "70350") {
        address <- tank_info_client[1, "Address Line2"]
    } else {
        address <- tank_info_client[1, "Address Line1"]
    }


    structure(
        list(
            DP = depot_number,
            state = as.character(state),
            city = as.character(city),
            zip = as.character(zip),
            address= as.character(address),
            delivery = deliverie_data,
            tank = tanks_data,
            matched = matched
        ),
        class = "Client"
    )
}


client_view_data <- function(sample_number = NULL)
{
    if (!exists("telemetry_data")) {
        print("Loading Telemetry Data")
        ##telemetry_data <- TelemetryData()
        telemetry_data <- readRDS(file = "../data/master/telemetry.rds")
    }
    telemetry_data$data %<>% data.table::as.data.table()

    if (!exists("delivery_data")) {
        print("Loading Delivery Data")
        ##delivery_data <- DeliveryData()
        delivery_data <- readRDS(file = "../data/master/delivery.rds")
    }
    delivery_data$data %<>% data.table::as.data.table()

    if (!exists("tank_info")) {
        print("Loading merge table")
        tank_info <- readxl::read_excel("../data/internship data/US Tank info.xlsx") %>%
            dplyr::mutate_all(function(x) ifelse(x == "NULL", NA, x)) %>%
            tidyr::drop_na(., TelemetrySitename, TelemetryLevelVariable, `Storage ERPCode`)

        ## Create our actual Depot Number column from the Storage ERPCode
        tank_info[['DPNumber']] <- sapply(tank_info[['Storage ERPCode']],
                                          split_erp_codes,
                                          USE.NAMES = FALSE)
    }
    tank_info %<>% data.table::as.data.table()


    ## We iterate over the Depot Numbers found in our merge table
    ## For each DPNumber, we obtain the Client data if we can find a matching sitename.
    ## If we cannot match a sitename, then we return NULL.
    depot_numbers <- unique(tank_info[['DPNumber']]) %>%
        ## Remove the depot numbers not in the delivery data
        intersect(., delivery_data$DPNumbers) %>%
        ## Remove depot_numbers in the blacklist
        setdiff(., blacklist)


    f <- pryr::partial(client_data_for_depot_number,
                       telemetry_data = telemetry_data,
                       delivery_data = delivery_data$data,
                       tank_info = tank_info)

    if (!is.null(sample_number)) {
        ## Create a consistent sample of the data
        actual_depot_numbers <- depot_numbers[1:sample_number]
    } else {
       actual_depot_numbers <- depot_numbers
    }

    print("Merging delivery and consumption")

    client_data <- lapply_pb(actual_depot_numbers, f) %>%
        plyr::compact()  #Remove NULL entries in the client_data list

    return(client_data)
}


client_data_for_depot_number <- function(depot_number, telemetry_data,
                                         delivery_data, tank_info)
{
    print(depot_number)
    tank_info_client <- tank_info[DPNumber==depot_number]
    sites_tanks <- sites_tanks_for_depot_subset(tank_info_client, telemetry_data$sites)

    ## If we didn't find a matching site for our depot number it will be an empty list
    if (length(sites_tanks) > 0) {
        return(ClientData(depot_number, telemetry_data$data, delivery_data,
                          sites_tanks, tank_info_client))
    } else {
        return(NULL)
    }
}


sites_tanks_for_depot_subset <- function(tank_info_client, telemetry_sites)
{
    sites <- list()
    for (i in 1:nrow(tank_info_client)) {
        sitename <- as.character(tank_info_client[i, "TelemetrySitename"])
        ## Check if the sitename is in our telemety_data sites
        if (sitename %in% telemetry_sites) {
            tank <- as.character(tank_info_client[i, "TelemetryLevelVariable"])
            sites[[sitename]] <- c(sites[[sitename]], tank)
        }
    }
    return(sites)
}


sites_tanks_for_depot_number <- function(depot_number, telemetry_data,
                                         delivery_data, tank_info)
{
    tank_info_subset <- tank_info[DPNumber==depot_number]
    sites_tanks <- sites_tanks_for_depot_subset(tank_info_subset, telemetry_data$sites)

    ## If we didn't find a matching site for our depot number it will be an empty list
    if (length(sites_tanks) > 0) {
        return(sites_tanks)
    } else {
        return(NULL)
    }
}


## cust_dps_from_tank_info_subset <- function(df)
## {
##     #' dp_nums is a nested list ordered : depot_number
##     #'                                        ▼
##     #'                                    sitename
##     #'                                        ▼
##     #'                                      tank

##     dp_nums = list()

##     for (i in 1:nrow(df)) {
##         depot_number <- as.character(split_erp_codes(df[i, "Storage ERPCode"]))
##         sitename <- as.character(df[i, "TelemetrySitename"])
##         tank <- df[i, "TelemetryLevelVariable"]

##         ## Append the tank if sitename is already in our list
##         if (sitename %in% names(dp_nums[[depot_number]])) {
##             dp_nums[[depot_number]][[sitename]] <- c(dp_nums[[depot_number]][[sitename]], tank)
##         } else {
##             ## Create a new site and append the whole list
##             new_site <- list()
##             new_site[[sitename]] <- tank
##             dp_nums[[depot_number]] <- c(dp_nums[[depot_number]], new_site)
##         }
##     }
##     return(dp_nums)
## }


split_erp_codes <- function(x)
{
    split <- strsplit(as.character(x), "_")
    return(split[[1]][1])
}


MatchedDelTel <- function(del, tel, depot_number, tanks_dp)
{
    #' Here tel variable is the deliveries calculated from the telemetry

    match_df <- match_time_series(del, tel, time_window = 24)
    cor_match <- cor_matched_time_series(match_df)
    best_start <- best_start_matching(match_df)
    best_end <- best_end_matching(match_df)

    ## If we best_start and best_end are identical or can't be found.
    ## best_start:best_end will have no length and give us an error
    ratio <- tryCatch({
        matching_ratio(match_df[best_start:best_end,])
    }, error = function(cond) {
        return(NA)
    })


    structure(
        list(
            df = match_df,
            ratio = ratio,
            cor = cor_match,
            start = best_start,
            end = best_end,
            length = best_end - best_start,
            dp = depot_number,
            tanks = tanks_dp
        ),
        class = "MatchedDelTel"
    )
}


telemetry_from_tanks <- function(tanks)
{
    if (length(tanks) == 1) {
        tel <- tanks[[1]]$telemetry.serie
    } else {
        ## We need to merge the telemetry deliveries from both tanks
        ## To do so we merge the raw telemetry from both tanks
        tel_list <- lapply(tanks, function(x) x$telemetry.serie)
        tel <- stats::aggregate(. ~ datetime, data.table::rbindlist(tel_list), sum)
    }
    return(tel)
}


match_deliverie_tanks <- function(delivery, tanks, depot_number)
{
    # Two cases to work with 1 del DP -> 1 tank, 1 del DP -> multiple tanks
    actual_del <- delivery_ts(delivery)

    tel <- telemetry_from_tanks(tanks)
    tel_del <- deliveries_from_telemetry(tel, threshold = 0, max_ratio = 1/6)

    tanks_dp <- lapply(tanks, function(x) x$id)

    if (is.null(tel_del)) {
        return(NULL)
    } else {
        return(MatchedDelTel(actual_del, tel_del, depot_number, tanks_dp))
    }
}


delivery_ts <- function(delivery)
{
    return(plyr::arrange(delivery$df[,c('ShiftRealStartDateTime',
                                        'DeliveredQuantity')], ShiftRealStartDateTime))
}


client_delivery_ts <- function(client, source = c("raw", "tel"))
{
    source <- match.arg(source)
    switch(source,
           raw = {
               serie <- as.data.frame(delivery_ts(client$delivery))
           }, tel = {
               serie <- client$matched$df[,1:2] %>%
                   tidyr::drop_na()
           })

    colnames(serie) <- c("datetime", "delivered")
    rownames(serie) <- NULL
    return(serie)
}


## client_delivery_xts <- function(client)
## {
##     #' How we decide to get the final delivery timeseries for a client.
##     #' We convert the hourly deliveries into daily bins
##     ts <- client_delivery_ts(client)
##     res <- aggregate(ts["DeliveredQuantity"],
##                      format(ts["ShiftRealStartDateTime"], "%Y-%m-%d"),
##                      sum) %>%
##         xts(x = .$DeliveredQuantity,
##             order.by = as.Date(as.character(.$ShiftRealStartDateTime)))
##     return(res)
## }


client_consumption_ts <- function(client)
{
    #' If the client has multiple tanks they are merged together
    consumption_list <- lapply(client$tank, function(x) x$consumption.serie)
    merged <- stats::aggregate(. ~ datetime, data.table::rbindlist(consumption_list), sum)
    return(merged)
}


## client_consumption_xts <- function(client)
## {
##     #' How we decide to get the final consumption timeseries for a client
##     ts <- client_consumption_ts(client)
##     merged_consumption_ts <- aggregate(. ~ date, ts, sum)
##     time_series <- xts(merged_consumption_ts$value, merged_consumption_ts$date)
##     return(time_series)
## }


#' Fetching function
fetch_serie_time <- function(client, serie = c("tel", "del", "con"), start, end)
{
    serie <- match.arg(serie)
    switch(serie,
           tel = subset(get_client_telemetry(client), datetime <= end & datetime >= start),
           del = subset(client$delivery$df, ShiftRealStartDateTime <= end & ShiftRealStartDateTime >= start)[,c("ShiftRealStartDateTime", "DeliveredQuantity")],
           con = subset(client_consumption_ts(client), datetime <= end & datetime > start))
}


get_client_product <- function(client)
{
    return(client$tank[[1]]$product)
}


filter_product <- function(l, products)
{
    is.product <- function(client, products)
    {
        #' Return True if the client's product is the product
        if (get_client_product(client) %in% products) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
    return(Filter(pryr::partial(is.product, products = products), l))
}


get_client_tank_unit <- function(client)
{
    return(client$tank[[1]]$unit)
}


filter_tank_unit <- function(l, units)
{
    is.unit <- function(client, units)
    {
        #' Return True if the client's product is the product
        if (get_client_tank_unit(client) %in% units) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
    return(Filter(pryr::partial(is.unit, units = units), l))
}


fetch_small_amounts <- function(client, serie = c("con", "del"), amount)
{
    serie <- match.arg(serie)
    switch(serie,
           del = subset(client$matched$df, DeliveredQuantity <= amount)[,c(1,2,3,4)],
           con = subset(client$matched$df, Amount <= amount)[,c(1,2,3,4)])
}


min_matched_amount <- function(client, serie = c("con", "del"))
{
    serie <- match.arg(serie)
    df <- client$match$df[complete.cases(client$match$df),]
    switch(serie,
           del = min(df$Amount),
           con = min(df$DeliveredQuantity))
}


filter_cvd <- function(cvd)
{
    res <- Filter(function(x) !is.null(x$matched), cvd) %>%
        filter_product(., c("LN2", "LO2", "LAR")) %>%
        Filter(function(x) !is.na(x$matched$ratio), .) %>%
        filter_tank_unit(., c("inH2O")) %>%
        filter_client_del_con_ratio(., 0.5, 1.5)
    return(res)
}


get_client_total_deliveries <- function(client, type)
{
    type <- match.arg(type, c("del", "tel"))
    tot <- switch(type,
                  tel = sum(client$matched$df$Amount, na.rm = TRUE),
                  del = sum(client$matched$df$DeliveredQuantity, na.rm = TRUE))
    return(tot)
}


client_del_con_ratio <- function(client)
{
    del <- get_client_total_deliveries(client, "tel")
    con <- sum(client_consumption_ts(client)$value)
    return(del/con)
}


filter_client_del_con_ratio <- function(cvd, low, high)
{
    res <- Filter(function(x) client_del_con_ratio(x) < high &&
                              client_del_con_ratio(x) > low,
                  cvd)
    return(res)
}


get_client_matching_ratio <- function(client)
{
    return(client$matched$ratio)
}


get_client_matching_cor <- function(client)
{
    return(client$matched$cor)
}


get_client_matching_length <- function(client)
{
    return(client$matched$length)
}


get_client_correlation <- function(client)
{
    return(client$matched$cor)
}


get_client_safety_level <- function(client, a = 0.2)
{
    return(a * sum(sapply(client$tank, function(x) x$max.level)))
}


get_client_safety_level_unit <- function(client)
{
    return(sapply(client$tank, function(x) x$max.level.unit))
}


get_client_telemetry <- function(client)
{
    return(telemetry_from_tanks(client$tank))
}


train_test_split <- function(l, n = 0.8)
{
    split <- round(length(l) * n)

    structure(
        list(
            train = seq(from = 1, to = split),
            test = seq(from = split + 1, to = length(l))
        )
    )
}


#' main
if (FALSE) {  # Prevents it from running when sourcing the file
    #' Data Analysis

    #' Are addreses the same for each tank that a client may have
    test_tank_address <- function(sitename, tank_info)
    {
        tank_info_client <- subset(tank_info, TelemetrySitename == sitename)
        address <- unique(tank_info_client[, "Address Line1"])
        if (length(address) > 1) {
            warning(cat("Different addresses for sitename ", as.character(sitename), "\n"))
            return(address)
        } else {
            return(NULL)
        }
    }

    f <- pryr::partial(test_tank_address, tank_info = tank_info)
    foo <- lapply(telemetry$sites, f) %>% compact

    ## These addresses need to be to be manually set
    subset(tank_info, TelemetrySitename == "TRIQUIN2_TX")

    ## Test for depot_numbers with multisites
    f <- pryr::partial(sites_tanks_for_depot_number,
                       telemetry_data = telemetry_data,
                       delivery_data = delivery_data,
                       tank_info = tank_info)

    client_data <- lapply_pb(depot_numbers, f) %>%
        plyr::compact()  #Remove NULL entries in the client_data list

    multisites <- lapply(client_data, function(x) if (length(x) > 1) x) %>% compact

    multitank <- lapply(client_data, function(x) if (length(x[[1]]) > 1) x) %>% compact

    get_address_site <- function(x, tank_info)
    {

        f <- function(y) {
            tank_info_site <- tank_info[TelemetrySitename==y]
            state <- tank_info_site[1, "State"]
            city <- tank_info_site[1, "City"]
            zip <- tank_info_site[1, "Zip"]
            address <- paste(state, city, zip)
            return(address)
        }
        return(length(unique(lapply(names(x), f))))
    }

    lapply(multisites, partial(get_sites, tank_info=tank_info))
}
