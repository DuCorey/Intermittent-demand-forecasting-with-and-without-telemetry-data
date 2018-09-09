#' title: data.R
#' comments: this file creates the data objects necessary for
#' author: Corey Ducharme / corey.ducharme@polymtl.ca
#' input: the excel files which contain the deliveries stored in the report folder
#' output: a csv which contains the deliveries for all clients for all years

#' Work directory
setwd("/home/corey/AL/code")

#' packages
library(plyr) # load plyr before dplyr
library(dplyr)
library(pryr)
library(tidyr)
library(magrittr)
library(readxl)
library(data.table)
library(lubridate)
library(xts)


#' functions
source("utils.R")
source("matching.R")

"%o%" <- pryr::compose

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

    ## Clean data
    ## TODO
    ## Speed up subsetting using data.table
    merged_data %<>% data.table::as.data.table() %>%
        ## Remove rows with 0 delivered quantity
        .[DeliveredQuantity > 0]


    # Determine unique client depot numbers
    dps <- unique(merged_data[["DPNumber"]])


    structure(
        list(
            sources = sources,
            files = files,
            data = data.table::as.data.table(merged_data),
            DPNumbers = dps
        ),
        class="DeliveryData"
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


    ## The time series
    consumption_serie <- convert_to_daily_consumption(telemetry_subset)
    telemetry_serie <- telemetry_subset[, c("DATETIME", "CUSTOMERVALUE")] %>%
        plyr::arrange(., DATETIME)
    colnames(telemetry_serie) <- c("datetime", "level")
    rownames(telemetry_serie) <- NULL


    structure(
        list(
            id = as.character(tank),
            sitename = sitename,
            unit = as.character(unit),
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


convert_to_daily_consumption <- function(df)
{
    ## Currently the first day may not be complete.
    ## Impact should be minor though.

    df$first_difference <- c(0, diff(df$CUSTOMERVALUE))

    ## We ignore the timezones in the date creation
    df$date <- as.Date(df$DATETIME)
    consumption <- aggregate(first_difference ~ date, df,
                             FUN = function(x) -sum(x[which(x<0)]))
    colnames(consumption) <- c("date", "value")
    return(consumption)
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
        intersect(., delivery_data$DPNumbers)

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


match_deliverie_tanks <- function(delivery, tanks, depot_number)
{
    # Two cases to work with 1 del DP -> 1 tank, 1 del DP -> multiple tanks
    actual_del <- delivery_ts(delivery)

    if (length(tanks) == 1) {
        ## Case: 1 del -> 1 tank
        tel_del <- deliveries_from_telemetry(tanks[[1]]$telemetry.serie)
    } else {
        ## Case: 1 del -> multiple tanks
        ## We need to merge the telemetry deliveries from both tanks
        ## To do so we merge the raw telemetry from both tanks
        tel_list <- lapply(tanks, . %>% .$telemetry.serie)
        merged_tel <- aggregate(. ~ datetime, data.table::rbindlist(tel_list), sum)
        tel_del <- deliveries_from_telemetry(merged_tel)
    }

    tanks_dp <- lapply(tanks, . %>% .$id)

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


client_consumption_ts <- function(client)
{
    #' How we decide to get the final consumption timeseries for a client
    #' If the client has multiple tanks, they are merged together.

    consumption_ts_list <- lapply(client$tank, . %>% .$consumption.serie)
    merged_consumption_ts <- aggregate(. ~ date, data.table::rbindlist(consumption_ts_list), sum)
    time_series <- xts(merged_consumption_ts$value, merged_consumption_ts$date)
    return(time_series)
}


if (FALSE) {  # Prevents it from running when sourcing the file
    #' Main
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
