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


#' functions
source("utils.R")
source("matching.R")

"%o%" <- pryr::compose

read_csv_skip_second_line <- function(file) {
    data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
    return(data[-1,])
}

fix_raw_delivery_column_names <- function(data) {
    colnames(data)[which(names(data) == "ExternalDeliveryNote")] <- "ExtDeliveryNote"
    return(data)
}

### data objects
## Delivery data
DeliveryData <- function() {
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

    #' Convert strings to dates
    #' First convert empty strings into NA so that the conversion to POSIXct can work
    merged_data$RealStartDateTime[merged_data$RealStartDateTime == ""]           <- NA
    merged_data$ShiftRealStartDateTime[merged_data$ShiftRealStartDateTime == ""] <- NA
    merged_data$ConfirmationDate[merged_data$ConfirmationDate == ""]             <- NA

    merged_data$RealStartDateTime      %<>% as.POSIXct(., tz = "America/New_York")
    merged_data$ShiftRealStartDateTime %<>% as.POSIXct(., tz = "America/New_York")
    merged_data$ConfirmationDate       %<>% as.POSIXct(., tz = "America/New_York")

    #' Clean data
    #' TODO
    #' Speed up subsetting using data.table
    merged_data %<>% data.table::as.data.table() %>%
        #' Remove rows with 0 delivered quantity
        .[DeliveredQuantity > 0]


    # Determine unique client depot numbers
    dps = unique(merged_data[["DPNumber"]])


    structure(
        list(
            sources = sources,
            files = files,
            data = merged_data,
            DPNumbers = dps
        ),
        class="DeliveryData"
    )
}


print.DeliveryData <- function(x, ...) {
    invisible(x)
}

summary.DeliveryData <- function(x, ...) {
    summary(x$data)
}

head.DeliveryData <- function(x, ...) {
    head(x$data)
}


deliveries_for_dp_nums <- function(delivery_data, dp_nums) {

    f <- function(key, value) {
         delivery_subset <- delivery_data[DPNumber == key]
         return(Delivery(delivery_subset, key, value))
    }

    res <- mapply(f, key=names(dp_nums), value=dp_nums, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    return(res)
}


Delivery <- function(df, dp_num, tanks) {
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
            df = data,
            tanks = tanks
        ),
        class = "Delivery"
    )
}

## Telemetry data
TelemetryData <- function() {
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
            data = data,
            sites = sites
        ),
        class = "TelemetryData"
    )
}

print.TelemetryData <- function(x, ...) {
    invisible(x)
}

summary.TelemetryData <- function(x, ...) {
    summary(x$data)
}

head.TelemtryData <- function(x, ...) {
    head(x$data)
}


## Tank level data
tank_data_for_sitename <- function(telem_sub, telemetry_sitename, tank_info_client, dp_nums) {
    modified_tank_info_name <- FALSE

    ## The tanks we have found in our tank_info
    tank_info_tanks <- dp_nums

    ## Fix telemetry variable names
    if ("LIN_LEVEL_INCH" %in% tank_info_tanks) {
        tank_info_tanks[tank_info_tanks=="LIN_LEVEL_INCH"] = "LIN_Level_Inch"
        modified_tank_info_name <- TRUE
        og_tank <- "LIN_LEVEL_INCH"
    }

    telem_tanks <- sapply(unique(telem_sub[, "VARNAME"]), as.character)

    if (setequal(tank_info_tanks, telem_tanks)) {

        f <- function(tank) {
            telemetry_tank <- telem_sub[VARNAME == tank]

            if (modified_tank_info_name) {
                tank_info_sub_tank <- subset(tank_info_client,
                                             TelemetryLevelVariable == og_tank)
            } else {
                tank_info_sub_tank <- subset(tank_info_client,
                                             TelemetryLevelVariable == tank)
            }

            res <- Tank(tank, tank_info_sub_tank, telemetry_tank)
            return(res)
        }

        res <- lapply(telem_tanks, f)
        names(res) <- NULL
        return(res)
    } else {
        warning(cat("Mismatched telemetry variable names for site ", telemetry_sitename, "\n"))
        return(NULL)
    }
}


Tank <- function(tank, tank_info_sub_tank, telemetry_subset) {
    ## Tank information
    product <- tank_info_sub_tank[, "Product"]
    max_level <- tank_info_sub_tank[, "MaxLevel"]
    max_level_unit <- tank_info_sub_tank[, "Abbreviation__1"]
    orientation <- tank_info_sub_tank[, "Name"]
    capacity <- tank_info_sub_tank[, "Capacity"]
    capacity_unit <- tank_info_sub_tank[, "Abbreviation"]
    timezone <- telemetry_subset[1, "TIMEZONE"]
##    varname <- telemetry_subset[1, "VARNAME"]
    unit <- telemetry_subset[1, "CUSTOMERUNIT"]
    rtu <- telemetry_subset[1, "RTUTYPE"]


    ## The time series
    consumption_serie <- convert_to_daily_consumption(telemetry_subset)
    telemetry_serie <- telemetry_subset[, c("DATETIME", "CUSTOMERVALUE")] %>%
        arrange(., DATETIME)
    colnames(telemetry_serie) <- c("datetime", "level")
    rownames(telemetry_serie) <- NULL


    structure(
        list(
            id = as.character(tank),
            unit = as.character(unit),
            product = as.character(product),
            max.level = as.numeric(max_level),
            max.level.unit = as.character(max_level_unit),
            orientation = as.character(orientation),
            capacity = as.numeric(capacity),
            capacity.unit = as.character(capacity_unit),

            timezone = as.numeric(timezone),
##            varname = as.character(varname),
            unit = as.character(unit),
            rtu = as.character(rtu),

            telemetry.serie = as.data.frame(telemetry_serie),
            consumption.serie = as.data.frame(consumption_serie)
        ),
        class = "Tank"
    )
}


convert_to_daily_consumption <- function(df) {
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


## Client view data objects and functions
ClientData <- function(telemetry_data, delivery_data, telemetry_sitename,
                       dp_nums, tank_info_client) {
    ## Get the delivery data for the dp_nums
    deliveries_data <- deliveries_for_dp_nums(delivery_data$data, dp_nums)

    ## Subset telemetry data
    telem_sub <- telemetry_data$data[SITENAME == telemetry_sitename]

    ## Tank data for the telemetry subset
    tanks_data <- tank_data_for_sitename(telem_sub,
                                        telemetry_sitename,
                                        tank_info_client,
                                        dp_nums)

    ## Matching the time series
    matched <- match_deliveries_tanks(deliveries_data, tanks_data, dp_nums)

    ## higher level information about the client
    country <- telem_sub[[1, "COUNTRY"]]
    state <- tank_info_client[1, "State"]
    city <- tank_info_client[1, "City"]
    zip <- tank_info_client[1, "Zip"]

    # Some manual fixes for address names
    if (telemetry_sitename == "TRIQUIN2_TX") {
        address <- "500 W Renner Rd"
    } else if (telemetry_sitename == "VARIANPA_CA") {
        address <- "911 Hansen Way"
    } else if (telemetry_sitename == "A_ALLISON_IN") {
        addres  <- tank_info_client[1, "Address Line2"]
    } else {
        address <- tank_info_client[1, "Address Line1"]
    }


    structure(
        list(
            UID = paste(telemetry_sitename,
                        paste(names(dp_nums), collapse = "_"),
                        sep = "_"),
            sitename = as.character(telemetry_sitename),
            dp_nums = dp_nums,
            country = as.character(country),  # requires unlist because of data.table
            state = as.character(state),
            city = as.character(city),
            zip = as.character(zip),
            address= as.character(address),
            delivery = deliveries_data,
            tank = tanks_data
        ),
        class = "Client"
    )
}


client_view_data <- function(sample_number = NULL, ...) {
    print("Loading Telemetry Data")
    ##telemetry_data <- TelemetryData()
    telemetry_data <- readRDS(file = "../data/master/telemetry.rds")
    telemetry_data$data %<>% data.table::as.data.table()

    print("Loading Delivery Data")
    ##delivery_data <- DeliveryData()
    delivery_data <- readRDS(file = "../data/master/delivery.rds")
    delivery_data$data %<>% data.table::as.data.table()

    print("Loading merge table")
    tank_info <- readxl::read_excel("../data/internship data/US Tank info.xlsx") %>%
        dplyr::mutate_all(function(x) ifelse(x == "NULL", NA, x)) %>%
        as.data.frame

    ## Actual telemetry sites in our merge table
    tank_info_telemetry_sites <- unique(tank_info$TelemetrySitename)


    ## We iterate over the telemetry sitenames in our telemetry data
    ## For each site, we obtain the Client data should we are able to match the
    ## sitename to DPNumber in our deliveries. If we cannot match a DPNumber,
    ## then we return NULL.

    f <- pryr::partial(client_data_for_telemetry_sitename,
                       telemetry_data = telemetry_data,
                       delivery_data = delivery_data,
                       tank_info = tank_info,
                       tank_info_telemetry_sites = tank_info_telemetry_sites)

    if (!is.null(sample_number)) {
        ## Create a consistent sample of the data
        data_telemetry_sites <- telemetry_data$sites[1:sample_number]
    } else {
       data_telemetry_sites <- telemetry_data$sites
    }

    print("Merging delivery and consumption")

    client_data <- lapply_pb(data_telemetry_sites, f) %>%
        plyr::compact()  #Remove NULL entries in the client_data list

    return(client_data)
}


client_data_for_telemetry_sitename <- function(telemetry_data, delivery_data,
                                               telemetry_sitename, tank_info,
                                               tank_info_telemetry_sites) {
    ## First check if our telemetry sitanem is in our tank info merge table
    if (telemetry_sitename %in% tank_info_telemetry_sites) {

        tank_info_subset <- subset(tank_info,
                                   TelemetrySitename == telemetry_sitename)
        dp_nums <- depot_numbers_from_sitename_tank_info_subset(tank_info_subset)

        ## Check if the depot numbers are in our delivery data
        dp_nums <- dp_nums[names(dp_nums) %in% delivery_data$DPNumbers]

        if (length(dp_nums) > 0) {
            ## Both conditions are met, we then return the data object for a client
            return(ClientData(telemetry_data, delivery_data, telemetry_sitename,
                              dp_nums, tank_info_subset))
        } else {
            return(NULL)
        }
    } else {
        return(NULL)
    }
}


depot_numbers_from_sitename_tank_info_subset <- function(df) {
    dp_nums = list()

    for (i in 1:nrow(df)) {
        erp_code <- as.character(split_erp_codes(df[i, "Storage ERPCode"]))
        tank <- df[i, "TelemetryLevelVariable"]
        ## append to the list the tank
        dp_nums[[erp_code]] <- c(dp_nums[[erp_code]], tank)
    }
    return(dp_nums)
}


split_erp_codes <- function(x) {
    split <- strsplit(x, "_")
    return(split[[1]][1])
}


MatchedDelTel <- function(del, tel, del_dp, tanks_dp) {
    #' Here tel variable is the deliveries calculated from the telemetry

    match_df <- match_time_series(del, tel)
    cor_match <- cor_matched_time_series(match_df)
    best_start <- best_start_matching(match_df)
    best_end <- best_end_matching(match_df)
    ratio <- matching_ratio(match_df[best_start:best_end,])


    structure(
        list(
            df = match_df,
            ratio = ratio,
            cor = cor_match,
            start = best_start,
            end = best_end,
            length = best_end - best_start,
            dp = del_dp,
            tanks = tanks_dp
        ),
        class = "MatchedDelTel"
    )
}


match_deliveries_tanks <- function(deliveries, tanks, dp_nums) {
    ## Two cases to work with 1 del DP -> 1 tank, 1 del DP -> multiple tanks

    f <- function(del_dp, tanks_dp) {
        actual_del <- delivery_ts(delivery_for_dp(del_dp, deliveries))

        if (length(tanks_dp) == 1) {
            ## Case: 1 del -> 1 tank
            tel_del <- deliveries_from_telemetry(tank_for_id(tanks_dp, tanks)$telemetry)
        } else {
            ## Case: 1 del -> multiple tanks
            ## We need to merge the telemetry deliveries from both tanks
            ## To do so we merge the raw telemetry from both tanks
            f2 <- pryr::partial(tank_for_id, tanks=tanks)
            tel_list <- lapply(tanks_dp, . %>% .$telemetry %o% f2)
            merged_tel <- aggregate(. ~ datetime, data.table::rbindlist(tel_list), sum)
            tel_del <- deliveries_from_telemetry(merged_tel)
        }
        return(MatchedDelTel(actual_del, tel_del, del_dp, tanks_dp))
    }
    return(mapply(f, del_dp=names(dp_nums), tanks_dp=dp_nums, SIMPLIFY = FALSE, USE.NAMES = FALSE))
}


delivery_for_tank <- function(tank, client) {
    for (delivery in client$delivery) {
        if (tank$id %in% delivery$tanks) {
            return(delivery)
        }
    }
}


delivery_for_dp <- function(dp, deliveries) {
    for(delivery in deliveries) {
        if (dp == delivery$DPNumber) {
            return(delivery)
        }
    }
}


tank_for_id <- function(id, tanks) {
    for (tank in tanks) {
        if (id == tank$id) {
            return(tank)
        }
    }
}


tanks_for_delivery <- function(delivery, client) {
    del_dp <- delivery$DPNumber
    dp_nums <- client$dp_nums
    tanks <- dp_nums[names(dp_nums)==del_dp]

    f <- function(tank) {
        if (tank$id %in% tanks) {
            return(tank)
        } else {
            return(NULL)
        }
    }

    res <- lapply(client$tank, f) %>%
        plyr::compact()

    return(res)
}


delivery_ts <- function(delivery) {
    return(delivery$df[,c('ShiftRealStartDateTime', 'DeliveredQuantity')])
}


cvd_dp_nums <- function(cvd) {
     return(lapply(cvd, . %>% .$dp_nums))
}


if (FALSE) {  # Prevents it from running when sourcing the file
    #' Main
    #' Data Analysis

    #' Are addreses the same for each tank that a client may have
    test_tank_address <- function(sitename, tank_info) {
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
}
