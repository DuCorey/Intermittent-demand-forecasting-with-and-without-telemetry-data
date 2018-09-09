# Run code from main

#' Work directory
setwd("/home/corey/AL/code")

#' Soucing files
source("data.R")

#' Repl options
options(max.print = 1000, error=recover)

#' Logging error messages
error <- file("error.txt")
sink(error)
file.show("error.txt")
## Unsink
sink()


#' Data objects
delivery_data <- DeliveryData()
delivery_data <- readRDS("../data/master/delivery.rds")
delivery_data$data %<>% as.data.table
telemetry_data <- TelemetryData()
telemetry_data <- readRDS("../data/master/telemetry.rds")
telemetry_data$data %<>% as.data.table

tank_info <- readxl::read_excel("../data/internship data/US Tank info.xlsx") %>%
    dplyr::mutate_all(function(x) ifelse(x == "NULL", NA, x)) %>%
    tidyr::drop_na(., TelemetrySitename, TelemetryLevelVariable, `Storage ERPCode`)
tank_info[['DPNumber']] <-  sapply(tank_info[['Storage ERPCode']],
                                   split_erp_codes,
                                   USE.NAMES = FALSE)
tank_info %<>% as.data.table

sample <- client_view_data(sample_number = 40)

cvd <- client_view_data()

#' Saving data objects
saveRDS(delivery_data, "../data/master/delivery.rds")
saveRDS(telemetry_data, "../data/master/telemetry.rds")
saveRDS(sample, "../data/master/client_sample.rds")
saveRDS(cvd, "../data/master/client.rds")

sample <- readRDS("../data/master/client_sample.rds")
cvd <- readRDS("../data/master/client.rds")

#' Testing the CVD data object
depot_number <- "140041"
depot_number <- "600881"
depot_number <- "600000"

tank_info_client <- tank_info[DPNumber==depot_number]
sites_tanks <- sites_tanks_for_depot_subset(tank_info_client, telemetry_data$sites)

## Site and tank
f <- pryr::partial(tank_data_for_sitename,
                   telemetry_data = telemetry_data$data,
                   tank_info_client = tank_info_client)

tanks_data <- c(mapply(f, sitename = names(sites_tanks), tanks = sites_tanks,
                       USE.NAMES = FALSE))

## Delivery data
deliverie_data <- Delivery(delivery_data$data[DPNumber==depot_number], depot_number)

## Matched
matched <- match_deliverie_tanks(deliverie_data, tanks_data, depot_number) %>%
    plyr::compact()  # Should we fail to do a matching remove it from the result

## What happens with the matching if there's nothing possible

## 0 correlation?


## Matching
deliveries <- sample[[48]]$delivery
tanks <- sample[[48]]$tank
dp_nums <- sample[[48]]$dp_nums
match_deliveries_tanks(deliveries, tanks, dp_nums)
length(match_deliveries_tanks(deliveries, tanks, dp_nums))
system.time(match_deliveries_tanks(deliveries, tanks, dp_nums))

## UID for tank info
length(unique(tank_info$TelemetrySitename))



#' Delivery Distribution
library("ggplot2")
deliveries <- do.call(rbind, lapply(CVD, function(x) x$delivery))

plot <- qplot(deliveries$DeliveredQuantity, geom="histogram")

ggsave("../code/histplot.png", plot = plot, device = png())


#' Error logging to file
all_out  <- file("all.Rout", open = "wt")
sink(all_out, type = "message")

close(all_out)
closeAllConnections()
readLines("all.Rout")
