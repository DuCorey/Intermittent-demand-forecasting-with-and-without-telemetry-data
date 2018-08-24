# Run code from main

#' Work directory
setwd("/home/corey/AL/code")

#' Soucing files
source("data.R")

#' Repl options
options(max.print = 1000)

#' Data objects
delivery_data <- DeliveryData()
delivery_data <- readRDS("../data/master/delivery.rds")
telemetry_data <- TelemetryData()
telemetry_data <- readRDS("../data/master/telemetry.rds")

tank_info <- readxl::read_excel("../data/internship data/US Tank info.xlsx") %>%
    dplyr::mutate_all(function(x) ifelse(x == "NULL", NA, x)) %>%
    as.data.frame

sample <- client_view_data(sample_number = 100)

cvd <- client_view_data()

#' Saving data objects
saveRDS(delivery_data, "../data/master/delivery.rds")
saveRDS(telemetry_data, "../data/master/telemetry.rds")
saveRDS(sample, "../data/master/client_sample.rds")
saveRDS(CVD, "../data/master/client.rds")


#' Testing the CVD data object
sitename <- "A_MEDJHSN_MI"
sitename <- "A_ADM_IL"
sitename <- "A_ALLISON_IN"
sitename <- "A_BWCS_KY"

tank_info_subset <- subset(tank_info, TelemetrySitename == sitename)
dp_nums <- depot_numbers_from_sitename_tank_info_subset(tank_info_subset)
dp_nums <- dp_nums[names(dp_nums) %in% delivery$DPNumbers]

deliveries_data <- deliveries_for_dp_nums(as.data.table(delivery_data$data), dp_nums)
telem_sub <- subset(telemetry_data$data, SITENAME == sitename)
tanks_data <- tank_data_for_sitename(as.data.table(telem_sub), sitename, tank_info_subset, dp_nums)
matched <- match_deliveries_tanks(deliveries_data, tanks_data, dp_nums)

## What happens with the matching if there's nothing possible



## Matching
deliveries <- sample[[48]]$delivery
tanks <- sample[[48]]$tank
dp_nums <- sample[[48]]$dp_nums
match_deliveries_tanks(deliveries, tanks, dp_nums)
length(match_deliveries_tanks(deliveries, tanks, dp_nums))
system.time(match_deliveries_tanks(deliveries, tanks, dp_nums))


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
