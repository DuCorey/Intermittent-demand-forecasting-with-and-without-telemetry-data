# Run code from main

#' Work directory
setwd("/home/corey/AL/code")

#' Soucing files
source("data.R")

#' Data objects
delivery <- DeliveryData()
delivery <- readRDS("../data/master/delivery.rds")
telemetry <- TelemetryData()
telemetry  <- readRDS("../data/master/telemetry.rds")
tank_info <- readxl::read_excel("../data/internship data/US Tank info.xlsx") %>%
    as.data.frame

sample <- client_view_data(sample_number = 100)

##cvd <- client_view_data()

#' Saving data objects
saveRDS(delivery, "../data/master/delivery.rds")
saveRDS(telemetry, "../data/master/telemetry.rds")
saveRDS(sample, "../data/master/client_sample.rds")
saveRDS(CVD, "../data/master/client.rds")


#' Testing the CVD data object
sitename <- "A_ADM_IL"
tank_info_subset <- subset(tank_info, TelemetrySitename == sitename)
tank_info_client <- tank_info_subset
dp_nums <- depot_numbers_from_sitename_tank_info_subset(tank_info_subset)

sitename <- "A_ARIZONA_GA"
sitename <- "DQ_GLOBALFABO_MS"
sitename <- "SAFEWAY13N_CA"
sitename <- "A_AJAX_MI"

deliveries_data <- deliveries_for_dp_nums(as.data.table(delivery$data), dp_nums)

telem_sub <- subset(telemetry$data, SITENAME == sitename)

tank_data <- tank_data_for_sitename(as.data.table(telem_sub), sitename, tank_info_subset)


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
