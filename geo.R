#' Geolocation functions
library('ggmap')
library('maps')
library('mapdata')
library('ggplot2')

setwd("/home/corey/AL/code")

CVD <- readRDS("../data/master/client.rds")

sample <- readRDS("../data/master/client_sample.rds")

format_zip <- function(zip) {
    #' The zips are encoded as strings of floating numbers with .0
    #' This removes only gets the digits we want for the zip
    res <- strsplit(zip, "[.]")
    return(res[[1]][1])
}


get_address <- function(client) {
    client_info <- na.omit(c(client$address, client$city, client$state, format_zip(client$zip)))
    return(paste(client_info, collapse = ", "))
}

init_latlon <- function(client_list) {
    address <- lapply(client_list, get_address)

    f <- pryr::partial(ggmap::geocode, output = "latlon")
    lat_lon <- lapply(address, f)

    geo_df <- data.frame(uid=unlist(lapply(client_list, . %>% .$UID)),
                         address=unlist(address),
                         lon=unlist(lapply(lat_lon, .%>% .$lon)),
                         lat=unlist(lapply(lat_lon, .%>% .$lat)))
    return(geo_df)
}

update_latlon <- function(df) {
    for (ind in which(!complete.cases(df))) {
        address <- as.character(df[ind, 'address'])
        res <- ggmap::geocode(address, output = "latlon")
        df[ind, 'lon'] <- res$lon
        df[ind, 'lat'] <- res$lat
    }
    return(df)
}

missing_geo <- function(geo_df) {
    return(geo_df[which(!complete.cases(geo_df)),])
}


## sample_geo <- init_latlon(sample)
## missing_geo(sample_geo)
## sample_geo %<>% update_latlon
## saveRDS(sample_geo, "../data/master/sample_geo.rds")

cvd_geo <- init_latlon(cvd)
missing_geo(cvd_geo)
cvd_geo %<>% update_latlon
saveRDS(cvd_geo, "../data/master/cvd_geo.rds")

geo_plot <- function(df) {
    usa <- ggplot2::map_data("usa")
    ggplot() +
        geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
        geom_point(data = lat_lon, aes(x=lon, y = lat), color = "red", size = 1) +
        coord_fixed(1.3)
 }


## map <- get_map(location = center, zoom = 4, color = "bw")
## map.plot <- ggmap(map)
## map.plot <- map.plot + geom_point(data = lat_lon, aes(x = lon, y = lat, colour = "red"), show.legend = F, size = 2)

geo_plot(cvd_geo)

ggsave("../code/USplot.png", plot = map.plot, device = png())
