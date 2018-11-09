source("data.R")

conver_LAR <- readRDS("../data/master/conver_LAR.rds")
conver_LO2 <- readRDS("../data/master/conver_LO2.rds")
conver_LN2 <- readRDS("../data/master/conver_LN2.rds")


## TODO - the reverse of everything

convert_pressure_to_delivery <- function(client)
{
    cor <- get_client_matching_cor(client)
    product <- get_client_product(client)
    df <- client$matched$df
    match_length <- get_client_matching_length(client)

    conver <- switch(product,
                     LAR = conver_LAR,
                     LO2 = conver_LO2,
                     LN2 = conver_LN2)
    if (cor >= conver$cor & match_length >= 1 & !is.na(cor)) {
        model <- lm(log(DeliveredQuantity) ~ log(Amount), data = df, na.action = na.exclude)
    } else {
        model <- conver$model
    }

    df <- tidyr::drop_na(df, Amount)
    ind <- is.na(df$DeliveredQuantity)

    df$Amount[!ind] <- df$DeliveredQuantity[!ind]
    df$Amount[ind] <- round(exp(predict(model, data.frame(Amount = df$Amount[ind]))))

    serie <- df[,1:2]
    return(serie)
}


if (FALSE) {
    cvd <- readRDS("../data/master/client.rds") %>%
        filter_cvd

    sapply(cvd, get_client_correlation)
    table(sapply(cvd, get_client_tank_unit))
    table(sapply(cvd, get_client_product))
    unique(sapply(cvd, get_client_product))

    f <- function(x) x$matched$df[complete.cases(x$matched$df),
                                  c("Amount", "DeliveredQuantity")]

    ## Make the conversion models
    product <- "LN2"
    merged <- data.table::rbindlist(lapply(filter_product(cvd, product), f))
    cor <- cor(merged$DeliveredQuantity, merged$Amount, method = "pearson")
    model <- lm(log(DeliveredQuantity) ~ log(Amount), merged)
    res <- list(cor = cor, model = model)
    saveRDS(res, paste("../data/master/conver_", product, ".rds", sep = ""))

    product <- "LO2"
    merged <- data.table::rbindlist(lapply(filter_product(cvd, product), f))
    cor <- cor(merged$DeliveredQuantity, merged$Amount, method = "pearson")
    model <- lm(log(DeliveredQuantity) ~ log(Amount), merged)
    res <- list(cor = cor, model = model)
    saveRDS(res, paste("../data/master/conver_", product, ".rds", sep = ""))

    product <- "LAR"
    merged <- data.table::rbindlist(lapply(filter_product(cvd, product), f))
    cor <- cor(merged$DeliveredQuantity, merged$Amount, method = "pearson")
    model <- lm(log(DeliveredQuantity) ~ log(Amount), merged)
    res <- list(cor = cor, model = model)
    saveRDS(res, paste("../data/master/conver_", product, ".rds", sep = ""))


}
