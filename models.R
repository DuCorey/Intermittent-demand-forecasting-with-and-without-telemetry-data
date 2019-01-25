#' title: models.R
#' comments: Demand and consumption models
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' pacakges

#' imports
source("smooth.R")
source("error.R")
source("series.R")

#'functions

all_models <- function(con, del, asact_time, asact_init_serie, max_bin = "auto", error_type = "rmse", norm = FALSE)
{
    if (norm) {
        ## Normalize data
        foo <- scale_mult(con, del)
        con <- foo[[1]]
        del <- foo[[2]]
    }

    ## Match the ends
    foo <- match_ends(con, del, "end")
    con <- foo[[1]]
    del <- foo[[2]]

    if (max_bin == "auto") {
        max_bin <- floor(length(del) / 2)
    }
    bins <- 1:max_bin

    ## Model 1 - SMA ADIDA
    series_1 <- lapply(bins, function(x) ADIDA(del, x, type = "SMA"))

    ## Model 2 - EMA ADIDA
    series_2 <- lapply(bins, function(x) ADIDA(del, x, type = "EMA"))

    ## Model 3 - Croston
    serie_3 <- my_croston(del) %>%
        normalize_relative(., del)

    ## Model 4 - ASACT
    serie_4 <- ASACT(asact_init_serie, asact_time) %>%
        normalize_relative(., del)
    foo <- match_ends(con, serie_4, "end")
    serie_4 <- foo[[2]]

    ## Model 5 - Aggregation
    serie_5 <- my_agg(series_1)

    ## Match the lengths of the series for all the models
    min_length <- min(lengths(series_1), lengths(series_2), length(serie_3),
                      length(serie_4))

    series_2 <- lapply(series_2, function(x) trim_ts(x, length(x) - min_length, "start"))
    serie_3 <- trim_ts(serie_3, length(serie_3) - min_length, "start")
    serie_4 <- trim_ts(serie_4, length(serie_4) - min_length, "start")
    con <- trim_ts(con, length(con) - min_length, "start")

    ## Calculate the errors
    errors_1 <- sapply(series_1, error_calc, x = con, type = error_type)
    errors_2 <- sapply(series_2, error_calc, x = con, type = error_type)
    errors_3 <- sapply(serie_3, error_calc, x = con, type = error_type)
    errors_4 <- sapply(serie_4, error_calc, x = con, type = error_type)

    opt <- which.min(c(min(errors_1), min(errors_2), errors_3, errors_4))
    opt <- switch(as.character(opt),
                  "1" = "ADIDA - SMA",
                  "2" = "ADIDA - EMA",
                  "3" = "Croston",
                  "4" = "ASACT")

    ## We try to find an quasi optimal value. The first value close to the true minimum.
    sma_min <- which(abs(errors_1 - min(errors_1)) < 0.05 * (max(errors_1) - min(errors_1)))[[1]]
    ema_min <- which(abs(errors_2 - min(errors_2)) < 0.05 * (max(errors_2) - min(errors_2)))[[1]]
    q_opt <- which.min(c(errors_1[[sma_min]], errors_2[[ema_min]], errors_3, errors_4))
    q_opt <- switch(as.character(q_opt),
                    "1" = "ADIDA - SMA",
                    "2" = "ADIDA - EMA",
                    "3" = "Croston",
                    "4" = "ASACT")


    structure(
        list(
            con = con,
            del = del,
            SMA = list(error = errors_1, opt = which.min(errors_1), q_opt = sma_min),
            EMA = list(error = errors_2, opt = which.min(errors_2), q_opt = sma_min),
            Croston = errors_3,
            ASACT = errors_4,
            asact_time = asact_time,
            asact_init_serie = asact_init_serie,
            opt = opt,
            q_opt = q_opt,
            error_type = error_type
        ),
        class = "ErrorModel"
    )
}


get_error_model <- function(error, model, opt = "opt", agg_level = NULL)
{
    model <- match.arg(model, c("ADIDA - SMA", "ADIDA - EMA", "Croston", "ASACT"))
    opt <- match.arg(opt, c("opt", "q_opt"))
    del <- error$del

    switch(model,
           "ADIDA - SMA" = {
               if (is.null(agg_level)) {
                   agg_level <- error$SMA[[opt]]
               }
               serie <- ADIDA(del, agg_level, "SMA")
               err <- error$SMA$error[agg_level]
           }, "ADIDA - EMA" = {
               if (is.null(agg_level)) {
                   agg_level  <- error$EMA[[opt]]
               }
               serie <- ADIDA(del, agg_level, "EMA")
               err <- error$EMA$error[agg_level]
           }, "Croston" = {
               serie <- my_croston(del) %>%
                   normalize_relative(., del)
               err <- error$Croston
           }, "ASACT" = {
               serie <- ASACT(asact_init_serie, asact_time) %>%
                   normalize_relative(., del)
               err <- error$ASACT
           })

    serie <- trim_ts(serie, length(serie) - length(error$con), "start")

    ## Test to make sure the conversion is correct
    check_err <- error_calc(error$con, serie, type = error$error_type)
    if (check_err != err) {
        stop("Error producing best serie from model.")
    }
    return(serie)
}


optimal_error <- function(error, opt)
{
    opt <- match.arg(opt, c("opt", "q_opt"))
    switch(error[[opt]],
           "ADIDA - SMA" = {
               agg_level <- error$SMA[[opt]]
               err <- error$SMA$error[agg_level]
           }, "ADIDA - EMA" = {
               agg_level  <- error$EMA[[opt]]
               err <- error$EMA$error[agg_level]
           }, "Croston" = {
               err <- error$Croston
           }, "ASACT" = {
               err <- error$ASACT
           })
    return(err)
}


best_model_from_error <- function(error, opt = "opt")
{
    opt <- match.arg(opt, c("opt", "q_opt"))
    opt_model <- error[[opt]]

    return(get_error_model(error, opt_model, opt))
}


filter_model <- function(l, model)
{
    is.model <- function(x, model)
    {
        if (x$opt == model) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }

    return(Filter(pryr::partial(is.model, model = model), l))
}


sum_errors <- function(a, b)
{
    sma <- a$SMA$error + b$SMA$error
    ema <- a$EMA$error + b$EMA$error
    croston <- a$Croston + b$Croston
    asact <- a$ASACT + b$ASACT

    opt <- which.min(c(min(sma), min(ema), croston, asact))
    opt <- switch(as.character(opt),
                  "1" = "ADIDA - SMA",
                  "2" = "ADIDA - EMA",
                  "3" = "Croston",
                  "4" = "ASACT")

    ## We try to find an quasi optimal value. The first value close to the true minimum.
    sma_qmin <- which(abs(sma - min(sma)) < 0.05 * (max(sma) - min(sma)))[[1]]
    ema_qmin <- which(abs(ema - min(ema)) < 0.05 * (max(ema) - min(ema)))[[1]]
    q_opt <- which.min(c(sma[[sma_qmin]], ema[[ema_qmin]], croston, asact))
    q_opt <- switch(as.character(q_opt),
                    "1" = "ADIDA - SMA",
                    "2" = "ADIDA - EMA",
                    "3" = "Croston",
                    "4" = "ASACT")


    structure(
        list(
            SMA = list(error = sma, opt = which.min(sma), q_opt = sma_qmin),
            EMA = list(error = ema, opt = which.min(ema), q_opt = ema_qmin),
            Croston = croston,
            ASACT = asact,
            opt = opt,
            q_opt = q_opt
        ),
        class = "ErrorModel"
    )
}


#' main
if (FALSE) {
    ## Static consumption
    del <- clus_tel_day$del$orig[[1]]
    con <- clus_tel_day$con$orig[[1]]
    asact_init_serie <- clus_tel_day$del$orig[[1]]
    asact_time = "daily"

    ## Dynamic consumption
    del <- clus_tel_day$del$orig[[5]]
    con <- clus_tel_day$con$orig[[5]]
    asact_init_serie  <- clus_tel_day$del$orig[[5]]



    ## Daily model
    foo_rmse <- all_models(con, del, asact_time, asact_init_serie, error_type = "rmse")
    foo_mae <- all_models(con, del, asact_time = "daily", asact_init_serie, error_type = "mae")

    bar <- best_model_from_error(foo_rmse, "opt")

    plot_error_model(foo_mae)
    plot_mult_xts(bar)

    ## Weekly model
    all_models(clus_tel_week$con$orig[[5]], clus_tel_week$del$orig[[5]],
               asact_time = "weekly", asact_init_serie = clus_tel_day$del$orig[[5]])

    con <- clus_tel_week$con$orig[[5]]
    del <- clus_tel_week$del$orig[[5]]

    asact_time = "weekly"
    asact_init_serie = clus_tel_day$del$orig[[5]]


    foo <- match_ends(con, del, "end")
    con <- foo[[1]]
    del <- foo[[2]]

    ## Model 1 - SMA ADIDA
    max_bin <- floor(length(del) / 2)
    bins <- 1:max_bin
    series_1 <- lapply(bins, function(x) ADIDA(del, x, type = "SMA"))

    ## Model 2 - EMA ADIDA
    series_2 <- lapply(bins, function(x) ADIDA(del, x, type = "EMA"))

    ## Model 3 - Croston
    serie_3 <- my_croston(del) %>%
        normalize_relative(., del)

    ## Model 4 - ASACT
    serie_4 <- ASACT(asact_init_serie, asact_time) %>%
        normalize_relative(., del)

    foo <- match_ends(con, serie_4, "end")
    serie_4 <- foo[[2]]

    min_length <- min(lengths(series_1), lengths(series_2), length(serie_3),
                      length(serie_4), length(con))
    series_1 <- lapply(series_1, function(x) trim_ts(x, length(x) - min_length, "start"))
    series_2 <- lapply(series_2, function(x) trim_ts(x, length(x) - min_length, "start"))
    serie_3 <- trim_ts(serie_3, length(serie_3) - min_length, "start")
    serie_4 <- trim_ts(serie_4, length(serie_4) - min_length, "start")
    con <- trim_ts(con, length(con) - min_length, "start")
    del <- trim_ts(del, length(del) - min_length, "start")

    errors_1 <- sapply(series_1, error_calc, b = con, type = "mae")
    errors_2 <- sapply(series_2, error_calc, b = con, type = "mae")
    errors_3 <- sapply(serie_3, error_calc, b = con, type = "mae")
    errors_4 <- sapply(serie_4, error_calc, b = con, type = "mae")

    ## Monthly model
    all_models(clus_tel_month$con$orig[[2]], clus_tel_month$del$orig[[2]],
               asact_time = "monthly", asact_init_serie = clus_tel_day$del$orig[[2]])

    plot_mult_xts(del, con)
    plot_mult_xts(foo$SMA$serie[[150]], foo$con)
}
