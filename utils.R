#' title: utils.R
#' comments: this file contains utility functions for working on the code
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages

#' imports

#' functions
lapply_pb <- function(X, FUN, ...) {
    env <- environment()
    pb_Total <- length(X)
    counter <- 0
    pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)

    ## wrapper around FUN
    wrapper <- function(...){
        curVal <- get("counter", envir = env)
        assign("counter", curVal +1 ,envir=env)
        setTxtProgressBar(get("pb", envir=env), curVal +1)
        FUN(...)
    }
    res <- lapply(X, wrapper, ...)
    close(pb)
    res
}


quoted_call <- function(fun, ..., dots = NULL)
{
    #' do.call but always quoted
    do.call(fun, enlist(..., dots = dots), quote = TRUE)
}


enlist <- function(..., dots = NULL)
{
    #' Enlist parameters for do.calls
    c(list(...), dots)
}
