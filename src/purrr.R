#' title: purrr.R
#' comments: this file contains edits and imports from the purrr package
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages
library(purrr)

#' imports

#' functions
reduce_subset_call <- getFromNamespace("reduce_subset_call", "purrr")


force_assign_in <- function(x, where, value) {
    #' Does the same as assign_in or pluck<- but doesn't check if the where
    #' exists in the x object. If the indexes don't exist they are created
    #' during the assignment. This function is a lot more dangerous than the
    #' normal assign_in use with caution.
    call <- reduce_subset_call(quote(x), as.list(where))
    call <- call("<-", call, value)
    eval(call)

    return(x)
}


pluck_list <- function(l, ...) {
    #' Apply plus over a list of object, plucing each ones with args
    return(lapply(l, function(x) purrr::pluck(x, ...)))
}
