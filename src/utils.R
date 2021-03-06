#' title: utils.R
#' comments: this file contains utility functions for working on the code
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages

#' imports

#' functions
filtered_substitute <- function(expr, env) {
    #' Works exactly like substitute but removes any arguments from the expression
    #' that are NULL from the environment. Currently it works only with
    #' The expression must be quoted to be passed properly.

    #' Example
    #' f1 <- function(x=1,y=1,z=1) x+y+z
    #' expr <- quote(f1(x = a, y = 2, z = b))
    #' env <- list(a = 1, b = NULL)
    #' filted_substitute(expr, env)
    #' f1(x=1, y=2)

    ## Determine which elements in our environment are NULL
    null_ind <- names(env)[sapply(env, is.null)]

    ## Convert the expression into a named list
    list_expr <- as.list(expr)

    ## Remove from the named expr list the elements which are from our null list
    ## Convert the list back into a function call
    new_expr <- as.call(list_expr[!(list_expr %in% null_ind)])

    ## Substitute the new expression using the envrionment
    sub_new_expr <- do.call("substitute", args = list(expr=new_expr, env = env))
    return(sub_new_expr)
}


cache_name <- function(id, cache)
{
    #' Return the file name for the id and cache
    cache <- match.arg(cache, c("distance"))

    switch(cache,
           distance = {
               file <- paste("../data/master/cluster/distance/", id, ".rds", sep = "")
           })

    return(file)
}


load_cache <- function(id, cache)
{
    #' Load data from the cache
    file <- cache_name(id, cache)
    if (file.exists(file)) {
        data <- readRDS(file)
    } else {
        data <- NULL
    }

    return(data)
}


save_cache <- function(data, id, cache)
{
    #' Save the data to the cache
    file <- cache_name(id, cache)
    saveRDS(data, file)
}


cross_apply <- function(l1, l2, FUN, ...) {
    #' Apply a function over two list where every element is applied to function
    #' crosswise
    #' Example
    #' l1 <- c(A, B)
    #' l2 <- c(C, D)
    #' will be evaluated
    #' f(A,C), f(A,D), f(B,C), f(B,D)
    return(sapply(l1, function(x) sapply(l2, function(y) FUN(x, y)), ...))
 }
