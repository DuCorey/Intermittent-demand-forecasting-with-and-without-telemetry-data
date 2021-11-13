#' title: state_event.R
#' comments: State/event pattern in R
#' author: Corey Ducharme / corey.ducharme@polymtl.ca

#' packages

#' imports

#' functions
update_state <- function(current_state, event, how = c("parallel", "apply", "call"), mc.cores = 8) {
    #' Apply the event function to the current state of our data object
    #' Caculate it in parallel to speed up code exectution

    if (length(how) > 1) {
        ## Length is greater than 1 if no option is sent because the default is
        ## a character vector. If we don't check manually a character vector
        ## sent to a switch function will attempt to match on the first object
        ## in the vector.
        stop("Must select an option for how. Available options are 'parallel', 'apply', and 'call'.")
    }

    res <- switch(how, parallel = {
        mclapply(current_state, event, mc.cores = mc.cores)
    }, apply = {
        lapply(current_state, event)
    }, call = {
        quoted_call(event, current_state)
    })

    return(res)
}


unzip_update_state <- function(current_state, event) {
    #' Unzip the event args which are a list and call the update_state function
    #' with the all args.
    return(do.call(update_state, c(list(current_state = current_state), event), quote = TRUE))
}


end_state <- function(start_state, events) {
    #' Events are a list of two elements which contain the event function and
    #' the args to be given to the update_state function

    return(Reduce(unzip_update_state, init = start_state, x = events))
}
