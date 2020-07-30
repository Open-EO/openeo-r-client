# processes endpoint ----

#' List available processes on server
#'
#' list all processes available on the back-end
#' @param con Connection object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a list of lists with process_id and description
#' @export
list_processes = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        if (is.null(con$processes)) {
            tag = "process_overview"
            
            listOfProcesses = con$request(tag = tag, authorized=con$isLoggedIn(), type = "application/json")
            con$processes = lapply(listOfProcesses$processes, function(process) {
                class(process) = "ProcessInfo"
                return(process)
            })
            
            names(con$processes) = sapply(con$processes, function(p) p$id)
        }
        
        return(con$processes)
    }, error = .capturedErrorToMessage)
}

#' Describe a process
#'
#' Queries an openeo back-end and retrieves more detailed information about offered processes
#' @param con Authentication object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param process id of a process to be described or the ProcessInfo object
#'
#' @return a list of detailed information
#' @export
describe_process = function(process = NA, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        describeProcess = !missing(process) && !is.na(process)
        
        if (!describeProcess) {
            message("No or invalid process_id(s) or process")
            invisible(NULL)
        }
        
        if ("ProcessInfo" %in% class(process)) {
            return(process)
        }
        
        if (is.null(con$processes)) {
            message("No processes found or loaded from the back-end")
            invisible(NULL)
        }
        
        if (!process %in% names(con$processes)) {
            message(paste("Cannot describe process '", process, "'. Process does not exist.", sep = ""))
            invisible(NULL)
        } else {
            return(con$processes[[process]])
        }
    }, error = .capturedErrorToMessage)
}

#' Get a process graph builder / process collection from the connection
#' 
#' Queries the connected back-end for all available processes and collection names and registers them via R functions on
#' a ProcessCollection object to build a process graph in R. To get a better overview about the process graph building, please have
#' a look at \url{https://github.com/Open-EO/openeo-r-client/wiki/Process-Graph-Building}
#' 
#' @param con a connection to an openeo back-end (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a ProcessCollection object with the offered processes of the backend
#' @export
processes = function(con = NULL) {
    tryCatch({
        con = .assure_connection(con)
        return(con$getProcessCollection())
    }, error = .capturedErrorToMessage)
}




