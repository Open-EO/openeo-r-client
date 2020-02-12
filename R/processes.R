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
            con$processes = listOfProcesses$processes
            
            names(con$processes) = sapply(con$processes, function(p) p$id)
        }
        
        return(lapply(con$processes, function(process) {
            class(process) = "ProcessInfo"
            return(process)
        }))
    }, error = .capturedErrorToMessage)
}

#' Describe a process
#'
#' Queries an openeo back-end and retrieves more detailed information about offered processes
#' @param con Authentication object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param id id of a process to be described
#'
#' @return a list of detailed information
#' @export
describe_process = function(con=NULL, id = NA) {
    con = .assure_connection(con)
    
    describeProcess = !missing(id) && !is.na(id)
    
    if (!describeProcess) {
        message("No or invalid process_id(s)")
        invisible(NULL)
    }
    
    if (is.null(con$processes)) {
        message("No processes found or loaded from the back-end")
        invisible(NULL)
    }
    
    if (!id %in% names(con$processes)) {
        message(paste("Cannot describe process '", id, "'. Process does not exist.", sep = ""))
        invisible(NULL)
    } else {
        return(con$processes[[id]])
    }
}
