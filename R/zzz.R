pkgEnvironment = new.env()

.onLoad = function(libname, pkgname) {
    # flag if the debug mode is on in order to print more details on the HTTP requests
    assign(x = "DEBUG_MODE", value = FALSE, envir = pkgEnvironment)
  
    # the currently active OpenEOConnection
    assign(x = "active_connection", value = NULL, envir = pkgEnvironment)
    
    # translated processes from JSON into Process object
    assign(x = "process_collection", value = NULL, envir = pkgEnvironment)
    
    # named list of the services data collection (~JSON result from the collections endpoint)
    assign(x = "data_collection", value = NULL, envir = pkgEnvironment)
    
    # a named list of processes which are created from JSON at the processes endpoint
    assign(x = "process_list", value = NULL, envir = pkgEnvironment)
    
    register_all_spatial_s3_methods()
}

#' openeo-deprecated
#' 
#' Lists all currently deprecated functions that will be removed in the future.
#' 
#' @name openeo-deprecated
#' @rdname openeo-deprecated
#' 
#' @section Deprecated:
#' \describe{
#'  \item{graphToJSON(x,...)}{replaced by toJSON}
#'  \item{processToJSON(x,...)}{replaced by toJSON}
#' }
NULL