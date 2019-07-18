# udf endpoint ----

#' Lists the supported UDF runtimes
#' 
#' The function queries the back-end for its supported udf runtimes and returns detailed information about each
#' runtime.
#' 
#' @param con connected and authenticated openeo client object
#' @return list of udf runtimes with supported udf types, versions and installed packages
#' @export
list_udf_runtimes = function(con) {
    tryCatch({
        tag = "udf_runtimes"
        return(con$request(tag = tag, authorized = FALSE))
    }, error = .capturedErrorToMessage)
}
