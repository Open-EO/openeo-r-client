#
# udf endpoint ----
#

#' Lists the supported UDF runtimes
#' 
#' The function queries the back-end for its supported udf runtimes and returns detailed information about each
#' runtime.
#' 
#' @param con connected and authenticated openeo client object
#' @return list of udf runtimes with supported udf types, versions and installed packages
#' @export
list_udf_runtimes = function(con) {
  tryCatch(
    {
      tag = "udf_runtimes"
      return(con$request(tag=tag,
                         authorized = FALSE))
    }, 
    error = .capturedErrorToMessage
  )
}

#' Gets detailed information about a particular udf type
#' 
#' Queries the back-end for a particular runtime and time to retrieve information how the udf_type will work.
#' 
#' @param con connected and authenticated openeo client object
#' @param language the udf runtime identifier
#' @param type the udf type
#' @return list with udf runtime type information
#' @export
describe_udf_type = function(con, language, type) {
  tryCatch({
    if (is.null(language) || is.null(type)) {
      stop("Missing parameter language or udf_type")
    }
    
    tag = "udf_functions"
    msg = con$request(tag=tag,
                      parameters=list(language,type),
                      authorized = FALSE)
    return(msg)
  },error=.capturedErrorToMessage)
}