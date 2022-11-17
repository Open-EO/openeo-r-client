# udf endpoint ----

#' Lists the supported UDF runtimes
#' 
#' The function queries the back-end for its supported UDF runtimes and returns detailed information about each
#' runtime.
#' 
#' @param con connected and authenticated openEO client object (optional) otherwise [active_connection()]
#' is used.
#' @return list of UDF runtimes with supported UDF types, versions and installed packages
#' @export
list_udf_runtimes = function(con=NULL) {
    tryCatch({
        tag = "udf_runtimes"
        con = .assure_connection(con)
        
        listOfUdfRuntimes = con$request(tag = tag, authorized = con$isLoggedIn())
        
        runtime_names = names(listOfUdfRuntimes)
        
        listOfUdfRuntimes = lapply(runtime_names, function(udf_runtime_name) {
          udf_runtime = listOfUdfRuntimes[[udf_runtime_name]]
          udf_runtime$id = udf_runtime_name
          
          versions = names(udf_runtime$versions)
          udf_runtime$versions = lapply(versions, function(v) {
            version = udf_runtime$versions[[v]]
            version$version = v
            class(version) = "UdfRuntimeVersion"
            
            return(version)
          })
          names(udf_runtime$versions) = versions
          
          class(udf_runtime) = "UdfRuntime"
          return(udf_runtime)
        })
        
        names(listOfUdfRuntimes) = runtime_names
        
        class(listOfUdfRuntimes) = "UdfRuntimeList"
        
        return(listOfUdfRuntimes)
    }, error = .capturedErrorToMessage)
}


#' Test a UDF operation
#' 
#' This function is still under development and depends heavily on test data in a specific format and whether or not the back-end provider
#' exposes their UDF service endpoint or if you have setup a local UDF service (see notes). The openEO UDF API v0.1.0 had foreseen to ship
#' data and code in a single message and to be interpretable by a computing service a specific format was designed. Usually this whole operation
#' is neatly hidden within the back-end, but if you want to test and debug the code, you need to create such data first. Some examples are available
#' at <https://github.com/Open-EO/openeo-r-udf/tree/master/examples/data>.
#' 
#' Hint: If you use a local R UDF service you might want to debug using the 'browser()' function.
#' 
#' @param data file path or a list object with the UDF-API data object
#' @param code a call object or a file path of the user defined code
#' @param host URL to the UDF service
#' @param port (optional) port of the UDF service host
#' @param language programming language (R or Python) of the source code
#' @param debug (optional) logical - Switch on / off debugging information
#' @param user_context list - Context parameter that are shipped from the user into the udf_service
#' @param server_context list - Context usually sent from the back-end to trigger certain settings
#' @param download_info (optional) logical - Whether or not to print the time taken separately for 
#' the download
#' @param legacy logical - Whether or not the legacy endpoint is used (default: FALSE)
#' @param ... parameters passed on to httr::content or to be more precise to jsonlite::fromJSON
#' 
#' @return the textual JSON representation of the result
#' 
#' @note  
#' The debug options are only available for the R-UDF service. The R UDF-API version has to be of version 0.1.0 (not the old alpha 
#' version). You might want to check <https://github.com/Open-EO/openeo-r-udf#running-the-api-locally> for setting up a local 
#' service for debugging.
#' 
#' @examples 
#' \dontrun{
#' port = 5555
#' host = "http://localhost" 
#' script = quote({
#'   all_dim = names(dim(data))
#'   ndvi_result = st_apply(data, FUN = function(X,...) {
#'     (X[8]-X[4])/(X[8]+X[4])
#'   }, MARGIN = all_dim[-which(all_dim=="band")])
#'
#'   all_dim = names(dim(ndvi_result))
#'   min_ndvi = st_apply(ndvi_result,FUN = min, MARGIN = all_dim[-which(all_dim=="t")])
#'
#'   min_ndvi
#' })
#' result = send_udf(data = "hypercube.json",code = script,host=host,port=port)
#' 
#' }
#' @export
send_udf = function(data, code, host="http://localhost", port=NULL, language="R", 
                    debug = FALSE, user_context = NA,server_context=NA, download_info = FALSE, legacy = FALSE, ...) {
  if (is.character(data)) {
    data = read_json(data, simplifyVector = TRUE)
  }
  
  if (is.call(code)) {
    code = paste(deparse(code),collapse = "\n")
  } else if (is.character(code)) {
    if (file.exists(code)) {
      # if valid file path open file and attach
      code = readChar(code, file.info(code)$size)
    }    
  }
  
  payload = list(
    code = list(
      source = code,
      language = language),
    data = data
  )
  
  if (!legacy) {
    if (length(user_context) > 0 && !is.na(user_context)) {
      payload$data$user_context = user_context
    }
    
    if (length(server_context) > 0 && !is.na(server_context)) {
      payload$data$server_context = server_context
    }
  }
  
  
  endpoint = if (legacy) "/udf_legacy" else "/udf"
  if (is.null(port)) {
    url = paste0(host,endpoint)
  } else {
    url = paste0(host,":",port,endpoint)
  }
  
  
  
  options(digits.secs=3)
  start = Sys.time()
  
  req = request(url)
  req = req_method(req,"POST")
  
  req = req_headers(req, Date=start, `Content-Type`="application/json")
  
  req = req_body_json(payload,auto_unbox = TRUE)
  
  res = req_perform(req)
  
  end = Sys.time()
  if (debug) {
    print(end-start)
  }
  
  if (download_info) {
    cat("Download time:\n")
    print(end-as_datetime(res$date,tz=Sys.timezone()))
  }
  
  val = resp_body_json(res)
  
  if (res$status_code >= 400) {
    return(val)
  } else {
    return(val)
  }
  
}

