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
        if (missing(con)) {
          con = active_connection()
        }
        return(con$request(tag = tag, authorized = con$isLoggedIn()))
    }, error = .capturedErrorToMessage)
}


#' Test a UDF operation
#' 
#' This function is a testing function for UDF requests against a UDF service. If an 
#' openEO UDF service is available (external via an URL or a local setup) and if you can obtain 
#' sample data that fit the context of your UDF use case, you can use this function to send the 
#' code and the data to the UDF service. Depending on the state of the service (error or success)
#' you can debug your code.
#' 
#' If you use a local R UDF service you might want to debug using the 'browser()' function.
#' 
#' @param data file path or a list object with the UDF-API data object
#' @param code a call object or a file path of the user defined code
#' @param host URL to the UDF service
#' @param port optional port of the UDF service host
#' @param language programming language (R or Python) of the source code
#' @param debug (optional) logical. Switch on / off debugging information for time taken
#' @param download_info (optional) logical. Whether or not to print the time taken separately for 
#' the download
#' 
#' @return the textual JSON representation of the result
#' 
#' @note  
#' The debug options are only available for the R-UDF service.
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
send_udf = function(data, code, host="http://localhost", port=NULL, language="R", debug = FALSE, download_info = FALSE) {
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
  
  if (is.null(port)) {
    url = paste0(host,"/udf")
  } else {
    url = paste0(host,":",port,"/udf")
  }
  
  
  
  options(digits.secs=3)
  start = Sys.time()
  res = httr::POST(url,config=add_headers(Date=start),body=toJSON(payload,auto_unbox = TRUE),encode=c("json"))
  end = Sys.time()
  if (debug) {
    print(end-start)
  }
  
  if (download_info) {
    cat("Download time:\n")
    print(end-as_datetime(res$date,tz=Sys.timezone()))
  }
  
  if (res$status > 400) {
    message(paste0("[Server-ERROR] ",content(res,as = "parsed")$message))
  } else {
    return(content(res,as = "text",encoding = "UTF-8"))
  }
  
}