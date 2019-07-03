#
# udf endpoint ----
#


#' Defines a UDF on the server
#'
#' This function will allow the user to define and uploads the script (content) into
#' the users workspace (target)
#'
#' @param con The authorized Connection
#' @param prior.name The parameter name of the predecessor of this pipe
#' @param type The udf type
#' @param content The local file path of a script the user wants to upload
#' @param target The relative path on the users workspace on the openEO back-end
#' @param language The programming language of the uploaded script
#' @param ...
#'
#' @return A named list that represents an UDF as list for the process graph
#' @export
defineUDF = function(process,con, prior.name="collections", language, type, content, target, ...) {
  if (!missing(con) && !missing(content)) {
    if (is.character(content)) {
      content = file.path(content)
    }
    if (!file.exists(content)) {
      message(paste("Cannot find file at ",content))
      return()
    }
    
    response = con$uploadUserFile(content,target)
  }
  
  
  
  # type check "process" either collection or process
  res = list()
  
  res$process_id = paste("/udf",language,type,sep="/")
  additionalArgs = list(...)
  
  arguments = list()
  arguments$script = target
  if (!missing(process) && !is.null(process)) {
    if (is.list(process)) {
      
      if (class(process) %in% c("collection","process","udf")) {
        arguments[[prior.name]] = process
      } else {
        stop("Chain corrupted. Prior element is neither process, udf nor collection")
      }
    }
  }
  
  res$args = append(arguments,additionalArgs)
  
  class(res) = "udf"
  
  return(res)
}

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
      endpoint = con$getBackendEndpoint(tag)
      
      return(con$request(operation="GET",endpoint = endpoint,
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
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(language,type)
    
    msg = con$request(operation="GET",endpoint = endpoint,
                      authorized = FALSE)
    return(msg)
  },error=.capturedErrorToMessage)
}