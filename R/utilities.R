# utility functions ----
.not_implemented_yet = function() {
  warning("Not implemented yet.")
}

.not_supported_by_client = function() {
  warning("Not supported by the current client version.")
}

.not_supported_by_backend = function() {
  warning("The function is not supported by the current back-end version.")
}

#' Wrapper for toJSON
#' 
#' This function is intended to have a preconfigured toJSON function
#' to allow a user to visualize the process graph in JSON (like it will
#' be sent to the back-end)
#' 
#' @param graph a list / nested list representing the process graph
#' @return JSON string of the process graph as a character string 
#' 
#' @export
# dont't expose it later
graphToJSON = function(graph) {
  #task is a Graph object
  
  if ("Graph" %in% class(graph)) {
    return(toJSON(graph$serialize(),auto_unbox = T,pretty=T,force=TRUE))
  } else {
    stop("Parameter is no Graph object.")
    invisible(NULL)
  }
  
}

.urlHardEncode=function(text) {
  text = URLencode(text)
  text = gsub("\\/","%2F",text)
  text = gsub("\\.","%2E",text)
  return(text)
}

.capturedErrorToMessage = function(e) {
  message(e)
  invisible(NULL)
}
