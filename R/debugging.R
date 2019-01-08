pkgEnvironment = new.env()
assign(x="DEBUG_MODE",value=FALSE,envir=pkgEnvironment)

#' @export
debug = function(status=TRUE) {
  assign(x = "DEBUG_MODE",value = status,envir = pkgEnvironment)
}

#' @export
is.debugging = function() {return(get(x="DEBUG_MODE",envir = pkgEnvironment))}

print.response = function(x) {
  print(x$request)
  
  cat("\n*** Response ***",paste("Status:",x$status_code),"Headers:",sep="\n")
  
  headers = as.data.frame(cbind(headers(x)))
  names(headers) = NULL
  
  print(headers)
  
  if (!is.null(headers["content-length",]) && unlist(headers["content-length",]) != "0") {
    cat("\nBody:",sep="\n")
    
    if (!is.null(headers["content-type",]) && unlist(headers["content-type",]) == "application/json") {
      print(toJSON(content(x), auto_unbox = TRUE, pretty = TRUE))
    } else {
      print("Other formats not yet implemented.")
    }
    
  }
}

print.request = function(x) {
  call = paste(x$method,x$url)
  headers = x$headers
  if (!is.null(x$options$httpauth) && x$options$httpauth == 1 && !is.null(x$options$userpwd)) {
    headers = c(headers, Authorization=paste("Basic",base64_enc(x$options$userpwd)))
  }
  headers = as.data.frame(headers)
  names(headers) = NULL
  
  cat("*** Request ***",call,"Headers:",sep = "\n")
  print(headers)
  
  if (!is.null(x$options$postfields)) {
    cat("Body:",sep="\n")
    print(prettify(rawToChar(x$options$postfields)))
  }
}