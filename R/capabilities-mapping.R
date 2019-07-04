#' @importFrom utils read.csv2
load_api = function(version) {
  if (!version %in% c("0.0.2","0.3.1","0.4.1")) stop("Unsupported API version.")
  
  api = read.csv2(system.file("extdata","api_0.4.1.csv",package="openeo"),stringsAsFactors=FALSE)
  
  api = as_tibble(api)[c("endpoint","operation","tag")]
  
  return(api)
}

endpoint_mapping = function(con) {
  endpoints = (capabilities(con))$endpoints
  
  server_offering = tibble(path=character(),method=character())
  for (i in 1:length(endpoints)) {
    entry = endpoints[[i]]
    path = entry$path
    
    for (j in 1:length(entry$method)) {
      method = entry$method[[j]]
      
      server_offering = add_row(server_offering, path=path,method=method)
    }
  }
  
  api = load_api(version="0.4.1")

  backend_df = data.frame(endpoint = unlist(sapply(server_offering$endpoints, function(entry) {
    return(rep(entry$path, length(entry$methods)))
  })),operation = unlist(sapply(server_offering$endpoints, function(entry) {
    return(entry$methods)
  })))
  
  backend_df[,"available"] = TRUE
  
  api_mapping=merge(api,backend_df,all.x=TRUE,by=c("endpoint","operation"))
  api_mapping[is.na(api_mapping$available),"available"] = FALSE
  
  return(as_tibble(api_mapping))
  
}

replace_endpoint_parameter = function(endpoint, ...) {
  if (startsWith(endpoint,"/")) {
    coll = unlist(strsplit(endpoint,split = "/"))[-1]
  } else {
    coll = unlist(strsplit(endpoint,split = "/"))
  }
  
  endsWithSlash = endsWith(endpoint,"/")
  
  # get parameter
  variable_pattern = "^[\\{|<|\\[|%].*[\\}|>|\\]|%]$"
  
  param_names = grepl(variable_pattern,coll)
  
  params = list(...)
  # replace those parameter by given ... parameter (based on order)
  coll[param_names] = params
  
  if (endsWithSlash) {
    return(paste(coll,collapse="",sep="","/"))
  } else {
    return(paste(coll,collapse = "/",sep=""))
  }
  
}

#' Tag support lookup
#' 
#' Looks up the client tag for a particular endpoint on the back-end and returns whether it is available
#' or not.
#' 
#' @param con backend connection
#' @param tag_name the endpoints "tag" name as character
#' @return logical - whether the back-end supports the endpoint or not
#' 
#' @export
supports = function(con, tag_name) {
  return(unname(
    unlist(
      dplyr::select(
        dplyr::filter(
          con$api.mapping,
          tag == tag_name),
        available))))
}