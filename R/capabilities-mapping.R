#' @include zzz.R
#' @importFrom utils read.csv2
load_api = function(version) {
    if (!version %in% c("0.0.2", "0.3.1", "0.4.1", "0.4.2","1.0.0-rc.2","1.0.0")) 
        stop("Unsupported API version.")
    
    api = read.csv2(system.file("extdata", paste0("api_",version,".csv"), package = "openeo"), stringsAsFactors = FALSE)
    if (isNamespaceLoaded("tibble")) 
        api = tibble::as_tibble(api)
    
    return(api[, c("endpoint", "operation", "tag")])
}

endpoint_mapping = function(con) {
    tryCatch({
        con = .assure_connection(con)
    }, error = function(e){
        message("Not connected to an openEO service.")
        return(NULL)
    })
    
    
    endpoints = capabilities(con)$endpoints
    
    # each package R version refers to a certain API version, this is also documented
    api = load_api(version = "1.0.0")
    
    backend_df = data.frame(endpoint = unlist(sapply(endpoints, function(entry) {
        return(rep(entry$path, length(entry$methods)))
    })), operation = unlist(sapply(endpoints, function(entry) {
        return(entry$methods)
    })))
    
    backend_df[, "available"] = TRUE
    
    api_mapping = merge(api, backend_df, all.x = TRUE, by = c("endpoint", "operation"))
    api_mapping[is.na(api_mapping$available), "available"] = FALSE
    
    
    if (isNamespaceLoaded("tibble")) {
        api_mapping = tibble::as_tibble(api_mapping)
    }
    return(api_mapping)
    
}

requires_endpoint_parameter = function(endpoint) {
    if (startsWith(endpoint, "/")) {
        coll = unlist(strsplit(endpoint, split = "/"))[-1]
    } else {
        coll = unlist(strsplit(endpoint, split = "/"))
    }
    
    endsWithSlash = endsWith(endpoint, "/")
    
    # get parameter
    variable_pattern = "^[\\{|<|\\[|%].*[\\}|>|\\]|%]$"
    
    param_names = grepl(pattern = variable_pattern, x = coll)
    
    return(any(param_names))
}

replace_endpoint_parameter = function(endpoint, ...) {
    if (startsWith(endpoint, "/")) {
        coll = unlist(strsplit(endpoint, split = "/"))[-1]
    } else {
        coll = unlist(strsplit(endpoint, split = "/"))
    }
    
    endsWithSlash = endsWith(endpoint, "/")
    
    # get parameter
    variable_pattern = "^[\\{|<|\\[|%].*[\\}|>|\\]|%]$"
    
    param_names = grepl(pattern=variable_pattern, x = coll)
    
    params = list(...)
    # replace those parameter by given ... parameter (based on order)
    coll[param_names] = params
    
    if (endsWithSlash) {
        return(paste(coll, collapse = "", sep = "", "/"))
    } else {
        return(paste(coll, collapse = "/", sep = ""))
    }
    
}

#' Tag support lookup
#' 
#' Finds the client tag for a particular endpoint on the back-end and returns whether it is available
#' or not.
#' 
#' @param con backend connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param tag_name the endpoints 'tag' name as character
#' @return logical - whether the back-end supports the endpoint or not
#' 
#' @export
supports = function(con=NULL, tag_name) {
    tryCatch({
        con = .assure_connection(con)
    }, error = function(e){
        message("Not connected to an openEO service.")
        return(NULL)
    })
    
    if (isNamespaceLoaded("tibble")) 
        return(con$api.mapping[con$api.mapping$tag == tag_name, "available"][[1]]) else return(con$api.mapping[con$api.mapping$tag == tag_name, "available"])
}
