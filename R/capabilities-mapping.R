#' @include zzz.R
#' @importFrom utils read.csv2
load_api = function(version) {
    if (!version %in% c("0.0.2", "0.3.1", "0.4.1")) 
        stop("Unsupported API version.")
    
    api = read.csv2(system.file("extdata", "api_0.4.1.csv", package = "openeo"), stringsAsFactors = FALSE)
    
    if (isNamespaceLoaded("tibble")) 
        api = tibble::as_tibble(api)
    
    return(api[, c("endpoint", "operation", "tag")])
}

endpoint_mapping = function(con) {
    con = .assure_connection(con)
    
    endpoints = capabilities(con)$endpoints
    
    api = load_api(version = "0.4.1") # also valid for 0.4.2
    
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

replace_endpoint_parameter = function(endpoint, ...) {
    if (startsWith(endpoint, "/")) {
        coll = unlist(strsplit(endpoint, split = "/"))[-1]
    } else {
        coll = unlist(strsplit(endpoint, split = "/"))
    }
    
    endsWithSlash = endsWith(endpoint, "/")
    
    # get parameter
    variable_pattern = "^[\\{|<|\\[|%].*[\\}|>|\\]|%]$"
    
    param_names = grepl(variable_pattern, coll)
    
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
#' Looks up the client tag for a particular endpoint on the back-end and returns whether it is available
#' or not.
#' 
#' @param con backend connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param tag_name the endpoints 'tag' name as character
#' @return logical - whether the back-end supports the endpoint or not
#' 
#' @export
supports = function(con=NULL, tag_name) {
    con = .assure_connection(con)
    
    if (isNamespaceLoaded("tibble")) 
        return(con$api.mapping[con$api.mapping$tag == tag_name, "available"][[1]]) else return(con$api.mapping[con$api.mapping$tag == tag_name, "available"])
}
