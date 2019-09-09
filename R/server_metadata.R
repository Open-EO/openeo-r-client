# server endpoint ----
#' Returns the suppported OpenEO API versions
#' 
#' The function queries the back-end for its supported versions. The endpoint \href{https://open-eo.github.io/openeo-api/apireference/#tag/Capabilities/paths/~1.well-known~1openeo/get}{/.well-known/openeo} 
#' is called and the JSON result is coerced into a tibble.
#' 
#' @param url the url as String pointing to the base host of the back-end
#' 
#' @return a tibble containing all supported API versions of the back-end
#' @export
api_versions = function(url) {
    tryCatch({
        
        if (endsWith(url, "/")) 
            url = substr(url, 1, nchar(url) - 1)
        endpoint = "/.well-known/openeo"
        
        info = GET(url = paste(url, endpoint, sep = "/"))
        if (info$status == 200) {
            vlist = content(info)
            class(vlist) = "VersionsList"
            
            if (isNamespaceLoaded("tibble")) {
                return(tibble::as_tibble(vlist))
            }
            return(as.data.frame(vlist))
        } else {
            stop("Host is not reachable. Please check the stated URL.")
        }
        
    }, error = .capturedErrorToMessage)
}

#' Shows an overview about the capabilities of an OpenEO back-end
#' 
#' Queries the back-end for its general capabilities.
#' 
#' @param con A connected OpenEO client
#' 
#' @return capabilities object
#' 
#' @export
capabilities = function(con) {
    endpoint = "/"
    tryCatch({
        con$stopIfNotConnected()
        capabilities = content(httr::GET(url = paste0(con$getHost(), endpoint)))
        class(capabilities) = "OpenEOCapabilities"
        return(capabilities)
    }, error = .capturedErrorToMessage)
}

#' List the openeo endpoints
#' 
#' The client queries the version resolved back-end for its endpoint capabilities and returns it as
#' a tibble.
#' 
#' @param con A connected OpenEO client
#' 
#' @return data.frame or tibble (if available)
#' 
#' @export
list_features = function(con) {
    return(con$api.mapping[c("endpoint", "operation", "available")])
}

#' Returns the output formats
#' 
#' The function queries the back-end for supported output formats.
#' 
#' @param con openeo client object
#' @return a data frame with formats, the applied output data type ('raster', 'vector', 'table' and/or 'other') and optional configuration parameter
#' @export
list_file_types = function(con) {
    tryCatch({
        tag = "formats"
        
        formats = con$request(tag = tag, authorized = FALSE)
        
        class(formats) = "FileTypesList"
        
        table = as.data.frame(formats)
        
        if (isNamespaceLoaded("tibble")) {
            table = tibble::as_tibble(table)
        }
        
        return(table)
    }, error = .capturedErrorToMessage)
}

#' Returns the offered webservice types of the back-end
#' 
#' The function queries the back-end for the supported webservice types that can be used on the client.
#' 
#' @param con a connected openeo client object
#' @return vector of identifier of supported webservice
#' @export
list_service_types = function(con) {
    tryCatch({
        con$stopIfNotConnected()
        
        tag = "ogc_services"
        
        services = con$request(tag = tag, authorized = FALSE)
        
        updated_services = list()
        for (key in names(services)) {
            service = services[[key]]
            service$service = key
            
            updated_services = c(updated_services, list(service))
        }
        return(lapply(updated_services, function(service) {
            class(service) = "ServiceType"
            return(service)
        }))
    }, error = .capturedErrorToMessage)
    
    return(con$list_service_types())
}
