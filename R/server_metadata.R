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
#' @param con A connected OpenEO client (optional), if omitted \code{\link{active_connection}} is used
#' 
#' @return capabilities object
#' 
#' @export
capabilities = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        con$stopIfNotConnected()
        return(con$getCapabilities())
    }, 
    error = function(e) {
        warning(e$message)
        return(invisible(e$message))
    })
    
}

#' List the openeo endpoints
#' 
#' The client queries the version resolved back-end for its endpoint capabilities and returns it as
#' a tibble.
#' 
#' @param con A connected OpenEO client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return data.frame or tibble (if available)
#' 
#' @export
list_features = function(con=NULL) {
    con = .assure_connection(con)
    return(con$api.mapping[c("endpoint", "operation", "available")])
}

#' Returns the output formats
#' 
#' The function queries the back-end for supported output formats.
#' 
#' @param con openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a data frame with formats, the applied output data type ('raster', 'vector', 'table' and/or 'other') and optional configuration parameter
#' @export
list_file_types = function(con=NULL) {
    tryCatch({
        tag = "formats"
        
        con = .assure_connection(con)
        
        # optional sending of bearer otherwise no authentication required
        formats = con$request(tag = tag, authorized = con$isLoggedIn())
        
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
#' @param con a connected openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return vector of identifier of supported webservice
#' @export
list_service_types = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        
        tag = "ogc_services"
        
        services = con$request(tag = tag, authorized = con$isLoggedIn())
        
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

#' Visualizes the terms of service
#' 
#' If the service provides information in their capabilities about their terms of service, the function opens a new RStudio 
#' viewer panel and visualizes the HTML content of the link.
#' 
#' @param con a connected openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a list of the link identifying the terms of service from the service capabilities or NULL
#' @export
terms_of_service = function(con = NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        capabilities = con$getCapabilities()
        
        sel = lapply(capabilities$links, function(link) {
            if (link$rel == "terms-of-service") {
                return(link)
            } else {
                return(NULL)
            }
        })
        sel = as.list(unlist(sel))
        if (length(sel) == 0) {
            openeo:::.no_information_by_backend("terms of service")
            return(invisible(NULL))
        } else {
            htmlViewer(content(GET(sel$href),as = "text",type = "text/html",encoding = "UTF-8"))
            return(invisible(sel))
        }
        
    }, error = .capturedErrorToMessage)
}

#' Visualizes the privacy policy
#' 
#' If the service provides information in their capabilities about their privacy policy, the function opens a new RStudio 
#' viewer panel and visualizes the HTML content of the link.
#' 
#' @param con a connected openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a list of the link identifying the privacy policy from the service capabilities or NULL
#' @export
privacy_policy = function(con = NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        
        capabilities = con$getCapabilities()
        sel = lapply(capabilities$links, function(link) {
            if (link$rel == "privacy-policy") {
                return(link)
            } else {
                return(NULL)
            }
        })
        sel = as.list(unlist(sel))
        if (length(sel) == 0) {
            openeo:::.no_information_by_backend("privacy policy")
            return(invisible(NULL))
        } else {
            htmlViewer(content(GET(sel$href),as = "text",type = "text/html",encoding = "UTF-8"))
            return(invisible(sel))
        }
        
    }, error = .capturedErrorToMessage)
}

#' @export
conformance = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        
        tag = "ogc_conformance"
        return(con$request(tag = tag))
    }, error = .capturedErrorToMessage)
}
