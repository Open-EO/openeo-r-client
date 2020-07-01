# server endpoint ----
#' Returns the suppported OpenEO API versions
#' 
#' The function queries the back-end for its supported versions. The endpoint \href{https://open-eo.github.io/openeo-api/apireference/#tag/Capabilities/paths/~1.well-known~1openeo/get}{/.well-known/openeo} 
#' is called on the given host URL and the JSON result is coerced into a tibble.
#' 
#' @param url the url as String pointing to the base host of the back-end
#' 
#' @return a data.frame or a tibble containing all supported API versions of the back-end
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

#' Capabilities overview
#' 
#' The function queries the openEO service to which was connected for general information about the service. 
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

#' Supported Input/Output formats
#' 
#' The function queries the openEO service for supported I/O formats as a \code{FileFormatList} object.
#' 
#' @details The \code{FileFormatList} object ist a named list, which is organized into 'input' and 'output'. At each category we have another
#' named list with the \code{FileFormat} indexed by its format ID.
#' 
#' @param con openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a FileFormatList object
#' @export
list_file_formats = function(con=NULL) {
    tryCatch({
        tag = "formats"
        
        con = .assure_connection(con)
        
        # optional sending of bearer otherwise no authentication required
        formats = con$request(tag = tag, authorized = con$isLoggedIn())
        
        class(formats) = "FileFormatList"
        
        if (length(formats$input) > 0) {
            input_formats = names(formats$input)
            modified_input_formats = lapply(input_formats, function(format_name) {
                f = formats$input[[format_name]]
                f$name = format_name
                class(f) = "FileFormat"
                return(f)
            })
            names(modified_input_formats) = input_formats
        }
        
        if (length(formats$output) > 0) {
            output_formats = names(formats$output)
            modified_output_formats = lapply(output_formats, function(format_name) {
                f = formats$output[[format_name]]
                f$name = format_name
                class(f) = "FileFormat"
                return(f)
            })
            names(modified_output_formats) = output_formats 
            formats$output = modified_output_formats
        }
        
        return(formats)
    }, error = .capturedErrorToMessage)
}

#' Returns the offered webservice types of the back-end
#' 
#' The function queries the back-end for the supported webservice types that can be used on the client and returns a named list of
#' \code{ServiceType}, which are indexed by the service type ID. ServiceTypes can later be used when creating a supported web service
#' out of the user defined process (process graph).
#' 
#' @param con a connected openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a \code{ServiceTypeList}
#' @export
list_service_types = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        
        tag = "ogc_services"
        
        services = con$request(tag = tag, authorized = con$isLoggedIn())
        class(services) = "ServiceTypeList"
        
        services_type_names = names(services)
        
        services = lapply(services_type_names, function(service_name) {
            service = services[[service_name]]
            service$name = service_name
            class(service) = "ServiceType"
            
            return(service)
        })
        
        names(services) = services_type_names
        return(services)
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

#' OGC conformance
#' 
#' Queries the openEO service for the conformance. As mentioned in the API it is highly optional and is only available if the service
#' wants to achieve full compatibility with OGC API clients. This function queries the /conformance endpoint and returns it results
#' as a list object translated from JSON using the jsonlite package.
#' 
#' @export
conformance = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        
        tag = "ogc_conformance"
        return(con$request(tag = tag))
    }, error = .capturedErrorToMessage)
}
