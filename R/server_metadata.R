# server endpoint ----
#' Returns the supported openEO API versions
#' 
#' The function queries the back-end for its supported versions. The endpoint \href{https://openeo.org/documentation/1.0/developers/api/reference.html#operation/connect}{/.well-known/openeo} 
#' is called on the given host URL and the JSON result is coerced into a \code{tibble}.
#' 
#' @param url the URL as string pointing to the base host of the back-end
#' 
#' @return a \code{data.frame} or a \code{tibble} containing all supported API versions of the back-end
#' @export
api_versions = function(url) {
    tryCatch({
        if (endsWith(url, "/")) 
            url = substr(url, 1, nchar(url) - 1)
        endpoint = "/.well-known/openeo"
        
        info = request(paste0(url, endpoint))
        info = req_headers(info,`Content-Type` = "application/json")
        info = req_perform(info)
        
        if (info$status == 200) {
            vlist = resp_body_json(info)
            
            if (is.raw(vlist)) {
                vlist=jsonlite::fromJSON(rawToChar(vlist),simplifyDataFrame = FALSE)
            }
            class(vlist) = "VersionsList"
            
            if (isNamespaceLoaded("tibble")) {
                return(tibble::as_tibble(vlist))
            }
            return(as.data.frame(vlist))
        } else {
            stop("Host is not reachable. Please check the URL.")
        }
        
    }, error = .capturedErrorToMessage)
}

#' Capabilities overview
#' 
#' The function queries the connected openEO service for general information about the service. 
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

#' List the openEO endpoints
#' 
#' The client queries the version resolved back-end for its endpoint capabilities and returns it as
#' a tibble.
#' 
#' @param con A connected openEO client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return \code{data.frame} or \code{tibble} (if available)
#' 
#' @export
list_features = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        return(con$api.mapping[c("endpoint", "operation", "available")])
    }, error = .capturedErrorToMessage)
}

#' Supported Input/Output formats
#' 
#' The function queries the openEO service for supported I/O formats as a \code{FileFormatList} object.
#' 
#' @details The \code{FileFormatList} object is a named list, which is organized into 'input' and 'output'. For each category a different 
#' named list with the \code{FileFormat} is indexed by its format ID.
#' 
#' @param con openEO client object (optional) otherwise \code{\link{active_connection}}
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
                f$type = "input"
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
                f$type = "output"
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

#' Returns the web service types of the back-end
#' 
#' The function queries the back-end for the supported web service types usable by the client and returns a named list of
#' \code{ServiceType} indexed by the service type ID. ServiceTypes can be used when creating a supported web service
#' from the user defined process (process graph).
#' 
#' @param con a connected openEO client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a \code{ServiceTypeList}
#' @export
list_service_types = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        
        tag = "ogc_services"
        
        services = con$request(tag = tag, authorized = con$isLoggedIn())
        
        services_type_names = names(services)
        
        services = lapply(services_type_names, function(service_name) {
            service = services[[service_name]]
            service$name = service_name
            class(service) = "ServiceType"
            
            return(service)
        })
        
        names(services) = services_type_names
        class(services) = "ServiceTypeList"
        return(services)
    }, error = .capturedErrorToMessage)
    
    return(services)
}

#' Visualize the terms of service
#' 
#' If the service provides information about their terms of service in the capabilities, the function opens a new RStudio 
#' viewer panel and visualizes the HTML content of the link.
#' 
#' @param con a connected openEO client object (optional) otherwise \code{\link{active_connection}}
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
            .no_information_by_backend("terms of service")
            return(invisible(NULL))
        } else {
            req = request(sel$href)
            req = req_headers(req,`Content-Type`="text/html")
            res = req_perform(req)

            htmlViewer(as.character(resp_body_html(res)))
            
            cat(sel$href,"\n")
            
            return(invisible(sel))
        }
        
    }, error = .capturedErrorToMessage)
}

#' Visualize the privacy policy
#' 
#' If the service provides information about their privacy policy in their capabilities, the function opens a new RStudio 
#' viewer panel and visualizes the HTML content of the link.
#' 
#' @param con a connected openEO client object (optional) otherwise \code{\link{active_connection}}
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
            .no_information_by_backend("privacy policy")
            return(invisible(NULL))
        } else {
            req = request(sel$href)
            req = req_headers(req,`Content-Type`="text/html")
            res = req_perform(req)
            
            htmlViewer(as.character(resp_body_html(res)))
            
            cat(sel$href,"\n")
            return(invisible(sel))
        }
        
    }, error = .capturedErrorToMessage)
}

#' OGC conformance
#' 
#' Queries the openEO service for the conformance. As stated in the API it is highly optional and  only available if the service
#' wants to achieve full compatibility with OGC API clients. This function queries the /conformance endpoint and returns it results
#' as a list object translated from JSON using the jsonlite package.
#' 
#' @param con a connected openEO client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @export
conformance = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$stopIfNotConnected()
        
        tag = "ogc_conformance"
        return(con$request(tag = tag))
    }, error = .capturedErrorToMessage)
}
