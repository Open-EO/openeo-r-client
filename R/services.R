# services endpoint ----

#' Lists the current users services
#' 
#' Queries the back-end to retrieve a list of services that the current user owns. Services are 
#' webservices like WCS, WFS, etc. The result is an object of type \code{ServiceList}, which is a named list of \code{Service}. The 
#' indices are the service IDs, the service object that is indexed by its ID can be used other functions instead of its service ID.
#' 
#' @param con connected and authenticated openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return named list of Services (class ServiceList)
#' @export
list_services = function(con=NULL) {
    tryCatch(suppressWarnings({
        tag = "user_services"
        
        con = .assure_connection(con)
        
        listOfServices = con$request(tag = tag, authorized = TRUE, type = "application/json")
        
        listOfServices = lapply(listOfServices$services,function(service) {
            class(service) = "Service"
            return(service)
        })
        
        class(listOfServices) = "ServiceList"
        
        service_ids = sapply(listOfServices, function(service)service$id)
        names(listOfServices) = service_ids
        
        return(listOfServices)
    }), error = .capturedErrorToMessage)
}

#' Prepares and publishes a service on the back-end
#' 
#' The function will create a web service of a process graph / workflow on the connected openEO service.
#' 
#' @param type character the ogc web service type name to be created or an object of type ServiceType, which can be obtained by list_service_types()
#' @param graph A \code{\link{Graph}}, a function returning a \code{\link{ProcessNode}} as an endpoint or the \code{\link{ProcessNode}} 
#' will return the results
#' @param title character (optional) - a title for the service intended for visualization purposes in clients for humans to read
#' @param description character (optional) - a short description of the service
#' @param enabled logical - whether or not the service is active or not
#' @param configuration a named list specifying the configuration parameter
#' @param plan character - the billing plan
#' @param budget numeric the amount of credits that can be spent on this service
#' @param con connected and authenticated openeo clien object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param ... additional parameters passed to toJSON (like 'digits')
#'  
#' @return Service object
#' @export
create_service = function(type, graph, title = NULL, description = NULL, enabled = NULL, configuration = NULL, plan = NULL, budget = NULL, con=NULL, ...) {
    tryCatch({
        if (is.null(type)) {
            stop("No type specified.")
        }
        
        con = .assure_connection(con)
        
        # type and process are added later
        service_request_object = list(title = title, 
                                      description = description, 
                                      enabled = enabled, 
                                      configuration = configuration, 
                                      plan = plan, 
                                      budget = budget)
        
        # build an empty process
        if (!is.null(graph)) {
            process = Process$new(id=NA,description = NA,
                                  summary = NA,
                                  process_graph=graph)
            service_request_object$process = process$serialize()
            
            service_request_object$process$process_graph=unclass(service_request_object$process$process_graph)
        } else {
            stop("No process graph was defined. Please provide either a process graph id or a process graph description.")
        }
        
        if ("ServiceType" %in% class(type)) {
            service_request_object$type = type$name
        } else if (is.character(type)) {
            service_request_object$type = type
        } else {
            stop("Type is neither ServiceType nor a character string.")
        }
        
        tag = "service_publish"
        response = con$request(tag = tag, authorized = TRUE, data = service_request_object, encodeType = "json", raw = TRUE, ...)
        
        message("Service was successfully created.")
        locationHeader = headers(response)$location
        split = unlist(strsplit(locationHeader, "/"))
        
        id = trimws(split[length(split)])
        service = describe_service(con=con, service = id)
        return(service)
    }, error = .capturedErrorToMessage)
}

#' Modifies a service
#' 
#' The function updates a service with the given information. If a parameter is NULL then it will
#' not be overwritten at the backend. If the parameter is set to NA then the value on the backend
#' will be deleted and also set to NULL.
#' 
#' @param service the Service or its ID
#' @param type character the ogc web service type name to be created
#' @param graph A \code{\link{Graph}}, a function returning a \code{\link{ProcessNode}} as an endpoint or the \code{\link{ProcessNode}} 
#' will return the results
#' @param title character (optional) the title in human readabled form for the service
#' @param description character (optional) the description for the service
#' @param enabled logical 
#' @param configuration a list of service creation configuration
#' @param plan character the billing plan
#' @param budget numeric the amount of credits that can be spent for this service
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param ... additional parameters passed to toJSON (like 'digits')
#' 
#' @return Service object
#' 
#' @export
update_service = function(service, type = NULL, graph = NULL, title = NULL, description = NULL, enabled = NULL, configuration = NULL, plan = NULL, budget = NULL, con=NULL, ...) {
    
    tryCatch({
        patch = list()
        
        con = .assure_connection(con)
        
        if (!is.null(type)) {
            if ("ServiceType" %in% class(type)) {
                patch[["type"]] = type$name
            } else if (is.character(type)) {
                patch[["type"]] = type
            } else {
                stop("Type is neither ServiceType nor a character string.")
            }
            
        }
        
        if (!is.null(graph)) {
            if (any(c("Graph","function","ProcessNode") %in% class(graph))) {
                process = Process$new(id=NA,description = NA,
                                      summary = NA,
                                      process_graph=graph)
                process = process$serialize()
                
                process = unclass(process)
                
                patch[["process"]] = process
            } else {
                stop("Process graph cannot be set to be empty.")
            }
        }
        
        if ("Service" %in% class(service)) {
            service = service$id
        }
        
        if (!is.null(title)) {
            if (!is.na(title)) {
                patch[["title"]] = title
            } else {
                patch[["title"]] = NULL
            }
        }
        
        if (!is.null(description)) {
            if (!is.na(description)) {
                patch[["description"]] = description
            } else {
                patch[["description"]] = NULL
            }
        }
        
        if (!is.null(enabled)) {
            if (!is.na(enabled) && is.logical(enabled)) {
                patch[["enabled"]] = enabled
            } else {
                stop("No valid data for parameter 'enabled'. Use TRUE, FALSE or NULL")
            }
        }
        
        if (!is.null(configuration)) {
            if (is.na(configuration)) {
                patch[["configuration"]] = NULL
            } else if (is.list(configuration)) {
                patch[["configuration"]] = configuration
            } else {
                stop("No valid data for parameter 'configuration'. It has to be a list")
            }
        }
        
        if (!is.null(plan)) {
            if (!is.na(plan)) {
                patch[["plan"]] = plan
            } else {
                stop("No valid data for parameter 'plan'. Use a plan identifier or skip updating the parameter with NULL")
            }
        }
        
        # budget = NULL
        if (!is.null(budget)) {
            if (!is.na(budget)) {
                patch[["budget"]] = budget
            } else {
                patch[["budget"]] = NULL
            }
        }
        
        tag = "services_update"
        res = con$request(tag = tag, parameters = list(service), authorized = TRUE, encodeType = "json", data = patch, ...)
        message(paste("Service '", service, "' was successfully updated.", sep = ""))
        
        
        service = describe_service(con = con, service = service)
        
        return(service)
    }, error = .capturedErrorToMessage)
}

#' Describes a service
#' 
#' Queries the server and returns information about a particular service
#' 
#' @param service the Service object or its id
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return Service object
#' @export
describe_service = function(service, con=NULL) {
    tryCatch({
        if (is.null(service)) {
            stop("No service id specified.")
        }
        
        if ("Service" %in% class(service)) {
            id = service$id
        } else if (is.character(service)) {
            id = service
        } else {
            stop("No service id provided. Please pass a 'Service' object or the service id.")
        }
        
        
        con = .assure_connection(con)
        
        tag = "services_details"
        service = con$request(tag = tag, parameters = list(id), authorized = TRUE)
        class(service) = "Service"
        return(service)
    }, error = .capturedErrorToMessage)
}

#' Deletes a service function for a job
#' 
#' Queries the back-end and removes the current set service function of job.
#' 
#' @param service the Service or its id
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @export
delete_service = function(service, con=NULL) {
    tryCatch({
        tag = "services_delete"
        
        con = .assure_connection(con)
        
        if (is.character(service)) {
            id = service
        } else if ("Service" %in% class(service)) {
            id = service$id
        }
        
        msg = con$request(tag = tag, parameters = list(id), authorized = TRUE)
        message("Service '", id, "' successfully removed.")
        invisible(TRUE)
    }, error = .capturedErrorToMessage)
}

#' Service log
#' 
#' Attempts to open the log of secondary service.
#' 
#' @param service the service or the service_id
#' @param offset the id of the log entry to start from
#' @param limit the limit of lines to be shown
#' @param con an optional connection if you want to address a specific service
#' 
#' @return a \code{Log} object
#' @export
log_service = function(service, offset=NULL,limit=NULL, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        if (!is.null(service) && "Service" %in% class(service)) {
            service_id = service$id
        } else {
            service_id = service
        }
        
        query_params = list()
        if (length(offset) > 0) {
            query_params$offset = offset
        }
        
        if (length(limit) > 0) {
            query_params$limit = limit
        }
        
        
        if (is.null(service_id)) {
            stop("No job id specified.")
        }
        tag = "service_log"
        
        success = con$request(tag = tag, parameters = list(service_id), authorized = TRUE, query=query_params)
        class(success) = "Log"
        return(success)
    }, error = .capturedErrorToMessage)
}

#' @rdname status
#' @export
status.Service = function(x, ...) {
    x = describe_service(service=x)
    
    if (x$enabled) return("enabled") else return("disabled")
}