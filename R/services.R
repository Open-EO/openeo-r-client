# services endpoint ----

#' Lists the current users services
#' 
#' Queries the back-end to retrieve a list of services that the current user owns. Services are 
#' webservices like WCS, WFS, etc.
#' 
#' @param con connected and authenticated openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return list of services (class ServiceList)
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
#' The function will send a configuration object to the back-end to create a webservice from a job considering
#' additional parameter.
#' 
#' @param con connected and authenticated openeo clien object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param type character the ogc web service type name to be created or an object of type ServiceType, which can be obtained by list_service_types()
#' @param graph A graph object, a ProcessNode (result node) or function returning a ProcessNode
#' @param title character (optional) the title in human readabled form for the service
#' @param description character (optional) the description for the service
#' @param enabled logical 
#' @param configuration a named list specifying the configuration parameter
#' @param plan character the billing plan
#' @param budget numeric the amount of credits that can be spent for this service
#' @return service representation as list
#' @export
create_service = function(con=NULL, type, graph, title = NULL, description = NULL, enabled = NULL, configuration = NULL, plan = NULL, budget = NULL) {
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
        response = con$request(tag = tag, authorized = TRUE, data = service_request_object, encodeType = "json", raw = TRUE)
        
        message("Service was successfully created.")
        locationHeader = headers(response)$location
        split = unlist(strsplit(locationHeader, "/"))
        return(trimws(split[length(split)]))
    }, error = .capturedErrorToMessage)
}

#' Modifies a service
#' 
#' The function updates a service with the given information. If a parameter is NULL then it will
#' not be overwritten at the backend. If the parameter is set to NA then the value on the backend
#' will be deleted and also set to NULL.
#' 
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param service the Service or its ID
#' @param type character the ogc web service type name to be created
#' @param process_graph list of processes composed as a process graph
#' @param title character (optional) the title in human readabled form for the service
#' @param description character (optional) the description for the service
#' @param enabled logical 
#' @param configuration a list of service creation configuration
#' @param plan character the billing plan
#' @param budget numeric the amount of credits that can be spent for this service
#' @return service representation as list
#' 
#' @export
update_service = function(con=NULL, service, type = NULL, graph = NULL, title = NULL, description = NULL, enabled = NULL, configuration = NULL, plan = NULL, budget = NULL) {
    
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
        res = con$request(tag = tag, parameters = list(service), authorized = TRUE, encodeType = "json", data = patch)
        message(paste("Service '", service, "' was successfully updated.", sep = ""))
        invisible(TRUE)
    }, error = .capturedErrorToMessage)
}

#' Describes a service
#' 
#' Queries the server and returns information about a particular service
#' 
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param service the Service object or its id
#' @return service as a list
#' @export
describe_service = function(con=NULL, service) {
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
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param service the Service or its id
#' @export
delete_service = function(con=NULL, service) {
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
        invisible(msg)
    }, error = .capturedErrorToMessage)
}
