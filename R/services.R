# services endpoint ----

#' Lists the current users services
#' 
#' Queries the back-end to retrieve a list of services that the current user owns. Services are 
#' webservices like WCS, WFS, etc.
#' 
#' @param con connected and authenticated openeo client object
#' 
#' @return list of services lists
#' @export
list_services = function(con) {
    tryCatch(suppressWarnings({
        tag = "user_services"
        
        if (missing(con)) {
            con = active_connection()
        }
        
        listOfServices = con$request(tag = tag, parameters = list(con$user_id), authorized = TRUE, type = "application/json")
        listOfServices = listOfServices$services
        
        table = .listObjectsToDataFrame(listOfServices)
        
        if (isNamespaceLoaded("tibble")) 
            table = tibble::as_tibble(table)
        
        
        return(table)
    }), error = .capturedErrorToMessage)
}

#' Prepares and publishes a service on the back-end
#' 
#' The function will send a configuration object to the back-end to create a webservice from a job considering
#' additional parameter.
#' 
#' @param con connected and authenticated openeo clien object
#' @param type character the ogc web service type name to be created
#' @param graph A process graph object
#' @param title character (optional) the title in human readabled form for the service
#' @param description character (optional) the description for the service
#' @param enabled logical 
#' @param parameters a list of service creation parameters
#' @param plan character the billing plan
#' @param budget numeric the amount of credits that can be spent for this service
#' @return service representation as list
#' @export
create_service = function(con, type, graph, title = NULL, description = NULL, enabled = NULL, parameters = NULL, plan = NULL, budget = NULL) {
    tryCatch({
        if (is.null(type)) {
            stop("No type specified.")
        }
        
        if (missing(con)) {
            con = active_connection()
        }
        
        service_request_object = list(type = type, process_graph = graph$serialize(), title = title, description = description, enabled = enabled, parameters = parameters, 
            plan = plan, budget = budget)
        
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
#' @param con connected and authorized openeo client object
#' @param id the service id
#' @param type character the ogc web service type name to be created
#' @param process_graph list of processes composed as a process graph
#' @param title character (optional) the title in human readabled form for the service
#' @param description character (optional) the description for the service
#' @param enabled logical 
#' @param parameters a list of service creation parameters
#' @param plan character the billing plan
#' @param budget numeric the amount of credits that can be spent for this service
#' @return service representation as list
#' 
#' @export
update_service = function(con, id, type = NULL, process_graph = NULL, title = NULL, description = NULL, enabled = NULL, parameters = NULL, plan = NULL, budget = NULL) {
    
    tryCatch({
        patch = list()
        
        if (missing(con)) {
            con = active_connection()
        }
        
        if (!is.null(type)) {
            patch[["type"]] = type
        }
        
        if (!is.null(process_graph)) {
            if (length(process_graph) > 0) {
                patch[["process_graph"]] = process_graph
            } else {
                stop("Process graph cannot be set to be empty.")
            }
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
        
        if (!is.null(parameters)) {
            if (is.na(parameters)) {
                patch[["parameters"]] = NULL
            } else if (is.list(parameters)) {
                patch[["parameters"]] = parameters
            } else {
                stop("No valid data for parameter 'parameters'. It has to be a list")
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
        res = con$request(tag = tag, parameters = list(id), authorized = TRUE, encodeType = "json", data = patch)
        message(paste("Service '", id, "' was successfully updated.", sep = ""))
        invisible(TRUE)
    }, error = .capturedErrorToMessage)
}

#' Describes a service
#' 
#' Queries the server and returns information about a particular service
#' 
#' @param con connected and authorized openeo client object
#' @param id the service id
#' @return service as a list
#' @export
describe_service = function(con, id) {
    tryCatch({
        if (is.null(id)) {
            stop("No service id specified.")
        }
        
        if (missing(con)) {
            con = active_connection()
        }
        
        tag = "services_details"
        service = con$request(tag = tag, parameters = list(id), authorized = TRUE)
        class(service) = "ServiceInfo"
        return(service)
    }, error = .capturedErrorToMessage)
}

#' Deletes a service function for a job
#' 
#' Queries the back-end and removes the current set service function of job.
#' 
#' @param con connected and authorized openeo client object
#' @param id the service id
#' @export
delete_service = function(con, id) {
    tryCatch({
        tag = "services_delete"
        
        if (missing(con)) {
            con = active_connection()
        }
        
        msg = con$request(tag = tag, parameters = list(id), authorized = TRUE)
        message("Service '", id, "' successfully removed.")
        invisible(msg)
    }, error = .capturedErrorToMessage)
}
