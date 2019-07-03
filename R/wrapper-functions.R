#' @include client.R
NULL

# utility functions ----
.not_implemented_yet = function() {
  warning("Not implemented yet.")
}

.not_supported_by_client = function() {
  warning("Not supported by the current client version.")
}

.not_supported_by_backend = function() {
  warning("The function is not supported by the current back-end version.")
}

#' Wrapper for toJSON
#' 
#' This function is intended to have a preconfigured toJSON function
#' to allow a user to visualize the process graph in JSON (like it will
#' be sent to the back-end)
#' 
#' @param task a list / nested list representing the process graph
#' @return JSON string of the process graph as a character string 
#' 
#' @export
# dont't expose it later
graphToJSON = function(graph) {
  #task is a Graph object
  
  if ("Graph" %in% class(graph)) {
    return(toJSON(graph$serialize(),auto_unbox = T,pretty=T,force=TRUE))
  } else {
    stop("Parameter is no Graph object.")
    invisible(NULL)
  }
  
}

#' Get a process graph builder from the connection
#' 
#' Queries the connected back-end for all available processes and registers them as R functions on
#' a process builder class to build up the process graph.
#' 
#' @param con a connection to an openeo back-end
#' @return a ProcessGraphBuilder class with the offered processes of the backend
#' @export
process_graph_builder = function(con) {
  tryCatch({
    if (is.null(con$processes)) {
      temp=list_processes(con)
    }
    
    collections = list_collections(self)$collections
    cids = sapply(collections,function(coll)coll$id)
    collections = as.list(cids)
    names(collections) = cids
    
    
    plist = lapply(self$processes,processFromJson)
    
    return(Graph$new(plist,collections))
  },error=.capturedErrorToMessage)
}

# server endpoint ----
#' Returns the suppported OpenEO API versions
#' 
#' The function queries the back-end for its supported versions. The endpoint \link[https://open-eo.github.io/openeo-api/apireference/#tag/Capabilities/paths/~1.well-known~1openeo/get]{/.well-known/openeo} 
#' is called and the JSON result is coerced into a tibble.
#' 
#' @param url the url as String pointing to the base host of the back-end
#' 
#' @return a tibble containing all supported API versions of the back-end
#' @export
api_versions = function(url) {
  tryCatch({
    
      if (endsWith(url,"/")) url = substr(url, 1, nchar(url)-1)
      endpoint = "/.well-known/openeo"

      info = GET(url=paste(url,endpoint,sep="/"))
      if (info$status == 200) {
        vlist = content(info)
        class(vlist) = "VersionsList"
        return(as_tibble(vlist))
      } else {
        stop("Host is not reachable. Please check the stated URL.")
      }
      
  },error=.capturedErrorToMessage)
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
    
    capabilities = con$request(operation="GET",endpoint = endpoint,authorized = FALSE)
    class(capabilities) = "OpenEOCapabilities"
    return(capabilities)
  },
  error = .capturedErrorToMessage)
}

#' List the openeo endpoints
#' 
#' The client queries the version resolved back-end for its endpoint capabilities and returns it as
#' a tibble.
#' 
#' @param con A connected OpenEO client
#' 
#' @return tibble
#' 
#' @export
list_features = function(con) {
  return(con$api.mapping[c("endpoint","operation","available")])
}

#' Returns the output formats
#' 
#' The function queries the back-end for supported output formats.
#' 
#' @param connected openeo client object
#' @return list of formats with optional configuration parameter
#' @export
list_file_types = function(con) {
  tryCatch({
    tag = "formats"
    endpoint = con$getBackendEndpoint(tag)
    
    formats = con$request(operation="GET",endpoint=endpoint,authorized= FALSE)
    
    names = names(formats)
    datatypes = unname(lapply(formats, function(format){
      return(format$gis_data_types)
    }))
    
    parameters = unname(lapply(formats, function(format){
      return(format$parameters)
    }))
    
    table = tibble(format=names,type=datatypes,parameters = parameters)
    
    return(table)
  },
  error = .capturedErrorToMessage)
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
    endpoint = con$getBackendEndpoint(tag)
    
    services = con$request(operation="GET",endpoint, authorized = FALSE)
    
    updated_services = list()
    for (key in names(services)) {
      service = services[[key]]
      service$service = key
      
      updated_services = c(updated_services,list(service))
    }
    return(lapply(updated_services, function(service) {
      class(service) = "ServiceType"
      return(service)
    }))
  },error=.capturedErrorToMessage)
  
  return(con$list_service_types())
}

# login ----
#' Connect to a openeEO back-end
#'
#' Connects to openEO back-end. If the backend provides a well-known endpoint that allows for redirecting to
#' specific versions, then you should provide the versions parameter.
#' 
#' @details Especially the login_type and the authType suggested by the client development guidelines are confusing. Here the login_type deals
#' just with considered login. Meaning "basic" allows you to use username and password directly in the call, whereas "oidc" will
#' open up a browser window, where you enter you credentials. The authentication against all protected endpoints will later
#' use the bearer token that the client has obtained after the login, unless the authentication was dropped with NULL anyways.
#' 
#' @param host URL pointing to the openEO server back-end host
#' @param version the version number as string
#' @param user the user name (optional)
#' @param password the password (optional)
#' @param login_type either NULL, "basic" or "oidc". This refers to the login mechanism that shall be used. NULL disables authentication.
#'
#' @export
connect = function(host, version=NULL, user=NULL, password=NULL,login_type = NULL) {
  con = OpenEOClient$new()
  
  if (is.null(user) && is.null(password) && is.null(login_type)) {
    con = con$connect(url=host,version=version,login_type=login_type)
  } else if (login_type == "basic") {
    if (!is.null(user) && !is.null(password)) {
      con = con$connect(url=host,version=version,login_type=login_type)$login(user=user,password=password)  
    } else {
      con = con$connect(url=host,version=version,login_type=login_type)
    }
  } else if (login_type == "oidc") {
    con = con$connect(url=host,version=version,login_type=login_type)$login() 
  } else {
    message("Incomplete credentials. Either username or password is missing")
    return()
  }
  
  return(con)
}

#' Function to login to a specific backend
#' 
#' Retrieves the bearer-token from the backend by sending user name and password to the backend. This step
#' is usually also performed in the "connect" step. But if you only connected to a back-end in order to 
#' register, then you need to log in afterwards.
#' 
#' @param con connected back-end connection
#' @param user the user name
#' @param password the password
#' @return a connected and authenticated back-end connection
#' @export
login = function(con, user=NULL, password=NULL) {
  return(con$login(user = user, password = password))
}

#' Returns the client version
#' 
#' The function returns the client version.
#' 
#' @param con an OpenEO client
#' 
#' @return the client version
client_version = function() {
  return("0.4.1")
}

#' Retrieves the current users account information
#' 
#' Calls endpoint /me to fetch the user account information of the user that is currently logged in to the back-end
#' 
#' @param con authenticated client object
#' @return object of type user
#' @export
describe_account = function(con) {
  tryCatch({
    tag = "user_info"
    endpoint = con$getBackendEndpoint(tag)
    
    user_info = con$request(operation="GET",endpoint=endpoint,authorized = TRUE,type="application/json")
    
    class(user_info) = "User"
    return(user_info)
    
  },
  error = .capturedErrorToMessage)
}

# data endpoint ----
#' List Data on conected server
#'
#' List available collections stored on a openEO server
#' @param con Connection object
#' @export
list_collections = function(con) {
  
  tryCatch({
    tag = "data_overview"
    endpoint = con$getBackendEndpoint(tag)
    
    listOfProducts = con$request(operation="GET",endpoint=endpoint,type="application/json")
    class(listOfProducts) = "CollectionList"
    return(listOfProducts)
  },
  error=.capturedErrorToMessage)
  
}

#' Describe a product
#' 
#' Queries an openeo back-end and retrieves a detailed description about one or more collections offered by the back-end
#' 
#' @param con Authentication object
#' @param id id of a product/collection to be described
#' 
#' @return a list of detailed information about a product/collection
#' @export
describe_collection = function(con, id=NA) {
  missing_id = !missing(id) && !is.na(id)
  
  if (!missing_id) {
    message("No or invalid collection id(s)")
    invisible(NULL)
  }
  if (length(id) > 1) {
    return(lapply(id,
                  function(cid) {
                    describe_collection(con,cid)
                  }
    ))
  } else {
    tryCatch({
      tag = "data_details"
      endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
      
      info = con$request(operation="GET",endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)
      
      class(info) = "CollectionInfo"
      
      if (!is.null(info$properties$`eo:bands`)) {
        class(info$properties$`eo:bands`) = "BandList"
      }
      
      class(info$properties$`cube:dimensions`) = "CubeDimensions"
      
      return(info)
    },
    error = .capturedErrorToMessage)
  }
}


#
# processes endpoint ----
# 

#' List available processes on server
#'
#' list all processes available on the back-end
#' @param con Connection object
#' @return a list of lists with process_id and description
#' @export
list_processes = function(con) {
  tryCatch({
    if (is.null(con$processes)) {
      tag = "process_overview"
      endpoint = con$getBackendEndpoint(tag)
      
      listOfProcesses = con$request(operation="GET",endpoint=endpoint,type="application/json")
      con$processes = listOfProcesses$processes
      
      names(con$processes) = sapply(con$processes,function(p)p$id)
    }
    
    return(lapply(con$processes,function(process) {
      class(process) = "ProcessInfo"
      return(process)
    }))
  },
  error=.capturedErrorToMessage)
}

#' Describe a process
#'
#' Queries an openeo back-end and retrieves more detailed information about offered processes
#' @param con Authentication object
#' @param id id of a process to be described
#'
#' @return a list of detailed information
#' @export
describe_process = function(con,id=NA) {
  describeProcess = !missing(id) && !is.na(id)
  
  if (!describeProcess) {
    message("No or invalid process_id(s)")
    invisible(NULL)
  }
  
  if (is.null(con$processes)) {
    message("No processes found or loaded from the back-end")
    invisible(NULL)
  }
  
  if (! id %in% names(con$processes)) {
    message(paste("Cannot describe process '",id,"'. Process does not exist.",sep=""))
    invisible(NULL)
  } else {
    return(con$processes[[id]])
  }
}

#
# process graph endpoint ----
#

#' Lists the Ids of the process graphs from the current user
#' 
#' Queries the back-end to retrieve a list of graph ids that the current user has stored on the back-end.
#' 
#' @param con connected and authenticated openeo client object
#' @return vector of process graph ids
#' @export
list_process_graphs = function(con) {
  tryCatch({
    tag = "graph_overview"
    endpoint = con$getBackendEndpoint(tag)
    
    listOfGraphShortInfos = con$request(operation="GET",endpoint=endpoint, authorized = TRUE)
    listOfGraphShortInfos = listOfGraphShortInfos$process_graphs
    
    table = tibble(id=character(),
                   title=character(),
                   description=character())
    
    if (length(listOfGraphShortInfos) > 0) {
      
      for (index in 1:length(listOfGraphShortInfos)) {
        graph_short = listOfGraphShortInfos[[index]]
        id = graph_short$id
        title = NA
        if (!is.null(graph_short$title)) title = graph_short$title
        description = NA
        if (!is.null(graph_short$description)) description = graph_short$description
        
        table= add_row(table,
                       id=id,
                       title = title,
                       description = description)
      }
    }
    
    return(table)
  }, error = .capturedErrorToMessage)
}

#' Fetches the representation of a stored graph
#' 
#' The function queries the back-end for a specific user defined process graph
#' 
#' @param con connected and authenticated openeo client object
#' @param id The id of a process graph on the back-end
#' 
#' @return the process graph as list
#' @export
describe_process_graph = function(con, id) {
  tryCatch(
    {
      if (is.null(id)) {
        stop("No graph id specified. Cannot fetch unknown graph.")
      }
      
      tag = "graph_details"
      endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
      graph = con$request(operation="GET",endpoint=endpoint, authorized = TRUE, type="application/json",auto_unbox=TRUE)
      
      class(graph) = "ProcessGraphInfo"
      class(graph$process_graph) = "Json_Graph"
      return(graph)
    },
    error=.capturedErrorToMessage
  )
}

#' Deletes a previously stored process graph
#' 
#' The function initiates the deletion of a process graph on the back-end. Only the owning user can delete
#' a graph. The graph also should not be part of any particular job.
#' 
#' @param con connected and authorized openeo client object
#' @param graph_id the id of the graph
#' 
#' @export
delete_process_graph = function(con, graph_id) {
  tryCatch({
    tag = "graph_delete"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
    
    success = con$request(operation="DELETE",endpoint = endpoint, authorized = TRUE)
    if(success) {
      message(paste("Graph '",id,"' was successfully deleted from the back-end",sep=""))
    }
    return(success)
  },error=.capturedErrorToMessage)
}

#' Stores a graph on the back-end
#' 
#' Uploads the process graph information to the back-end and stores it for reuse.
#' 
#' @param con connected and authorized openeo client object
#' @param graph a process graph definition
#' @param title the title of the process graph (optional)
#' @param description the description of a process graph (optional)
#' @export
create_process_graph = function(con, graph, title = NULL, description = NULL) {
  tryCatch({
    tag = "new_graph"
    endpoint = con$getBackendEndpoint(tag)
    
    if (!"Graph" %in% class(graph) || is.null(graph)) {
      stop("The graph information is missing or not a list")
    }
    
    requestBody = list(
      title=title,
      description = description,
      process_graph = graph$serialize()
    )
    
    response = con$request(operation="POST",
                           endpoint=endpoint,
                            authorized = TRUE,
                            data=requestBody,
                            raw=TRUE)
    
    message("Graph was sucessfully stored on the backend.")
    locationHeader = headers(response)$location
    split = unlist(strsplit(locationHeader,"/"))
    return(trimws(split[length(split)]))
  },error = .capturedErrorToMessage)
}

#' Modify the current graph with a given
#' 
#' Upload a process graph to the back-end under a given (existing) process graph.
#' 
#' @param con connected and authorized openeo client object
#' @param graph_id process graph id
#' @param graph a process graph definition created by chaining "process()", "collection()" or using a ProcessGraphBuilder
#' @param title title of the process graph (optional)
#' @param description description of the process graph (optional)
#' @export
update_process_graph = function(con, graph_id, graph=NULL,title=NULL,description=NULL) {
  tryCatch({
    if (is.null(id)) {
      stop("Cannot replace unknown graph. If you want to store the graph, use 'create_process_graph' instead")
    }
    
    requestBody = list()
    
    if (!is.null(graph)) {
      if (is.na(graph)) {
        stop("Cannot remove process graph from the element. Please replace it with another process graph, or ignore it via setting NULL")
      }else if (!is.list(graph)) {
        stop("The graph information is missing or not a list")
      } else {
        requestBody[["process_graph"]] = graph
      }
    }
    
    if (!is.null(title)) {
      if (is.na(title)) {
        requestBody[["title"]] = NULL
      } else {
        requestBody[["title"]] = title
      }
    }
    if (!is.null(description)) {
      if (is.na(description)) {
        requestBody[["description"]] = NULL
      } else {
        requestBody[["description"]] = description
      }
    }
    
    tag = "graph_replace"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
    
    message = con$request(operation="PATCH",endpoint = endpoint, 
                            authorized = TRUE, 
                            data = requestBody,
                            encodeType = "json")
    
    if (is.null(message)) {
      message(paste("Process graph '",graph_id,"' was successfully modified.",sep=""))
      invisible(TRUE)
    }
  },error=.capturedErrorToMessage)
}

#' Validates a process graph
#' 
#' Sends the process graph to the back-end and validates it against the offered processes by the backend.
#' 
#' @param con connected and authorized openeo client object
#' @param graph the process graph that will be sent to the back-end and is being validated
#' 
#' @export
validate_process_graph = function(con, graph) {
  tryCatch({
    tag = "process_graph_validate"
    endpoint = con$getBackendEndpoint(tag)
    
    if ("Graph" %in% class(graph)) graph = graph$serialize()
    
    if (!is.list(graph) || is.null(graph)) {
      stop("The graph information is missing or not a list")
    }
    
    requestBody = list(
      process_graph = graph
    )
    
    response = con$request(operation="POST",endpoint=endpoint,
                            authorized = TRUE,
                            data=requestBody,
                            encodeType = "json")
    
    message("Graph was sucessfully validated.")
    invisible(response)
  },error = .capturedErrorToMessage)
}

#
# services endpoint ----
#

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
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(con$user_id)
    
    listOfServices = con$request(operation="GET",endpoint=endpoint,authorized = TRUE ,type="application/json")
    listOfServices = listOfServices$services
    
    table = tibble(id=character(),
                   title=character(),
                   description=character(),
                   url = character(),
                   type = character(),
                   enabled = logical(),
                   submitted = .POSIXct(character(0)),
                   plan = character(0),
                   costs = numeric(0),
                   budget = numeric(0))
    
    if (length(listOfServices) > 0) {
      for (index in 1:length(listOfServices)) {
        service = listOfServices[[index]]
        
        id = NA
        if (!is.null(service$id)) id = service$id
        
        title = NA
        if (!is.null(service$title)) title = service$title
        
        description = NA
        if (!is.null(service$description)) description = service$description
        
        url = NA
        if (!is.null(service$url)) url = service$url
        
        type = NA
        if (!is.null(service$type)) type = service$type
        
        enabled = NA
        if (!is.null(service$enabled)) enabled = service$enabled
        
        submitted = NA
        if (!is.null(service$submitted)) submitted = as_datetime(service$submitted)
        
        plan = NA
        if (!is.null(service$plan)) plan = service$plan
        
        costs = NA
        if (!is.null(service$costs)) costs = as.numeric(service$costs)
        
        budget = NA
        if (!is.null(service$budget)) budget = as.numeric(service$budget)
        
        table= add_row(table, 
                       id=id,
                       title=title,
                       description=description,
                       url = url,
                       type = type,
                       enabled=enabled,
                       submitted=submitted,
                       plan = plan,
                       costs = costs,
                       budget=budget)
      }
    }
    
    
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
#' @param process_graph list of processes composed as a process graph
#' @param title character (optional) the title in human readabled form for the service
#' @param description character (optional) the description for the service
#' @param enabled logical 
#' @param parameters a list of service creation parameters
#' @param plan character the billing plan
#' @param budget numeric the amount of credits that can be spent for this service
#' @return service representation as list
#' @export
create_service = function(con, 
                         type, 
                         graph,
                         title = NULL,
                         description = NULL,
                         enabled = NULL,
                         parameters = NULL,
                         plan = NULL,
                         budget = NULL) {
  tryCatch({
    if (is.null(type)) {
      stop("No type specified.")
    }
    
    tag = "service_publish"
    endpoint = con$getBackendEndpoint(tag)
    
    service_request_object = list(
      type = type,
      process_graph = graph$serialize(),
      title = title,
      description = description,
      enabled =enabled,
      parameters = parameters,
      plan = plan,
      budget = budget
    )
    
    response = con$request(operation="POST",
                           endpoint=endpoint,
                            authorized = TRUE, 
                            data = service_request_object, 
                            encodeType = "json",
                            raw = TRUE)
    
    message("Service was successfully created.")
    locationHeader = headers(response)$location
    split = unlist(strsplit(locationHeader,"/"))
    return(trimws(split[length(split)]))
  },error=.capturedErrorToMessage)
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
update_service = function(con, id, 
                         type=NULL, 
                         process_graph=NULL,
                         title = NULL,
                         description = NULL,
                         enabled = NULL,
                         parameters = NULL,
                         plan = NULL,
                         budget = NULL) {
  tag = "services_update"
  endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
  tryCatch({
    patch = list()
    
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
    
    res = con$request(operation="PATCH",endpoint = endpoint,
                        authorized = TRUE,
                        encodeType = "json",
                        data=patch)
    message(paste("Service '",service_id,"' was successfully updated.",sep=""))
    invisible(TRUE)
  },error=.capturedErrorToMessage)
}

#' Describes a service
#' 
#' Queries the server and returns information about a particular service
#' 
#' @param con connected and authorized openeo client object
#' @param service_id the service id
#' @return service as a list
#' @export
describe_service = function(con, id) {
  tryCatch({
    if (is.null(id)) {
      stop("No service id specified.")
    }
    
    tag = "services_details"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
    
    service = con$request(operation="GET",endpoint=endpoint,authorized = TRUE)
    class(service) = "ServiceInfo"
    return(service)
  }, error=.capturedErrorToMessage)
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
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
    
    msg = con$request(operation="DELETE",endpoint = endpoint,
                         authorized = TRUE)
    message("Service '",id,"' successfully removed.")
    invisible(msg)
  },error=.capturedErrorToMessage)
}

#
# user endpoint ----
#

#' Lists workspace files
#' 
#' Lists all files in the workspaces of the authenticated user.
#' 
#' @param con authorized connection
#' 
#' @return a tibble of for filenames and their sizes
#' @export
list_files = function(con) {
  tryCatch({
    tag = "user_files"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(con$user_id)
    
    files = con$request(operation="GET",endpoint,TRUE,type="application/json")
    files = files$files
    
    if (is.null(files) || length(files) == 0) {
      message("The user workspace at this host is empty.")
      return(invisible(files))
    }
    
    files = tibble(files) %>% rowwise() %>% summarise(path=files$path, size=files$size, modified=files$modified)
    
    return(files)
  },error=.capturedErrorToMessage)
}


#' Uploads data into the users workspace
#'
#' This function sends the file given by 'content' to the specified target location (relative file path in the
#' user workspace) on the back-end.
#'
#' @param con authorized Connection
#' @param content the file path of the file to be uploaded
#' @param target the relative server path location for the file
#' @param encode the encoding type used to upload the data, e.g. "multipart","form","json","raw" ("raw" by default)
#' @param mime mime type used in upload_file ("application/octet-stream" as a default)
#' 
#' @return the relative file path on the server
#' @export
upload_file = function (con, content, target,encode="raw",mime="application/octet-stream") {

    if (missing(content)) {
      stop("Content data is missing")
    }
    if (is.character(content)) {
      content = file.path(content)
    }
    if (!file.exists(content)) {
      stop(paste("Cannot find file at ",content))
    }
    
  tryCatch({
    target = URLencode(target,reserved = TRUE)
    target = gsub("\\.","%2E",target)
    
    if (is.null(con$user_id)) {
      stop("User id is not set. Either login or set the id manually.")
    }
    
    tag = "user_file_upload"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(con$user_id,target)
    
    m = con$request(operation="PUT",
                    endpoint= endpoint,
                    authorized = TRUE, 
                    data=httr::upload_file(content,type=mime),
                    encodeType = encode)
    message("Upload of user data was successful.")
    return(m)
  },error=.capturedErrorToMessage)


}

#' Downloads a file from the users workspace
#' 
#' Sends a request to an openeo back-end to access the users files and downloads them to a given location
#' 
#' @param con authorized connection
#' @param src the relative filepath of the source file on the openeo back-end
#' @param dst the destination file path on the local file system
#' 
#' @return The file path of the stored file
#' @export
download_file = function(con, src, dst=NULL) {
  tryCatch({
    if (!is.character(src)) {
      stop("Cannot download file with a source statement that is no character")
    } else {
      src = .urlHardEncode(src)
    }
    
    if (is.null(dst)) {
      dst = tempfile()
    }
    
    tag = "user_file_download"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(con$user_id,src)
    
    file_connection = file(dst,open="wb")
    writeBin(object=con$request(operation="GET",endpoint=endpoint,
                                authorized = TRUE,as = "raw"),con = file_connection)
    
    message("Successfully downloaded the requested file.")
    
    return(dst)
  },error=.capturedErrorToMessage,
  finally= {
    close(file_connection,type="wb")
  })
}

#' Deletes a file from the users workspace
#'
#' Sends a request to an openeo back-end in order to remove a specific file from the users workspaces
#' 
#' @param con authorized connection
#' @param src the relative filepath of the source file on the openeo back-end that shall be deleted
#' 
#' @return logical
#' @export
delete_file = function(con, src) {
  tryCatch({
    if (is.character(src)) {
      src = .urlHardEncode(src)
    } else {
      stop("Cannot interprete parameter 'src' during delete request")
    }
    
    tag = "user_file_delete"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(con$user_id, src)
    
    return(con$request(operation="DELETE",endpoint = endpoint, authorized = TRUE))
  },error=.capturedErrorToMessage)
}

#
# jobs endpoint ----
#

#' List the jobs that a user has
#'
#' lists the jobs that a user has uploaded or in execution
#'
#' @param con the authenticated Connection
#' @export
list_jobs = function(con) {
  tryCatch({
    tag = "user_jobs"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(con$user_id)
    
    listOfJobs = con$request(operation="GET",endpoint=endpoint,authorized=TRUE,type="application/json")
    listOfJobs = listOfJobs$jobs
    # list to tibble
    table = tibble(id=character(),
                   title = character(),
                   status=character(),
                   submitted=.POSIXct(integer(0)),
                   updated=.POSIXct(integer(0)),
                   costs=integer(0),
                   budget=integer(0),
                   plan=character()
    )
    # desription left out on purpose... it might be to much to visualize
    
    if (length(listOfJobs) > 0) {
      for (index in 1:length(listOfJobs)) {
        job = listOfJobs[[index]]
        
        suppressWarnings({
          id = NA
          if (!is.null(job$id)) id = job$id
          title = NA
          if (!is.null(job$title)) title = job$title
          status = NA
          if (!is.null(job$status)) status = job$status
          submitted = NA
          if (!is.null(job$submitted)) submitted = as_datetime(job$submitted)
          updated = NA
          if (!is.null(job$updated)) updated = as_datetime(job$updated)
          costs = NA
          if (!is.null(job$costs)) costs = as.numeric(job$costs)
          budget = NA
          if (!is.null(job$budget)) budget = as.numeric(job$budget)
          plan = NA
          if (!is.null(job$plan)) plan = job$plan
          
          table= add_row(table,
                         id=id,
                         title = title,
                         status=status,
                         submitted=submitted,
                         updated=updated,
                         costs=costs,
                         budget=budget,
                         plan=plan)
        })
      }
    }
    
    return(table)
  },
  error=.capturedErrorToMessage)
}

#' Executes a job directly and returns the data immediately
#'
#' Executes a job directly on the connected openEO back-end and returns the data. It relates to
#' POST /api/execute in v0.0.2. During the execution phase the connection to the server remains open.
#'
#' @param con connected and authenticated openeo client
#' @param graph A process graph
#' @param format The inteded format of the data to be returned
#' @param output_file Where to store the retrieved data under
#' @param ... additional configuration parameter for output generation
#' @return a connection to file if output was provided, the raw data if not
#' @export
compute_result = function(con,graph,format=NULL,output_file=NULL, ...) {
  tryCatch({
    # former sync evaluation
    tag = "execute_sync"
    endpoint = con$getBackendEndpoint(tag)
    if (is.null(format)) {
      stop("Parameter \"format\" is not set. Please provide a valid format.")
    }
    
    output = list(...)
    output = append(output, list(format=format))
    
    if (is.null(graph)) stop("No process graph was defined. Please provide a process graph.")
    
    if ("Graph" %in% class(graph)) graph = graph$serialize()
    
    if (is.list(graph)) {
      job = toJSON(list(process_graph=graph,output = output),force=TRUE,auto_unbox = TRUE)
    } else {
      stop("Parameter graph is not a Graph object. Awaiting a list.")
    }
    
    res = con$request(operation="POST",
                      endpoint=endpoint,
                       authorized = TRUE, 
                       data=job,
                       encodeType = "json",
                       raw=TRUE)
    
    if (!is.null(output_file)) {
      tryCatch(
        {
          message("Task result was sucessfully stored.")
          writeBin(content(res,"raw"),output_file)
        },
        error = function(err) {
          stop(err)
        }
      )
      
      return(output_file)
    } else {
      return(content(res,"raw"))
    }
  },error=.capturedErrorToMessage)
}


#' Creates a job on the back-end from a prepared task
#' 
#' This function shall be called after the user defined a task for the back-end to create a job on the
#' back-end. Therefore the user sends the task (process graph) and the optional output specifications like
#' format and additional creation parameter by '...'. Afterwards ths user can decide to either execute the
#' job asynchronous or they can create a service from it.
#' 
#' @param con connected and authenticated openeo client
#' @param graph A Graph object
#' @param graph_id The id of an already stored process graph on the same back-end
#' @param format The inteded format of the data to be returned
#' @param ... additional configuration parameter for output generation
#' 
#' @return the id of the job
#' @export
create_job = function(con,graph=NULL, graph_id=NULL , 
                     title = NULL, description = NULL,
                     plan = NULL, budget = NULL,
                     format=NULL, ...) {
  tryCatch({
    tag = "jobs_define"
    endpoint = con$getBackendEndpoint(tag)
    
    create_options = list(...)
    output = list()
    output$format = format
    if (length(create_options) > 0) {
      output$parameters = create_options
    }
    
    if (!is.null(graph)) {
      if ("Graph" %in% class(graph)) {
        job = list(process_graph=graph$serialize(),output = output)
      } else if (is.list(graph)) {
        job = list(process_graph=toJSON(graph,force=TRUE),output = output)
      } else {
        stop("Parameter task is not a task object. Awaiting a list.")
      }
    } else if (! is.null(graph_id)) {
      job = list(process_graph=graph_id,output = output)
    } else {
      stop("No process graph was defined. Please provide either a process graph id or a process graph description.")
    }
    
    if (!is.null(title)) job$title = title
    if (!is.null(description)) job$description = description
    if (!is.null(plan)) job$plan = plan
    if (!is.null(budget)) job$budget = budget
    
    #endpoint,authorized=FALSE,data,encodeType = "json",query = list(),...
    response = con$request(operation="POST",
                           endpoint=endpoint,
                            authorized = TRUE,
                            data=job,
                            raw=TRUE)
    
    message("Job was sucessfully registered on the backend.")
    locationHeader = headers(response)$location
    split = unlist(strsplit(locationHeader,"/"))
    return(split[length(split)])
  },error=.capturedErrorToMessage)
}

#' Starts remote asynchronous evaluation of a job
#' 
#' The function sends a start signal to the backend in order to start processing the results
#' for a defined job.
#' 
#' @param con connected and authenticated openeo client
#' @param job the job object or the job id of the defined job
#' 
#' @return the job_id of the defined job
#' @export 
start_job = function(con, job) {
  if (!is.null(job) && "JobInfo" %in% class(job)) {
    job_id = job$id
  } else {
    job_id = job
  }
  
  tryCatch({
    if (is.null(job_id)) {
      stop("No job id specified.")
    }
    
    tag = "execute_async"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
    
    success = con$request(operation="POST",endpoint = endpoint, authorized = TRUE)
    message(paste("Job '",job_id,"' has been successfully queued for evaluation.",sep=""))
    
    invisible(success)
  },error=.capturedErrorToMessage)
}

#' Modifies a job with given parameter
#' 
#' The function will mofidy a stored job with the given parameter. The dot parameter will contain all the values
#' that shall be replaced or removed. Shows a message of result or failure.
#' 
#' @details The '...' operator shall contain all the values that are to be replaced in the job. There are some reserved
#' keys. 
#' 'process_graph' will replace the process graph with a newly defined one, therefore the process graph needs to be a Graph object.
#' 'format' will change the desired output format.
#' All other parameter will be assumed to be special output parameter. Remember, you don't need to specify a process graph or graph_id,
#' e.g. if you just want to update the output format. 
#' To leave parameter unchanged, then don't mention it in the ... parameter. If you want to delete some, then set them to NULL.
#' 
#' @param con connected and authenticated openeo client
#' @param job_id the job id of a created job
#' @param title update title for the job
#' @param description update description
#' @param process_graph a Graph object created with the process_graph_builder
#' @param plan replaces plan with the set value
#' @param budget replaces or sets the credits that can be spent at maximum
#' @param format the output format
#' @param ... The create options parameter you want to change. See Details for more information
#' @export
update_job = function(con, job_id,
                     title=NULL, description=NULL,
                     process_graph = NULL, 
                     plan = NULL, budget= NULL,
                     format=NULL, ...) {
  tryCatch({
    if (is.null(id)) {
      stop("No job i was specified.")
    }
    
    tag = "jobs_update"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
    
    patch = list()
    create_options = list(...)
    output = list()
    if (length(create_options) > 0) {
      output$parameters = create_options
    }
    if (!is.null(format)) output$format = format
    
    if (length(output) > 0) patch$output = output
    
    if (!is.null(process_graph)) {
      patch$process_graph = process_graph
    }
    
    if (!is.null(title)) {
      if (is.na(title)) patch$title = NULL
      else patch$title = title
    }
    
    if (!is.null(description)){
      if (is.na(description)) patch$description = NULL
      else patch$description = description
    } 
    
    if (!is.null(plan)) {
      if (is.na(plan)) patch$plan = NULL
      else patch$plan = plan
    }
    
    if (!is.null(budget)) {
      if (is.na(budget)) patch$budget = NULL
      else patch$budget = budget
    }
    
    res = con$request(operation="PATCH",endpoint = endpoint,
                        authorized = TRUE,
                        encodeType = "json",
                        data=patch)
    message(paste("Job '",job_id,"' was successfully updated.",sep=""))
    invisible(TRUE)
  },error=.capturedErrorToMessage)
} 

#' Follow an executed Job
#'
#' Opens up a websocket to the openEO back-end to fetch updates about a running job.
#'
#' @param con An authenticated connection
#' @param job_id the id of the job on the server the user wants to connect to
#' @return a WebSocket connection
#' @export
follow_job = function(con, job_id) {
 .not_implemented_yet()
}

#' Creates a list of download paths
#' 
#' The function queries the back-end to receive the URLs to the downloadable files of a particular job.
#' 
#' @param con connected and authenticated openeo client object
#' @param job the job object or the id of the job
#' 
#' @return result object containing of URLs for download
#' @export
list_results = function(con, job) {
  if (!is.null(job) && "JobInfo" %in% class(job)) {
    job_id = job$id
  } else {
    job_id = job
  }
  
  tryCatch({
    tag = "jobs_download"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
    
    listOfResults = con$request(operation="GET",endpoint=endpoint,authorized=TRUE,type="application/json")
    return(listOfResults)
  },error=.capturedErrorToMessage)
}

#' Downloads the results of a job into a specific folder
#' 
#' The function will fetch the results of a asynchronous job and will download all files stated in the links. The parameter
#' 'folder' will be the target location on the local computer.
#' 
#' @param con a connected and authenticated OpenEO connection
#' @param job job object or the job_id for which the results are fetched
#' @param folder a character string that is the target path on the local computer
#' 
#' @export
download_results = function(con, job, folder) {
  
  if (!dir.exists(folder)) dir.create(folder,recursive = TRUE)
  results = con %>% list_results(job)
  
  lapply(results$links, function(link){
    href = link$href
    type = link$type
    
    if (!folder %>% endsWith(suffix = "/")) folder = paste0(folder,"/")
    filename = basename(href)
    download.file(href,paste0(folder,filename),mode = "wb")
  })
  invisible(TRUE)
}

#' Terminates a running job
#'
#' Informs the server that the specified job needs to be terminated and taken "canceled" to prevent from
#' further executions and related costs.
#'
#' @param con authenticated Connection
#' @param job the job object or the id of job that will be canceled
#' @return a success / failure notification
#' @export
stop_job = function(con, job) {
  if (!is.null(job) && "JobInfo" %in% class(job)) {
    job_id = job$id
  } else {
    job_id = job
  }
  
  tryCatch({
    if (is.null(job_id)) {
      stop("No job id specified.")
    }
    
    tag = "jobs_cancel"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
    
    success = con$request(operation="DELETE",endpoint = endpoint, authorized = TRUE)
    if (success) {
      message(paste("Job '",job_id,"' has been successfully canceled.",sep=""))
    }
    
    return(success)
  },error=.capturedErrorToMessage)
}

#' Fetches information about a job
#'
#' Returns a detailed description about a specified job. For example to check the status of a job.
#'
#' @param con authenticated Connection
#' @param job the job object or the id of the job
#' @return a detailed description about the job
#' @export
describe_job = function(con,job) {
  if (!is.null(job) && "JobInfo" %in% class(job)) {
    job_id = job$id
  } else {
    job_id = job
  }
  
  tryCatch({
    tag = "jobs_details"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
    
    info = con$request(operation="GET",endpoint = endpoint,authorized = TRUE, type="application/json",auto_unbox=TRUE)
    
    class(info) = "JobInfo"
    class(info$process_graph) = "Json_Graph"
    
    return(info)
  },error=.capturedErrorToMessage)
}


#' Delete a job
#'
#' Deletes a job from the backend.
#'
#' @param con authenticated Connection
#' @param job the job or the id of the job
#' @return logical with state of success
#' @export
delete_job = function(con,job) {
  if (!is.null(job) && "JobInfo" %in% class(job)) {
    job_id = job$id
  } else {
    job_id = job
  }
  
  tryCatch({
    tag = "jobs_delete"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
    
    success = con$request(operation="DELETE",endpoint = endpoint, authorized = TRUE)
    if(success) {
      message(paste("Job '",job_id,"' was successfully deleted from the back-end",sep=""))
    }
    return(success)
  },error=.capturedErrorToMessage)
}


#' Estimates job costs
#'
#' Calls the back-end and asks for an approximation about the costs in money and how much time
#' will be required to finish the job and whether or not the job owners data download is already
#' included in the monetary costs.
#'
#' @param con authenticated Connection
#' @param job the job or the id of the job
#' @return JobCostsEstimation containing information how much money and time will be spent
#' @export
estimate_job = function(con,job) {
  if (!is.null(job) && "JobInfo" %in% class(job)) {
    job_id = job$id
  } else {
    job_id = job
  }
  
  tryCatch({
    if (is.null(job_id)) {
      stop("No job id specified.")
    }
    tag = "jobs_cost_estimation"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
    
    success = con$request(operation="GET",endpoint = endpoint, authorized = TRUE)
    class(success) = "JobCostsEstimation"
    return(success)
  },error=.capturedErrorToMessage)
}

#
# udf endpoint ----
#


#' Defines a UDF on the server
#'
#' This function will allow the user to define and uploads the script (content) into
#' the users workspace (target)
#'
#' @param con The authorized Connection
#' @param prior.name The parameter name of the predecessor of this pipe
#' @param type The udf type
#' @param content The local file path of a script the user wants to upload
#' @param target The relative path on the users workspace on the openEO back-end
#' @param language The programming language of the uploaded script
#' @param ...
#'
#' @return A named list that represents an UDF as list for the process graph
#' @export
defineUDF = function(process,con, prior.name="collections", language, type, content, target, ...) {
  if (!missing(con) && !missing(content)) {
    if (is.character(content)) {
      content = file.path(content)
    }
    if (!file.exists(content)) {
      message(paste("Cannot find file at ",content))
      return()
    }

    response = con$uploadUserFile(content,target)
  }



  # type check "process" either collection or process
  res = list()

  res$process_id = paste("/udf",language,type,sep="/")
  additionalArgs = list(...)

  arguments = list()
  arguments$script = target
  if (!missing(process) && !is.null(process)) {
    if (is.list(process)) {

      if (class(process) %in% c("collection","process","udf")) {
        arguments[[prior.name]] = process
      } else {
        stop("Chain corrupted. Prior element is neither process, udf nor collection")
      }
    }
  }

  res$args = append(arguments,additionalArgs)
  
  class(res) = "udf"

  return(res)
}

#' Lists the supported UDF runtimes
#' 
#' The function queries the back-end for its supported udf runtimes and returns detailed information about each
#' runtime.
#' 
#' @param con connected and authenticated openeo client object
#' @return list of udf runtimes with supported udf types, versions and installed packages
#' @export
list_udf_runtimes = function(con) {
  tryCatch(
    {
      tag = "udf_runtimes"
      endpoint = con$getBackendEndpoint(tag)
      
      return(con$request(operation="GET",endpoint = endpoint,
                         authorized = FALSE))
    }, 
    error = .capturedErrorToMessage
  )
}

#' Gets detailed information about a particular udf type
#' 
#' Queries the back-end for a particular runtime and time to retrieve information how the udf_type will work.
#' 
#' @param con connected and authenticated openeo client object
#' @param language the udf runtime identifier
#' @param type the udf type
#' @return list with udf runtime type information
#' @export
describe_udf_type = function(con, language, type) {
  tryCatch({
    if (is.null(language) || is.null(type)) {
      stop("Missing parameter language or udf_type")
    }
    tag = "udf_functions"
    endpoint = con$getBackendEndpoint(tag) %>% replace_endpoint_parameter(language,type)
    
    msg = con$request(operation="GET",endpoint = endpoint,
                      authorized = FALSE)
    return(msg)
  },error=.capturedErrorToMessage)
}

#
# service functions? ----
#
WCS = function() {
  .not_implemented_yet()
}
