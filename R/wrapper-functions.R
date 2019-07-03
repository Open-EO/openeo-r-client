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
taskToJSON = function(task) {
  #task is a Graph object
  
  if ("Graph" %in% class(task)) {
    return(toJSON(task$serialize(),auto_unbox = T,pretty=T,force=TRUE))
  } else {
    stop("Task is no Graph object.")
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
  con$process_graph_builder()
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
    return()
  }
  if (length(id) > 1) {
    return(lapply(id,
                  function(cid) {
                    con$describe_collection(cid)
                  }
    ))
  } else {
    return(con$describe_collection(id))
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
    return()
  }
  
  return(con$describe_process(id))
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
  return(con$describe_process_graph(id))
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
  con$delete_process_graph(graph_id)
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
  return(con$create_process_graph(graph,title = title, description = description))
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
  return(con$update_process_graph(graph_id=graph_id, graph=graph,title=title,description=description))
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
  return(con$validate_process_graph(graph))
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
  return(con$create_service(type = type, 
                           graph = graph,
                           title=title,
                           description = description,
                           enabled = enabled,
                           parameters = parameters,
                           plan = plan,
                           budget = budget))
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
  con$update_service(service_id = id,
                    type=type,
                    process_graph=process_graph,
                    title=title,
                    description=description,
                    enabled=enabled,
                    parameters=parameters,
                    plan=plan,
                    budget = budget)
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
  return(con$describe_service(id))
}

#' Deletes a service function for a job
#' 
#' Queries the back-end and removes the current set service function of job.
#' 
#' @param con connected and authorized openeo client object
#' @param id the service id
#' @export
delete_service = function(con, id) {
  return(con$delete_service(id))
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
    
    files = tibble(files) %>% rowwise() %>% summarise(name=files$name, size=files$size)
    
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
    
    response = con$upload_file(content,target,encode=encode,mime=mime)
    invisible(response)


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
  return(con$download_file(src,dst))
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
  con$delete_file(src = src)
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
  con$compute_result(graph=graph,
              format=format,
              output_file=output_file, 
              ...)
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
  return(con$create_job(graph=graph,graph_id = graph_id,
                      title = title, description = description,
                      plan = plan, budget = budget,
                      format = format, ...))
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
  con$start_job(job)
}

#' Modifies a job with given parameter
#' 
#' The function will mofidy a stored job with the given parameter. The dot parameter will contain all the values
#' that shall be replaced or removed. Shows a message of result or failure.
#' 
#' @details The '...' operator shall contain all the values that are to be replaced in the job. There are some reserved
#' keys. 
#' 'task' will replace the process graph with a newly defined one, therefore the task needs to be a list like in
#' createJob.
#' 'graph_id' will replace the process graph with the stated process graph id.
#' 'format' will change the desired output format.
#' All other parameter will be assumed to be special output parameter. Remember, you don't need to specify a task or graph_id,
#' e.g. if you just want to update the output format. 
#' To leave parameter unchanged, then don't mention it in the ... parameter. If you want to delete some, then set them to NULL.
#' 
#' @param con connected and authenticated openeo client
#' @param job_id the job id of a created job
#' @param ... The parameter you want to change. See Details for more information
#' @export
update_job = function(con, job_id,
                     title=NULL, description=NULL,
                     process_graph = NULL, 
                     plan = NULL, budget= NULL,
                     format=NULL, ...) {
  temp = con$update_job(job_id = job_id,
                       title=title, description=description,
                       process_graph = process_graph, 
                       plan = plan, budget= budget,
                       format=format, ...)
  invisible(temp)
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
    return(con$stop_job(job))
}

#' Fetches information about a job
#'
#' Returns a detailed description about a specified job. For example to check the status of a job.
#'
#' @param con authenticated Connection
#' @param id id of the job
#' @return a detailed description about the job
#' @export
describe_job = function(con,id) {
  return(con$describe_job(id))
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
  return(con$delete_job(job))
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
  return(con$estimate_job(job))
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
describeUdfType = function(con, language, type) {
  return(con$describeUdfRuntime)
}

#
# service functions? ----
#
WCS = function() {
  .not_implemented_yet()
}
