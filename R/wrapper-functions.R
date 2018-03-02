#' @include client.R
NULL

# utility functions ----
.not_implemented_yet = function() {
  warning("Not implemented yet.")
}

.listToDataFrame = function(list) {
  df = data.frame(stringsAsFactors = FALSE)
  for (index in 1:length(list)) {
    df = rbind(df,as.data.frame(list[[index]],stringsAsFactors=FALSE))
  }
  return(df)
}

#' Wrapper for toJSON
#' 
#' This function is intended to have a preconfigured toJSON function
#' to allow a user to visualize the process graph in JSON (like it will
#' be sent to the backend)
#' 
#' @param task a list / nested list representing the process graph
#' @return JSON string of the process graph as a character string 
#' 
#' @export
# dont't expose it later
taskToJSON = function(task) {
  return(toJSON(task,auto_unbox = T,pretty=T))
}

# server endpoint ----
#' Returns the API version
#' 
#' This function returns information against which was developed in this R-client version.
#' 
#' @return character describing the API version
#' @export
api.version = function() {
  message("This version is not directly compliant to API v0.0.1. It does not implement all intended functions.")
  return("0.0.2")
}

#' Returns the offered enpoints of the openEO API
#' 
#' The function queries the backend for its capabilities. The offered enpoints that are specified in the openeo
#' API are then returned.
#' 
#' @param con A connected openeo client
#' @return data.frame containing the supported / implemented endpoints of the backend
#' @export
capabilities = function(con) {
  capabilities = con$capabilities()
  if (is.list(capabilities)) {
    capabilities = unlist(capabilities)
  }
  return(data.frame(endpoints=capabilities))
}

#' Returns the output formats
#' 
#' The function queries the backend for supported output formats.
#' 
#' @param connected openeo client object
#' @return list of formats with optional configuration parameter
#' @export
formats = function(con) {
  if (api.version() == "0.0.1") {
    return(.not_implemented_yet())
  }
  
  formats = con$output_formats()
  message(paste("Host uses '",formats$default,"' as default output format",sep=""))
  return(formats$formats)
}

#' Returns the offered webservice types of the backend
#' 
#' The function queries the backend for the supported webservice types that can be used on the client.
#' 
#' @param con a connected openeo client object
#' @return vector of identifier of supported webservice
#' @export
services = function(con) {
  return(con$services())
}

# login ----
#' Connect to a openeEO backend
#'
#' connects to openEO backend
#' @param host URL pointing to the openEO server backend host
#' @param user the user name (optional)
#' @param password the password (optional)
#' @param rbackend logical to specify if the backend is the r test backend, default is FALSE
#' @param disable_auth flag to specify if the backend supports authorization on its endpoints
#' @param auth_type the general authentication method used on all endpoints. Either "bearer" or "basic".
#'
#' @export
connect = function(host, user=NULL, password=NULL, rbackend=FALSE, disable_auth=FALSE, auth_type="bearer") {
  if (is.null(user) && is.null(password)) {
    con = OpenEOClient$new()$connect(url=host)
  } else if (!is.null(user) && !is.null(password)) {
    con = OpenEOClient$new()$connect(url=host)$login(user=user,password=password)  
  } else {
    stop("Incomplete credentials. Either username or password is missing")
  }
  
  if (disable_auth) {
    con$disableAuth = TRUE
  }
  
  if (!disable_auth && !auth_type %in% c("basic","bearer")) {
    stop("Unsupported authentication type. Use 'bearer' or 'basic' or disable the authentication")
  } else {
    con$general_auth_type = auth_type
  }
  
  con$is_rserver = rbackend
  return(con)
}


#' Authenticate
#'
#' @param con Connection object
#' @return authenticated Connection
#' @export
openeo.auth = function (con, ...) {
  .not_implemented_yet()
}

# data endpoint ----
#' List Data on conected server
#'
#' List available collections stored on a openEO server
#' @param con Connection object
#' @export
listCollections = function(con) {
  return(.listToDataFrame(con$listData()))
}

#' Describe a product
#' 
#' Queries an openeo backend and retrieves a detailed description about one or more collections offered by the backend
#' 
#' @param con Authentication object
#' @param collection_id id of a product/collection to be described
#' 
#' @return a list of detailed information about a product/collection
#' @export
describeCollection = function(con, collection_id=NA) {
  describeProduct = !missing(collection_id) && !is.na(collection_id)
  
  if (!describeProduct) {
    stop("No or invalid collection id(s)")
  }
  
  return(lapply(collection_id,
                function(pid) {
                  con$describeProduct(pid)
                }
  ))
}

#
# processes endpoint ----
# 

#' List available processes on server
#'
#' list all processes available on the backend
#' @param con Connection object
#' @return a list of lists with process_id and description
#' @export
listProcesses = function(con) {
  return(.listToDataFrame(con$listProcesses()))
}

#' Describe a process
#'
#' Queries an openeo backend and retrieves more detailed information about offered processes
#' @param con Authentication object
#' @param process_id id of a process to be described
#'
#' @return a list of detailed information
#' @export
describeProcess = function(con,process_id=NA) {
  describeProcess = !missing(process_id) && !is.na(process_id)
  
  if (!describeProcess) {
    stop("No or invalid process_id(s)")
  }
  
  return(lapply(process_id,
                function(pid) {
                  con$describeProcess(pid)
                }
  ))
}

#
# process graph endpoint ----
#

#' Lists the Ids of the process graphs from the current user
#' 
#' Queries the backend to retrieve a list of graph ids that the current user has stored on the backend.
#' 
#' @param con connected and authenticated openeo client object
#' @return vector of process graph ids
#' @export
listGraphs = function(con) {
  graphIds = con$listGraphs()
  
  return(unlist(graphIds))
}

#' Fetches the representation of a stored graph
#' 
#' The function queries the backend for a specific user defined process graph
#' 
#' @param con connected and authenticated openeo client object
#' @param graph_id The id of a process graph on the backend
#' @param user_id (optional) the user id from which user to fetch the process graph
#' 
#' @return the process graph as list
#' @export
describeGraph = function(con, graph_id, user_id=NULL) {
  return(con$describeGraph(graph_id, user_id))
}

#' Deletes a previously stored process graph
#' 
#' The function initiates the deletion of a process graph on the backend. Only the owning user can delete
#' a graph. The graph also should not be part of any particular job.
#' 
#' @param con connected and authorized openeo client object
#' @param graph_id the id of the graph
#' 
#' @export
deleteGraph = function(con, graph_id) {
  con$deleteGraph(graph_id)
  message(paste("Graph '",graph_id,"' was successfully deleted from the backend",sep=""))
}

#' Stores a graph on the backend
#' 
#' Uploads the process graph information to the backend and stores it for reuse.
#' 
#' @param con connected and authorized openeo client object
#' @param graph a process graph definitions
#' @export
storeGraph = function(con, graph) {
  return(con$storeGraph(graph))
}

#
# services endpoint ----
#

#' Lists the current users services
#' 
#' Queries the backend to retrieve a list of services that the current user owns. Services are 
#' webservices like WCS, WFS, etc.
#' 
#' @param con connected and authenticated openeo client object
#' 
#' @return list of services lists
#' @export
listServices = function(con) {
  return(con$listServices())
}

#' Prepares and publishes a service on the backend
#' 
#' The function will send a configuration object to the backend to create a webservice from a job considering
#' additional parameter.
#' 
#' @param con connected and authenticated openeo clien object
#' @param job_id the job id to create the service from
#' @param service_type one of the supported services (see services())
#' @param ... additional parameter which are send as 'service_args'
#' @return service representation as list
#' @export
toService = function(con, job_id, service_type, ...) {
  return(con$toService(job_id = job_id, service_type = service_type, ...))
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
#' @return a data.frame of for filenames and their size
#' @export
listFiles = function(con) {
  return(.listToDataFrame(con$listUserFiles()))
}


#' Uploads data into the users workspace
#'
#' This function sends the file given by 'content' to the specified target location (relative file path in the
#' user workspace) on the backend.
#'
#' @param con authorized Connection
#' @param content the file path of the file to be uploaded
#' @param target the relative server path location for the file
#'
#' @return the relative file path on the server
#' @export
uploadUserData = function (con, content, target) {
  if (missing(content)) {
    stop("Content data is missing")
  }
  if (is.character(content)) {
    content = file.path(content)
  }
  if (!file.exists(content)) {
    stop(paste("Cannot find file at ",content))
  }
  
  response = con$uploadUserFile(content,target)
  message("Upload of user data was successful.")
  invisible(response)
}

#' Downloads a file from the users workspace
#' 
#' Sends a request to an openeo backend to access the users files and downloads them to a given location
#' 
#' @param con authorized connection
#' @param src the relative filepath of the source file on the openeo backend
#' @param dst the destination file path on the local file system
#' 
#' @return The file path of the stored file
#' @export
downloadUserData = function(con, src, dst=NULL) {
  return(con$downloadUserFile(src,dst))
}

#' Deletes a file from the users workspace
#'
#' Sends a request to an openeo backend in order to remove a specific file from the users workspaces
#' 
#' @param con authorized connection
#' @param src the relative filepath of the source file on the openeo backend that shall be deleted
#' 
#' @return logical
#' @export
deleteUserData = function(con, src) {
  con$deleteUserFile(src = src)
}

#' Returns the users available credits
#' 
#' Queries the server and returns the available credits of the current user.
#' @param con connected and authenticated openeo client object
#' @return the credits
#' @export
credits = function(con) {
  return(con$getUserCredits())
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
listJobs = function(con) {
  return(con$listJobs())
}

#' Executes a job directly and returns the data immediately
#'
#' Executes a job directly on the connected openEO backend and returns the data. It relates to
#' POST /api/jobs?evaluation="sync" in v0.0.1
#' POST /api/execute?format=GTiff in v0.0.2
#'
#' @param con Connection
#' @param task A Process or chained processes to a Task
#' @param format The inteded format of the data to be returned
#' @param ... additional configuration parameter for output generation
#' @return a connection to file if output was provided, the raw data if not
#' @export
executeTask = function(con,task,format, ...) {
  con$execute(task,format, ...)
}


#' Executes a job directly and stores it on the server
#'
#' Executes a job on a server directly and stores the data in the user workspace
#' on the server. It relates to POST /api/jobs?evaluation="batch"
#'
#' @param con Connection
#' @param task A Process or chained processes to a Task
#' @param format The inteded format of the data to be returned
#' @param path the relative path in the users workspace, where to store the data
#' @return A named list or vector with "job_id" and "path" to the file in the users workspace
#' @export
orderResult = function(con, task, format, path) {
  #TODO incorporate format and path -> probably extend task
  return(con$storeJob(task,"batch"))
}

#' Stores a job on the backend for execution on demand
#'
#' Uploads a job to a server for lazy evaluation
#' on the server. It relates to POST /api/jobs?evaluation="lazy"
#'
#' @param con Connection
#' @param task A Process or chained processes to a Task
#' @return the job_id
#' @export
queueTask = function(con, task) {
  return(con$storeJob(task,"lazy"))
}

#' Follow an executed Job
#'
#' Opens up a websocket to the openEO backend to fetch updates about a running job.
#'
#' @param con An authenticated connection
#' @param job_id the id of the job on the server the user wants to connect to
#' @return a WebSocket connection
#' @export
followJob = function(con, job_id) {
 .not_implemented_yet()
}

#' Deletes a job on the server
#'
#' Deletes a job on the server
#' @param con An authenticated connection
#' @param job_id the id of the job on the server the user wants to connect to
#' @return A success notification
#' 
#' @export
deleteJob = function(con, job_id) {
  con$deleteJob(job_id)
}

#' Downloads the result of job
#'
#' Download the result of a finished job
#' @param con authenticated Conenction
#' @param job_id id of the job
#' @param format specification about the format for the result
#' @return Data in the requests format
#' @export
downloadJob = function(con, job_id, format) {
  .not_implemented_yet()
}


#' Terminates a running job
#'
#' Informs the server that the specified job needs to be terminate and taken "canceled" to prevent from
#' further executions and related costs.
#'
#' @param con authenticated Connection
#' @param job_id id of job that will be canceled
#' @return a success / failure notification
#' @export
cancelJob = function(con, job_id) {
    .not_implemented_yet()
}

#' Fetches information about a job
#'
#' Returns a detailed description about a specified job. For example to check the status of a job.
#'
#' @param con authenticated Connection
#' @param job_id id of the job
#' @return a detailed description about the job
#' @export
queryJob = function(con,job_id) {
  return(con$describeJob(job_id))
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
#' @param target The relative path on the users workspace on the openEO backend
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
      stop(paste("Cannot find file at ",content))
    }

    response = con$uploadUserFile(content,target)
    if (response$status_code != 200) {
      warning("UDF upload failed")
    } else {
      cat("Successfully uploaded the udf script")
    }
  }



  # type check "process" either collection or process
  res = list()

  res$process_id = paste("/udf",language,type,sep="/")
  additionalArgs = list(...)

  arguments = list()
  arguments$script = target
  if (!missing(process) && !is.null(process)) {
    if (is.list(process)) {

      if (attr(process,"type") %in% c("collection","process","udf")) {
        arguments[[prior.name]] = process
      } else {
        stop("Chain corrupted. Prior element is neither process, udf nor collection")
      }
    }
  }

  res$args = append(arguments,additionalArgs)
  
  attr(res,"type") <- "udf"

  return(res)
}

#
# service functions? ----
#
WCS = function() {
  .not_implemented_yet()
}
