#' @include client.R
NULL

# utility functions ----
.not_implemented_yet = function() {
  warning("Not implemented yet.")
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
  return(toJSON(task,auto_unbox = T,pretty=T))
}

#' Get a process graph builder from the connection
#' 
#' Queries the connected back-end for all available processes and registers them as R functions on
#' a process builder class to build up the process graph.
#' 
#' @param con a connection to an openeo back-end
#' @return a ProcessGraphBuilder class with the offered processes of the backend
#' @export
pgb = function(con) {
  con$getProcessGraphBuilder()
}

# server endpoint ----
#' Returns the API version
#' 
#' This function returns information against which was developed in this R-client version.
#' 
#' @return character describing the API version
#' @export
api.version = function() {
  return("0.0.2")
}

#' Returns the offered enpoints of the openEO API
#' 
#' The function queries the back-end for its capabilities. The offered enpoints that are specified in the openeo
#' API are then returned.
#' 
#' @param con A connected openeo client
#' @return data.frame containing the supported / implemented endpoints of the back-end
#' @export
listCapabilities = function(con) {
  capabilities = con$capabilities()
  if (is.list(capabilities)) {
    capabilities = unlist(capabilities)
  }
  return(data.frame(endpoints=capabilities,stringsAsFactors = FALSE))
}

#' Returns the output formats
#' 
#' The function queries the back-end for supported output formats.
#' 
#' @param connected openeo client object
#' @return list of formats with optional configuration parameter
#' @export
listFormats = function(con) {
  if (api.version() == "0.0.1") {
    return(.not_implemented_yet())
  }
  
  formats = con$output_formats()
  message(paste("Host uses '",formats$default,"' as default output format",sep=""))
  return(formats$formats)
}

#' Returns the offered webservice types of the back-end
#' 
#' The function queries the back-end for the supported webservice types that can be used on the client.
#' 
#' @param con a connected openeo client object
#' @return vector of identifier of supported webservice
#' @export
services = function(con) {
  return(con$services())
}

# login ----
#' Connect to a openeEO back-end
#'
#' connects to openEO back-end
#' @param host URL pointing to the openEO server back-end host
#' @param user the user name (optional)
#' @param password the password (optional)
#' @param rbackend logical to specify if the back-end is the r test back-end, default is FALSE
#' @param disable_auth flag to specify if the back-end supports authorization on its endpoints
#' @param auth_type the general authentication method used on all endpoints. Either "bearer" or "basic".
#'
#' @export
connect = function(host, user=NULL, password=NULL, rbackend=FALSE, disable_auth=FALSE, auth_type="bearer") {
  if (is.null(user) && is.null(password)) {
    con = OpenEOClient$new()$connect(url=host)
  } else if (!is.null(user) && !is.null(password)) {
    con = OpenEOClient$new()$connect(url=host)$login(user=user,password=password)  
  } else {
    message("Incomplete credentials. Either username or password is missing")
    return()
  }
  
  if (disable_auth) {
    con$disableAuth = TRUE
  }
  
  if (!disable_auth && !auth_type %in% c("basic","bearer")) {
    message("Unsupported authentication type. Use 'bearer' or 'basic' or disable the authentication")
    return()
  } else {
    con$general_auth_type = auth_type
  }
  
  con$is_rserver = rbackend
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
login = function(con, user, password) {
  return(con$login(user = user, password = password))
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
  return(con$listData())
}

#' Describe a product
#' 
#' Queries an openeo back-end and retrieves a detailed description about one or more collections offered by the back-end
#' 
#' @param con Authentication object
#' @param collection_id id of a product/collection to be described
#' 
#' @return a list of detailed information about a product/collection
#' @export
describeCollection = function(con, collection_id=NA) {
  describeProduct = !missing(collection_id) && !is.na(collection_id)
  
  if (!describeProduct) {
    message("No or invalid collection id(s)")
    return()
  }
  if (length(collection_id) > 1) {
    return(lapply(collection_id,
                  function(pid) {
                    con$describeProduct(pid)
                  }
    ))
  } else {
    return(con$describeProduct(collection_id))
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
listProcesses = function(con) {
  return(con$listProcesses())
}

#' Describe a process
#'
#' Queries an openeo back-end and retrieves more detailed information about offered processes
#' @param con Authentication object
#' @param process_id id of a process to be described
#'
#' @return a list of detailed information
#' @export
describeProcess = function(con,process_id=NA) {
  describeProcess = !missing(process_id) && !is.na(process_id)
  
  if (!describeProcess) {
    message("No or invalid process_id(s)")
    return()
  }
  
  return(con$describeProcess(process_id))
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
listGraphs = function(con) {
  graphIds = con$listGraphs()
  
  return(unlist(graphIds))
}

#' Fetches the representation of a stored graph
#' 
#' The function queries the back-end for a specific user defined process graph
#' 
#' @param con connected and authenticated openeo client object
#' @param graph_id The id of a process graph on the back-end
#' @param user_id (optional) the user id from which user to fetch the process graph
#' 
#' @return the process graph as list
#' @export
describeGraph = function(con, graph_id, user_id=NULL) {
  return(con$describeGraph(graph_id, user_id))
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
deleteGraph = function(con, graph_id) {
  con$deleteGraph(graph_id)
}

#' Stores a graph on the back-end
#' 
#' Uploads the process graph information to the back-end and stores it for reuse.
#' 
#' @param con connected and authorized openeo client object
#' @param graph a process graph definition
#' @export
storeGraph = function(con, graph) {
  return(con$storeGraph(graph))
}

#' Replaces the current graph with a given
#' 
#' Upload a process graph to the back-end under a given (existing) process graph.
#' 
#' @param con connected and authorized openeo client object
#' @param graph_id process graph id
#' @param graph a process graph definition created by chaining "process()", "collection()" or using a ProcessGraphBuilder
#' @export
replaceGraph = function(con, graph_id, graph) {
  return(con$replaceGraph(graph_id=graph_id, graph=graph))
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
listServices = function(con) {
  return(con$listServices())
}

#' Prepares and publishes a service on the back-end
#' 
#' The function will send a configuration object to the back-end to create a webservice from a job considering
#' additional parameter.
#' 
#' @param con connected and authenticated openeo clien object
#' @param job_id the job id to create the service from
#' @param service_type one of the supported services (see services())
#' @param ... additional parameter which are send as 'service_args'
#' @return service representation as list
#' @export
defineService = function(con, job_id, service_type, ...) {
  return(con$createService(job_id = job_id, service_type = service_type, ...))
}

#' Modifies a service
#' 
#' @param con connected and authorized openeo client object
#' @param service_id the service id
modifyService = function(con, service_id, ...) {
  .not_implemented_yet()
}

#' Describes a service
#' 
#' Queries the server and returns information about a particular service
#' 
#' @param con connected and authorized openeo client object
#' @param service_id the service id
#' @return service as a list
#' @export
describeService = function(con, service_id) {
  return(con$describeService(service_id))
}

#' Deletes a service function for a job
#' 
#' Queries the back-end and removes the current set service function of job.
#' 
#' @param con connected and authorized openeo client object
#' @param service_id the service id
deleteService = function(con, service_id) {
  return(con$deleteService(service_id))
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
listFiles = function(con) {
  return(con$listUserFiles())
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
uploadUserData = function (con, content, target,encode="raw",mime="application/octet-stream") {

    if (missing(content)) {
      stop("Content data is missing")
    }
    if (is.character(content)) {
      content = file.path(content)
    }
    if (!file.exists(content)) {
      stop(paste("Cannot find file at ",content))
    }
    
    response = con$uploadUserFile(content,target,encode=encode,mime=mime)
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
downloadUserData = function(con, src, dst=NULL) {
  return(con$downloadUserFile(src,dst))
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
#' Executes a job directly on the connected openEO back-end and returns the data. It relates to
#' POST /api/execute in v0.0.2. During the execution phase the connection to the server remains open.
#'
#' @param con connected and authenticated openeo client
#' @param task A Process or chained processes to a Task
#' @param format The inteded format of the data to be returned
#' @param output_file Where to store the retrieved data under
#' @param ... additional configuration parameter for output generation
#' @return a connection to file if output was provided, the raw data if not
#' @export
executeTask = function(con,task,format=NULL,output_file=NULL, ...) {
  con$execute(task=task,
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
#' @param task A Process or chained processes to a Task
#' @param graph_id The id of an already stored process graph on the same back-end
#' @param format The inteded format of the data to be returned
#' @param ... additional configuration parameter for output generation
#' 
#' @return the id of the job
#' @export
defineJob = function(con,task=NULL, graph_id=NULL , format=NULL, ...) {
  return(con$storeJob(task=task,graph_id = graph_id, format = format, ...))
}

#' Queues a job on the back-end for evaluation
#' 
#' The function will queue a job for evaluation that has been created before by 'createJob'. There will be
#' a notification for success or failure, but no return value.
#' 
#' @param con connected and authenticated openeo client
#' @param job_id the job id of a created job
#' 
#' @export
queueJob = function(con, job_id) {
  return(con$queue(job_id))
}

#' Defines a job and starts the evaluation
#' 
#' Function to execute two operations in one call. First define a job on the server, then call
#' the endpoint to execute it immediately on the back-end.
#' 
#' @param con connected and authenticated openeo client
#' @param task A Process or chained processes to a Task
#' @param graph_id The id of an already stored process graph on the same back-end
#' @param format The inteded format of the data to be returned
#' @param ... additional configuration parameter for output generation
#' 
#' @return the job_id of the defined job
#' @export 
orderResult = function(con, task=NULL, graph_id=NULL, format, ...) {
  job_id = con$storeJob(task=task,
                        graph_id = graph_id,
                        format = format,
                        ...)
  processing = con$queue(job_id)
  
  return(job_id)
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
modifyJob = function(con, job_id, ...) {
  temp = con$modifyJob(job_id = job_id,...)
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
followJob = function(con, job_id) {
 .not_implemented_yet()
}

#' Creates a list of download paths
#' 
#' The function queries the back-end to receive the URLs to the downloadable files of a particular batch job.
#' 
#' @param con connected and authenticated openeo client object
#' @param job_id the id of the job
#' @param format optional format of the output result, in case it shall differ from the one stated in the job
#' 
#' @return vector of URLs
#' @export
downloadJob = function(con, job_id, format=NULL) {
  return(con$results(job_id=job_id,format=format))
}

#' Pauses a running batch job
#'
#' Sends a request to the server in order to pause a running batch job.
#'
#' @param con authenticated Connection
#' @param job_id id of job that will be canceled
#' @return a success / failure notification
#' @export
pauseJob = function(con, job_id) {
  return(con$pause(job_id))
}


#' Terminates a running job
#'
#' Informs the server that the specified job needs to be terminated and taken "canceled" to prevent from
#' further executions and related costs.
#'
#' @param con authenticated Connection
#' @param job_id id of job that will be canceled
#' @return a success / failure notification
#' @export
cancelJob = function(con, job_id) {
    return(con$cancel(job_id))
}

#' Fetches information about a job
#'
#' Returns a detailed description about a specified job. For example to check the status of a job.
#'
#' @param con authenticated Connection
#' @param job_id id of the job
#' @return a detailed description about the job as tibble
#' @export
describeJob = function(con,job_id) {
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

#' Lists the supported UDF runtimes
#' 
#' The function queries the back-end for its supported udf runtimes and returns detailed information about each
#' runtime.
#' 
#' @param con connected and authenticated openeo client object
#' @return list of udf runtimes with supported udf types, versions and installed packages
#' @export
udfRuntimes = function(con) {
  return(con$udf_runtimes())
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
