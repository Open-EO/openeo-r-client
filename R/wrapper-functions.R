#' @include client.R
NULL

#' Connect to a openeEO backend
#'
#' connects to openEO backend
#' @param host URL pointing to the openEO server backend host
#' @param user the user name
#' @param password the password
#'
#' @export
connect = function(host, user, password) {
  return(OpenEOClient$new()$connect(url=host)$login(user=user,password=password))
}


#' Authenticate
#'
#' @param con Connection object
#' @return authenticated Connection
#' @export
openeo.auth = function (con, ...) {

}

#' List Data on conected server
#'
#' List available collections stored on a openEO server
#' @param con Connection object
#' @export
listCollections = function(con) {
  return(con$listData())
}

#' List available processes on server
#'
#' list all processes available on the backend
#' @param con Connection object
#' @return a list of lists with process_id and description
#' @export
listProcesses = function(con) {
  return(con$listProcesses())
}

#' List the jobs that a user has
#'
#' lists the jobs that a user has uploaded or in execution
#'
#' @param con the authenticated Connection
#' @export
listJobs = function(con) {

}

#' Describe a process or product
#'
#' retrieve more detailed information data and processes
#' @param con Authentication object
#' @param process_id id of a process to be described
#' @param product_id id of a product to be described
#'
#' @return a list of detailed information
#' @export
describe = function(con,process_id=NA, product_id=NA, ...) {
  describeProcess = !missing(process_id) && !is.na(process_id)
  describeProduct = !missing(product_id) && !is.na(product_id)

  if (describeProcess && !describeProduct) {
    return(lapply(process_id,
                  function(pid) {
                    con$describeProcess(pid)
                  }))
  } else if (describeProduct && !describeProcess) {
    return(lapply(product_id,
                  function(pid) {
                    con$describeProduct(pid)
                  }))
  } else {
    stop("Cannot distinguish whether to fetch process information or products")
  }
}

#' Create a process object
#'
#' defines a process with arguments
#' @param process A process that might be chained with this process
#' @param process_id ID of the process offered by the connected openEO backend
#' @param prior.name The name of the prior process / collection, default "collections"
#' @param ... named arguments that are passed to the process description
#' @export
process = function(process=NULL, process_id, prior.name="collections", ...) {
  # type check "process" either collection or process
  res = list()
  arguments = list()
  if (!missing(process) && !is.null(process)) {
    if (is.list(process)) {

      if ("collection_id" %in% names(process)) {
        arguments[[prior.name]] = process
      } else if ("process_id" %in% names(process)){
        arguments[[prior.name]] = process
      } else {
        stop("Chain corrupted. prior elemente is neither a process or a collection")
      }
    }
  }
  additionalParameter = list(...)

  res$process_id=process_id
  res$args = append(arguments,additionalParameter)

  return(res)

}

#' @export
# dont't expose it later
taskToJSON = function(task) {
  return(toJSON(task,auto_unbox = T,pretty=T))
}

#' A collection object
#'
#' creates a list represenation of a collection object
#' @param collection_id the id of the product
#' @return a list represenation for a collection / product
#' @export
collection = function(collection_id) {
  return(list(collection_id = collection_id))
}

#' Executes a job directly and returns the data immediately
#'
#' Executes a job directly on the connected openEO backend and returns the data. It relates to
#' POST /api/jobs?evaluation="sync"
#'
#' @param con Connection
#' @param task A Process or chained processes to a Task
#' @param format The inteded format of the data to be returned
#' @return Raw data in the specified format
#' @export
executeTask = function(con,task,format) {

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

}

#' Stores a job on the backend for execution on demand
#'
#' Uploads a job to a server for lazy evaluation
#' on the server. It relates to POST /api/jobs?evaluation="lazy"
#'
#' @param con Connection
#' @param task A Process or chained processes to a Task
#' @param format The inteded format of the data to be returned
#' @return A named list or vector with "job_id"
#' @export
queueTask = function(con, task) {
  return(con$executeTask(task,"lazy"))
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

}

#' Deletes a job on the server
#'
#' Deletes a job on the server
#' @param con An authenticated connection
#' @param job_id the id of the job on the server the user wants to connect to
#' @return A success notification
deleteJob = function(con, job_id) {

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

}


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
#'
#' @return A named list that represents an UDF as list for the process graph
#' @export
defineUDF = function(process,con, prior.name="collections", language, type, content, target, ...) {
  if (!missing(con) && !missing(content)) {
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

      if ("collection_id" %in% names(process)) {
        arguments[[prior.name]] = process
      } else if ("process_id" %in% names(process)){
        arguments[[prior.name]] = process
      } else {
        stop("Chain corrupted. prior elemente is neither a process or a collection")
      }
    }
  }


  res$args = append(arguments,additionalArgs)

  return(res)
}

WCS = function() {

}
