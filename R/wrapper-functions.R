#' @include client.R
NULL

#' Returns the API version
#' 
#' This function returns information against which was developed in this R-client version.
#' 
#' @return character describing the API version
api.version = function() {
  message("This version is not directly compliant to API v0.0.1. It does not implement all intended functions.")
  return("0.0.2")
}

#' Connect to a openeEO backend
#'
#' connects to openEO backend
#' @param host URL pointing to the openEO server backend host
#' @param user the user name
#' @param password the password
#' @param rbackend logical to specify if the backend is the r test backend, default is FALSE
#'
#' @export
connect = function(host, user, password, rbackend=FALSE) {
  con = OpenEOClient$new()$connect(url=host)$login(user=user,password=password)
  con$is_rserver = rbackend
  return(con)
}


#' Authenticate
#'
#' @param con Connection object
#' @return authenticated Connection
#' @export
openeo.auth = function (con, ...) {

}

.listToDataFrame = function(list) {
  df = data.frame(stringsAsFactors = FALSE)
  for (index in 1:length(list)) {
    df = rbind(df,as.data.frame(list[[index]],stringsAsFactors=FALSE))
  }
  return(df)
}

#' List Data on conected server
#'
#' List available collections stored on a openEO server
#' @param con Connection object
#' @export
listCollections = function(con) {
  return(.listToDataFrame(con$listData()))
}

#' List available processes on server
#'
#' list all processes available on the backend
#' @param con Connection object
#' @return a list of lists with process_id and description
#' @export
listProcesses = function(con) {
  return(.listToDataFrame(con$listProcesses()))
}

#' List the jobs that a user has
#'
#' lists the jobs that a user has uploaded or in execution
#'
#' @param con the authenticated Connection
#' @export
listJobs = function(con) {
  return(con$listJobs())
}

#' Lists workspace files
#' 
#' Lists all files in the workspaces of the authenticated user.
#' 
#' @param con authorized connection
#' 
#' @return a list of lists with "name" and "size"
#' @export
listFiles = function(con) {
  return(.listToDataFrame(con$listUserFiles()))
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


#' @export
# dont't expose it later
taskToJSON = function(task) {
  return(toJSON(task,auto_unbox = T,pretty=T))
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
#' @param output the path, filename and extension, where to store the data
#' @return a connection to file if output was provided, the raw data if not
#' @export
executeTask = function(con,task,format,output=NULL) {
  con$execute(task,format,output)
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
  # return(con$executeTask(task,"lazy"))
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

      if ("collection_id" %in% names(process)) {
        arguments[[prior.name]] = process
      } else if ("process_id" %in% names(process)){
        arguments[[prior.name]] = process
      } else {
        stop("Chain corrupted. Prior element is neither process, udf nor collection")
      }
    }
  }

  res$args = append(arguments,additionalArgs)

  return(res)
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

  if (response$status_code != 200) {
    stop(paste("Upload of user data was not successful:",content(response)))
  } else {
    return(URLdecode(target))
  }
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


WCS = function() {

}
