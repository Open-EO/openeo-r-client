

#' Connect to a openeEO backend
#'
#' connects to openEO backend
#'
#' @export
connect = function() {

}


#' Authenticate
#'
#' @param con Connection object
#' @return authenticated Connection
#' @export
authenticate = function (con, ...) {

}

#' List Data on conected server
#'
#' List available collections stored on a openEO server
#' @param con Connection object
#' @export
listCollection = function(con, ...) {

}

#' List available processes on server
#'
#' list all processes available on the backend
#' @param con Connection object
#' @return a list of lists with process_id and description
#' @export
listProcesses = function(con, ...) {

}

#' List the jobs that a user has
#'
#' lists the jobs that a user has uploaded or in execution
#'
#' @param con the authenticated Connection
#' @return
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

}

#' Starts a job creation
#'
#' creates a job that can be extended by processes and udfs
#'
#' @return A Job object
#' @export
makeJob = function() {

}

#' Create a process object
#'
#' defines a process with arguments
#' @param process A process that might be chained with this process
#' @param process_id ID of the process offered by the connected openEO backend
#' @param ... named arguments that are passed to the process description
#' @export
process = function(process, process_id, ...) {

}

#' Executes a job directly and returns the data immediately
#'
#' Executes a job directly on the connected openEO backend and returns the data. It relates to
#' POST /api/jobs?evaluation="sync"
#'
#' @param con Connection
#' @param task A Process or chained processes to a Task
#' @param format The inteded format of the data to be returned
#' @returns Raw data in the specified format
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
#' @returns A named list or vector with "job_id" and "path" to the file in the users workspace
#' @export
orderResult = function(con, task, format, path) {

}

#' Stores a job on the backend for execution on demand
#'
#' Uploads a job to a server for lazy evaluation
#' on the server. It relates to POST /api/jobs?evaluation="batch"
#'
#' @param con Connection
#' @param task A Process or chained processes to a Task
#' @param format The inteded format of the data to be returned
#' @returns A named list or vector with "job_id"
#' @export
queueTask = function(con, task) {

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
#' @result Data in the requests format
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
#' @result a success / failure notification
#' @export
cancelJob = function(con, job_id) {

}

#' Fetches information about a job
#'
#' Returns a detailed description about a specified job. For example to check the status of a job.
#'
#' @param con authenticated Connection
#' @param job_id id of the job
#' @result a detailed description about the job
#' @export
queryJob = function(con,job_id) {

}

WCS = function() {

}
