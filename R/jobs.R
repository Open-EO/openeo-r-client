#' @include debugging.R
NULL

# jobs endpoint ----

#' List the jobs that a user has
#'
#' lists the jobs that a user has uploaded or in execution
#'
#' @param con the authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @export
list_jobs = function(con=NULL) {
    tryCatch({
        con=.assure_connection(con)
        
        tag = "user_jobs"
        
        listOfJobs = con$request(tag = tag, parameters = list(con$user_id), authorized = TRUE, type = "application/json")
        
        listOfJobs = listOfJobs$jobs
        
        job_ids = sapply(listOfJobs, function(job) {
            job$id
        })
        
        listOfJobs = lapply(listOfJobs, function(job) {
            class(job) = "Job"
            
            return(job)
        })
        
        names(listOfJobs) = job_ids
        
        class(listOfJobs) = "JobList"
        
        
        return(listOfJobs)
    }, error = .capturedErrorToMessage)
}

#' Executes a job directly and returns the data immediately
#'
#' Executes a job directly on the connected openEO service and returns the data. During the execution phase the connection to 
#' the server remains open. The functions and enpoints main purpose is the debugging of code, where results can be immediately
#' checked. Please keep in mind, that computational functions might be related to monetary costs, if no 'free' plan is available. 
#' So make sure to keep the data selection relatively small, also some openEO service provider might offer limited processes support,
#' e.g. not supporting UDFs at this endpoint.
#'
#' @param graph A \code{\link{Graph}}, a function returning a \code{\link{ProcessNode}} as an endpoint or the \code{\link{ProcessNode}} 
#' will return the results
#' @param output_file Where to store the retrieved data under
#' @param budget numeric, how much to spend at maximum on testing
#' @param plan character, selection of a service plan
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param ... additional parameters passed to toJSON (like 'digits')
#' 
#' @return a connection to file if output was provided, the raw data if not
#' 
#' @importFrom methods as
#' @export
compute_result = function(graph, output_file = NULL, budget=NULL, plan=NULL, con=NULL, ...) {
    tryCatch({
        con = .assure_connection(con)
        
        output = list()
        
        if (is.null(graph)) 
            stop("No process graph was defined. Please provide a process graph.")
        
        if ("Graph" %in% class(graph))  {
            # OK
        } else if (is.list(graph)) {
            graph = parse_graph(json=graph)
        } else if (any(c("function","ProcessNode") %in% class(graph))){
            graph = as(graph, "Graph")
        } else {
            stop("Parameter graph is not a Graph object. Awaiting a list.")
        }
        
        process = Process$new(id=NA,description = NA,
                              summary = NA,process_graph=graph)
        
        job = list(
            process = process$serialize()
        )
        
        if (length(budget) > 0) {
            job$budget = budget
        }
        
        if (length(plan) > 0) {
            job$plan = plan
        }
        
        tag = "execute_sync"
        res = con$request(tag = tag, authorized = TRUE, data = job, encodeType = "json", raw = TRUE, ...)
        
        if (!is.null(output_file)) {
            tryCatch({
                message("Task result was sucessfully stored.")
                writeBin(content(res, "raw"), output_file)
            }, error = function(err) {
                stop(err)
            })
            
            return(output_file)
        } else {
            return(content(res, "raw"))
        }
    }, error = .capturedErrorToMessage)
}


#' Creates a job on the back-end from a Graph object
#' 
#' This function shall be called after the user defined a process graph for the back-end to create a job on the
#' back-end. Therefore the user sends the process graph and the optional output specifications like
#' format and additional creation parameter by '...'. To add some meta data about the job, the user might use title or 
#' description. By providing a execution plan and a maximum usable budget the user can change the execution behavior of the
#' back-end provider.
#' 
#' @param graph A \code{\link{Graph}}, a function returning a \code{\link{ProcessNode}} as an endpoint or the \code{\link{ProcessNode}} 
#' will return the results
#' @param title Optional title of a job to be found
#' @param description Optional a more detailed information about a job
#' @param plan An optional execution plan offered by the back-end, determining how the job will be executed
#' @param budget An optional budget, which sets the maximum amount of credits to be used by the job
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param ... additional parameters passed to toJSON (like 'digits')
#' 
#' @return the id of the job
#' @export
create_job = function(graph = NULL, title = NULL, description = NULL, plan = NULL, budget = NULL, con=NULL, ...) {
    tryCatch({
        con = .assure_connection(con)
        
        job = list()
        
        if (!is.null(title)) 
            job$title = title
        if (!is.null(description)) 
            job$description = description
        if (!is.null(plan)) 
            job$plan = plan
        if (!is.null(budget)) 
            job$budget = budget
        
        # build an empty process
        if (!is.null(graph)) {
            process = Process$new(id=NA,description = NA,
                                  summary = NA,
                                  process_graph=graph)
            job$process = process$serialize()
            
            job$process$process_graph=unclass(job$process$process_graph)
        } else {
            stop("No process graph was defined. Please provide either a process graph id or a process graph description.")
        }
        
        # endpoint,authorized=FALSE,data,encodeType = 'json',query = list(),...
        tag = "jobs_define"
        
        response = con$request(tag = tag, authorized = TRUE, data = job, raw = TRUE, ...)
        
        message("Job was sucessfully registered on the backend.")
        
        job_id = headers(response)$`openeo-identifier`
        
        job = describe_job(job = job_id, con=con)
        
        return(job)
    }, error = .capturedErrorToMessage)
}

#' Starts remote asynchronous evaluation of a job
#' 
#' The function sends a start signal to the backend in order to start processing the results
#' for a defined job.
#' 
#' @param job the job object or the job id of the defined job
#' @param log logical - whether to enable automatic logging after starting the job
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return the job_id of the defined job
#' @export 
start_job = function(job, log=FALSE, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
    
        if (!is.null(job) && "Job" %in% class(job)) {
            job_id = job$id
        } else {
            job_id = job
        }
    
        if (is.null(job_id)) {
            stop("No job id specified.")
        }
        
        tag = "execute_async"
        
        success = con$request(tag = tag, parameters = list(job_id), authorized = TRUE)
        message(paste("Job '", job_id, "' has been successfully queued for evaluation.", sep = ""))
        
        if (log) {
            logs(job_id=job_id,con=con)
        }
        
        invisible(success)
    }, error = .capturedErrorToMessage)
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
#' To leave parameter unchanged, then don't mention it. If you want to delete some, then set them to NA.
#' 
#' @param id the job id of a created job
#' @param title update title for the job
#' @param description update description
#' @param process A \code{\link{Graph}}, a function returning a \code{\link{ProcessNode}} as an endpoint, the \code{\link{ProcessNode}} 
#' will return the results or a self defined \code{\link{Process}}
#' @param plan replaces plan with the set value
#' @param budget replaces or sets the credits that can be spent at maximum
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param ... additional parameters passed to toJSON (like 'digits')
#' 
#' @export
update_job = function(id, title = NULL, description = NULL, process = NULL, plan = NULL, budget = NULL, con=NULL, ...) {
    tryCatch({
        con = .assure_connection(con)
        
        if (is.null(id) || missing(id) || is.na(id)) {
            stop("No job was specified.")
        }
        
        patch = list()
        
        if (!is.null(process)) {
            if (!is.environment(process) && !is.function(process) && is.na(process)) stop("Cannot delete a process graph from a job. Either replace it or delete the whole job.")
            
            if (any(c("ProcessNode","function","Graph") %in% class(process))) {
                # final node!
                process = Process$new(id = NA, process_graph=process)
                patch$process = process$serialize()
            }
            
        }
        
        if (!is.null(title)) {
            patch$title = title
        }
        
        if (!is.null(description)) {
            patch$description = description
        }
        
        if (!is.null(plan)) {
            patch$plan = plan
        }
        
        if (!is.null(budget)) {
            patch$budget = budget
        }
        
        tag = "jobs_update"
        res = con$request(tag = tag, parameters = list(id), authorized = TRUE, encodeType = "json", data = patch, ...)
        message(paste("Job '", id, "' was successfully updated.", sep = ""))
        
        job = describe_job(con=con,job = id)
        
        return(job)
    }, error = .capturedErrorToMessage)
}

#' Creates a list of download paths
#' 
#' The function queries the back-end to receive the URLs to the downloadable files of a particular job.
#' 
#' @param job the job object or the id of the job
#' @param con connected and authenticated openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return result object containing of URLs for download
#' @export
list_results = function(job, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        if (!is.null(job) && "Job" %in% class(job)) {
            job_id = job$id
        } else {
            job_id = job
        }
    
        tag = "jobs_download"
        listOfResults = con$request(tag = tag, parameters = list(job_id), authorized = TRUE, type = "application/json")
        class(listOfResults) = "ResultList"
        class(listOfResults$assets) = "AssetList"
        
        return(listOfResults)
    }, error = .capturedErrorToMessage)
}

#' Downloads the results of a job into a specific folder
#' 
#' The function will fetch the results of a asynchronous job and will download all files stated in the links. The parameter
#' 'folder' will be the target location on the local computer.
#' 
#' @param job job object or the job_id for which the results are fetched
#' @param folder a character string that is the target path on the local computer
#' @param con a connected and authenticated OpenEO connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return a list of the target file paths
#' 
#' @importFrom utils download.file
#' @export
download_results = function(job, folder, con=NULL) {
    con = .assure_connection(con)
    
    if (!dir.exists(folder)) 
        dir.create(folder, recursive = TRUE)
    results = list_results(con=con, job=job)
    
    target_files = lapply(names(results$assets), function(file_name) {
        link = results$assets[[file_name]]
        
        href = link$href
        type = link$type
        
        if (!endsWith(x = folder, suffix = "/")) 
            folder = paste0(folder, "/")
        
        file_path = paste0(folder, file_name)
        
        download.file(href, file_path, mode = "wb")
        
        return(file_path)
    })
    
    return(target_files)
}

#' Terminates a running job
#'
#' Informs the server that the specified job needs to be terminated and taken 'canceled' to prevent from
#' further executions and related costs.
#'
#' @param job the job object or the id of job that will be canceled
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a success / failure notification
#' @export
stop_job = function(job, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        if (!is.null(job) && "Job" %in% class(job)) {
            job_id = job$id
        } else {
            job_id = job
        }
    
        if (is.null(job_id)) {
            stop("No job id specified.")
        }
        
        tag = "jobs_cancel"
        
        success = con$request(tag = tag, parameters = list(job_id), authorized = TRUE)
        if (success) {
            message(paste("Job '", job_id, "' has been successfully canceled.", sep = ""))
        }
        
        return(success)
    }, error = .capturedErrorToMessage)
}

#' Fetches information about a job
#'
#' Returns a detailed description about a specified job. For example to check the status of a job.
#'
#' @param job the job object or the id of the job
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a detailed description about the job
#' @export
describe_job = function(job,con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        if (!is.null(job) && "Job" %in% class(job)) {
            job_id = job$id
        } else {
            job_id = job
        }
    
        tag = "jobs_details"
        
        info = con$request(tag = tag, parameters = list(job_id), authorized = TRUE, type = "application/json", auto_unbox = TRUE)
        
        class(info) = "Job"
        class(info$process) = "ProcessInfo"
        class(info$process$process_graph) = "Json_Graph"
        
        return(info)
    }, error = .capturedErrorToMessage)
}


#' Delete a job
#'
#' Deletes a job from the backend.
#'
#' @param job the job or the id of the job
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return logical with state of success
#' @export
delete_job = function(job, con=NULL) {
    tryCatch({
        if (!is.null(job) && "Job" %in% class(job)) {
            job_id = job$id
        } else {
            job_id = job
        }
        con = .assure_connection(con)
    
        tag = "jobs_delete"
        
        success = con$request(tag = tag, parameters = list(job_id), authorized = TRUE)
        if (success) {
            message(paste("Job '", job_id, "' was successfully deleted from the back-end", sep = ""))
        }
        return(success)
    }, error = .capturedErrorToMessage)
}


#' Estimates job costs
#'
#' Calls the back-end and asks for an approximation about the costs in money and how much time
#' will be required to finish the job and whether or not the job owners data download is already
#' included in the monetary costs.
#'
#' @param job the job or the id of the job
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return JobCostsEstimation containing information how much money and time will be spent
#' @export
estimate_job = function(job, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        if (!is.null(job) && "Job" %in% class(job)) {
            job_id = job$id
        } else {
            job_id = job
        }
    
    
        if (is.null(job_id)) {
            stop("No job id specified.")
        }
        tag = "jobs_cost_estimation"
        
        success = con$request(tag = tag, parameters = list(job_id), authorized = TRUE)
        class(success) = "JobCostsEstimation"
        return(success)
    }, error = .capturedErrorToMessage)
}

#' Job log
#' 
#' Attempts to open the log of job.
#' 
#' @param job the job or the job_id
#' @param offset the id of the log entry to start from
#' @param limit the limit of lines to be shown
#' @param con an optional connection if you want to address a specific service
#' 
#' @return a \code{Log} object
#' @export
log_job = function(job, offset=NULL,limit=NULL, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        if (!is.null(job) && "Job" %in% class(job)) {
            job_id = job$id
        } else {
            job_id = job
        }
        
        query_params = list()
        if (length(offset) > 0) {
            query_params$offset = offset
        }
        
        if (length(limit) > 0) {
            query_params$limit = limit
        }
    
    
        if (is.null(job_id)) {
            stop("No job id specified.")
        }
        tag = "job_log"
        
        success = con$request(tag = tag, parameters = list(job_id), authorized = TRUE, query=query_params)
        class(success) = "Log"
        return(success)
    }, error = .capturedErrorToMessage)
}

#' @rdname status
#' @export
status.Job = function(x, ...) {
    tryCatch({
        # refresh description 
        x = describe_job(job = x)
        return(x$status)
    }, error = function(e) {
        print(e$message)
    })
}
