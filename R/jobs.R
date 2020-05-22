# jobs endpoint ----

#' List the jobs that a user has
#'
#' lists the jobs that a user has uploaded or in execution
#'
#' @param con the authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @export
# updated
list_jobs = function(con=NULL) {
    tryCatch({
        con=.assure_connection(con)
        
        tag = "user_jobs"
        
        listOfJobs = con$request(tag = tag, parameters = list(con$user_id), authorized = TRUE, type = "application/json")
        class(listOfJobs$jobs) = "JobList"
        listOfJobs = listOfJobs$jobs
        
        showed_columns = c("id", "title", "status", "created", "updated", "costs", "budget", "plan")
        
        # list to tibble
        if (length(listOfJobs) > 0) {
            df = as.data.frame(listOfJobs)
            
            missing_columns = showed_columns[!showed_columns %in% colnames(df)]
            if (length(missing_columns) > 0) {
                nas = rep(NA, length(missing_columns))
                names(nas) = missing_columns
                df = do.call("cbind", append(list(df), as.list(nas)))
            }
            df = df[, showed_columns]
            
        } else {
            df = data.frame(id = character(),title=character(),status = character(),
                            created = character(), updated = character(), costs=integer(),
                            budget=integer(), plan=character(),stringsAsFactors = FALSE)
        }
        
        if (nrow(df) == 0 || ncol(df) == 0) {
            message("No jobs stored on the back-end.")
            invisible(df)
        }
        
        if (isNamespaceLoaded("tibble")) {
            df = tibble::as_tibble(df)
        }
        
        return(df)
    }, error = .capturedErrorToMessage)
}

#' Executes a job directly and returns the data immediately
#'
#' Executes a job directly on the connected openEO back-end and returns the data. It relates to
#' POST /api/execute in v0.0.2. During the execution phase the connection to the server remains open.
#'
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param graph A process graph
#' @param format The inteded format of the data to be returned
#' @param output_file Where to store the retrieved data under
#' @param ... additional configuration parameter for output generation
#' @return a connection to file if output was provided, the raw data if not
#' @export
compute_result = function(con=NULL, graph, format = NULL, output_file = NULL, ...) {
    tryCatch({
        con = .assure_connection(con)
        
        # former sync evaluation
        if (is.null(format)) {
            stop("Parameter \"format\" is not set. Please provide a valid format.")
        }
        
        output = list(...)
        output = append(output, list(format = format))
        
        if (is.null(graph)) 
            stop("No process graph was defined. Please provide a process graph.")
        
        if ("Graph" %in% class(graph))  {
            graph = graph$serialize()
        } else if (is.list(graph)) {
            graph = list(process_graph = graph, output = output)
        } else if ("ProcessNode" %in% class(graph)){
            # final node!
            graph = Graph$new(final_node = graph)$serialize()
        } else {
            stop("Parameter graph is not a Graph object. Awaiting a list.")
        }
        
        job = list(
            process_graph = graph
        )
        
        tag = "execute_sync"
        res = con$request(tag = tag, authorized = TRUE, data = job, encodeType = "json", raw = TRUE)
        
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
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param graph A Graph object
#' @param title Optional title of a job to be found
#' @param description Optional a more detailed information about a job
#' @param plan An optional execution plan offered by the back-end, determining how the job will be executed
#' @param budget An optional budget, which sets the maximum amount of credits to be used by the job
#' @param format The inteded format of the data to be returned
#' @param ... additional configuration parameter for output generation
#' 
#' @return the id of the job
#' @export
create_job = function(con=NULL, graph = NULL, title = NULL, description = NULL, plan = NULL, budget = NULL, ...) {
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
        
        response = con$request(tag = tag, authorized = TRUE, data = job, raw = TRUE)
        
        message("Job was sucessfully registered on the backend.")
        
        job_id = headers(response)$`openeo-identifier`
        return(job_id)
    }, error = .capturedErrorToMessage)
}

#' Starts remote asynchronous evaluation of a job
#' 
#' The function sends a start signal to the backend in order to start processing the results
#' for a defined job.
#' 
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job the job object or the job id of the defined job
#' 
#' @return the job_id of the defined job
#' @export 
start_job = function(con=NULL, job) {
    con = .assure_connection(con)
    
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
        
        success = con$request(tag = tag, parameters = list(job_id), authorized = TRUE)
        message(paste("Job '", job_id, "' has been successfully queued for evaluation.", sep = ""))
        
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
#' To leave parameter unchanged, then don't mention it in the ... parameter. If you want to delete some, then set them to NULL.
#' 
#' @param con connected and authenticated openeo client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param id the job id of a created job
#' @param title update title for the job
#' @param description update description
#' @param process_graph a Graph object created with the process_graph_builder
#' @param plan replaces plan with the set value
#' @param budget replaces or sets the credits that can be spent at maximum
#' @param format the output format
#' @param ... The create options parameter you want to change. See Details for more information
#' @export
update_job = function(con=NULL, id, title = NULL, description = NULL, process_graph = NULL, plan = NULL, budget = NULL, format = NULL, ...) {
    tryCatch({
        con = .assure_connection(con)
        
        if (is.null(id)) {
            stop("No job i was specified.")
        }
        
        patch = list()
        create_options = list(...)
        output = list()
        if (length(create_options) > 0) {
            output$parameters = create_options
        }
        if (!is.null(format)) 
            output$format = format
        
        if (length(output) > 0) 
            patch$output = output
        
        if (!is.null(process_graph)) {
            if ("ProcessNode" %in% class(process_graph)){
                # final node!
                process_graph = Graph$new(final_node = process_graph)$serialize()
            }
            patch$process_graph = process_graph
        }
        
        if (!is.null(title)) {
            if (is.na(title)) 
                patch$title = NULL else patch$title = title
        }
        
        if (!is.null(description)) {
            if (is.na(description)) 
                patch$description = NULL else patch$description = description
        }
        
        if (!is.null(plan)) {
            if (is.na(plan)) 
                patch$plan = NULL else patch$plan = plan
        }
        
        if (!is.null(budget)) {
            if (is.na(budget)) 
                patch$budget = NULL else patch$budget = budget
        }
        
        tag = "jobs_update"
        res = con$request(tag = tag, parameters = list(id), authorized = TRUE, encodeType = "json", data = patch)
        message(paste("Job '", id, "' was successfully updated.", sep = ""))
        invisible(TRUE)
    }, error = .capturedErrorToMessage)
}

#' Follow an executed Job
#'
#' Opens up a websocket to the openEO back-end to fetch updates about a running job.
#'
#' @param con An authenticated connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job_id the id of the job on the server the user wants to connect to
#' @return a WebSocket connection
#' @export
follow_job = function(con=NULL, job_id) {
    .not_implemented_yet()
    
    con = .assure_connection(con)
}

#' Creates a list of download paths
#' 
#' The function queries the back-end to receive the URLs to the downloadable files of a particular job.
#' 
#' @param con connected and authenticated openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job the job object or the id of the job
#' 
#' @return result object containing of URLs for download
#' @export
list_results = function(con=NULL, job) {
    con = .assure_connection(con)
    
    if (!is.null(job) && "JobInfo" %in% class(job)) {
        job_id = job$id
    } else {
        job_id = job
    }
    
    tryCatch({
        tag = "jobs_download"
        listOfResults = con$request(tag = tag, parameters = list(job_id), authorized = TRUE, type = "application/json")
        return(listOfResults)
    }, error = .capturedErrorToMessage)
}

#' Downloads the results of a job into a specific folder
#' 
#' The function will fetch the results of a asynchronous job and will download all files stated in the links. The parameter
#' 'folder' will be the target location on the local computer.
#' 
#' @param con a connected and authenticated OpenEO connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job job object or the job_id for which the results are fetched
#' @param folder a character string that is the target path on the local computer
#' 
#' @return a list of the target file paths
#' 
#' @importFrom utils download.file
#' @export
download_results = function(con=NULL, job, folder) {
    con = .assure_connection(con)
    
    if (!dir.exists(folder)) 
        dir.create(folder, recursive = TRUE)
    results = list_results(con, job)
    
    target_files = lapply(results$links, function(link) {
        href = link$href
        type = link$type
        
        if (!endsWith(x = folder, suffix = "/")) 
            folder = paste0(folder, "/")
        filename = basename(href)
        
        file_path = paste0(folder, filename)
        
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
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job the job object or the id of job that will be canceled
#' @return a success / failure notification
#' @export
stop_job = function(con=NULL, job) {
    con = .assure_connection(con)
    
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
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job the job object or the id of the job
#' @return a detailed description about the job
#' @export
describe_job = function(con=NULL, job) {
    con = .assure_connection(con)
    
    if (!is.null(job) && "JobInfo" %in% class(job)) {
        job_id = job$id
    } else {
        job_id = job
    }
    
    tryCatch({
        tag = "jobs_details"
        
        info = con$request(tag = tag, parameters = list(job_id), authorized = TRUE, type = "application/json", auto_unbox = TRUE)
        
        class(info) = "JobInfo"
        class(info$process) = "ProcessInfo"
        class(info$process$process_graph) = "Json_Graph"
        
        return(info)
    }, error = .capturedErrorToMessage)
}


#' Delete a job
#'
#' Deletes a job from the backend.
#'
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job the job or the id of the job
#' @return logical with state of success
#' @export
delete_job = function(con=NULL, job) {
    if (!is.null(job) && "JobInfo" %in% class(job)) {
        job_id = job$id
    } else {
        job_id = job
    }
    con = .assure_connection(con)
    
    tryCatch({
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
#' @param con authenticated Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param job the job or the id of the job
#' @return JobCostsEstimation containing information how much money and time will be spent
#' @export
estimate_job = function(con=NULL, job) {
    con = .assure_connection(con)
    
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
        
        success = con$request(tag = tag, parameters = list(job_id), authorized = TRUE)
        class(success) = "JobCostsEstimation"
        return(success)
    }, error = .capturedErrorToMessage)
}
