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
    
    listOfJobs = con$request(tag=tag,
                             parameters=list(con$user_id),
                             authorized=TRUE,
                             type="application/json")
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
    
    tag = "execute_sync"
    res = con$request(tag=tag,
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


#' Creates a job on the back-end from a Graph object
#' 
#' This function shall be called after the user defined a process graph for the back-end to create a job on the
#' back-end. Therefore the user sends the process graph and the optional output specifications like
#' format and additional creation parameter by '...'. To add some meta data about the job, the user might use title or 
#' description. By providing a execution plan and a maximum usable budget the user can change the execution behavior of the
#' back-end provider.
#' 
#' @param con connected and authenticated openeo client
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
create_job = function(con,graph=NULL, 
                      title = NULL, description = NULL,
                      plan = NULL, budget = NULL,
                      format=NULL, ...) {
  tryCatch({
    
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
    } else {
      stop("No process graph was defined. Please provide either a process graph id or a process graph description.")
    }
    
    if (!is.null(title)) job$title = title
    if (!is.null(description)) job$description = description
    if (!is.null(plan)) job$plan = plan
    if (!is.null(budget)) job$budget = budget
    
    #endpoint,authorized=FALSE,data,encodeType = "json",query = list(),...
    tag = "jobs_define"
    response = con$request(tag=tag,
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
    
    success = con$request(tag=tag,parameters=list(job_id), authorized = TRUE)
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
    
    tag = "jobs_update"
    res = con$request(tag=tag,
                      parameters=list(id),
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
    listOfResults = con$request(tag=tag,parameters=list(job_id),authorized=TRUE,type="application/json")
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
#' @importFrom utils download.file
#' @export
download_results = function(con, job, folder) {
  
  if (!dir.exists(folder)) dir.create(folder,recursive = TRUE)
  results = list_results(con,job)
  
  lapply(results$links, function(link){
    href = link$href
    type = link$type
    
    if (!endsWith(x = folder,suffix = "/")) folder = paste0(folder,"/")
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
    
    success = con$request(tag=tag,parameters=list(job_id), authorized = TRUE)
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
    
    info = con$request(tag=tag,parameters=list(job_id),authorized = TRUE, type="application/json",auto_unbox=TRUE)
    
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
    
    success = con$request(tag=tag,parameters=list(job_id), authorized = TRUE)
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
    
    success = con$request(tag=tag,parameters=list(job_id), authorized = TRUE)
    class(success) = "JobCostsEstimation"
    return(success)
  },error=.capturedErrorToMessage)
}