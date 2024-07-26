#' @include debugging.R
NULL

# jobs endpoint ----

#' List the jobs of a user
#'
#' Lists the jobs that a user has uploaded or in that are in execution
#'
#' @param con the authenticated Connection (optional) otherwise [active_connection()]
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

setClass("Job")

#' Executes a job and returns the data immediately
#'
#' Executes a job directly on the connected openEO service and returns the data. During the execution phase the connection to 
#' the server remains open. This function allows to debug the code and check the results immediately. 
#' Please keep in mind, that computational functions might be related to monetary costs, if no 'free' plan is available. 
#' Make sure to keep the data selection relatively small, also some openEO service provider might offer limited processes support,
#' e.g. not supporting UDFs at this endpoint. When a file format is set, then the process graph will be parsed and the arguments for 
#' 'save_result' will be replaced. If the 'stars' package is installed and parameter `as_stars` is set to TRUE, then the downloaded
#' data is opened and interpreted into a stars object.
#' 
#' @note 
#' If parameter 'format' is ignored, it is assumed that 'save_result' was already used in the process graph. Otherwise it is up to the
#' back-end provider how data is stored if 'save_result' was omitted.
#'
#' @param graph a [Graph()], a function returning a [ProcessNode()] as an endpoint or the [ProcessNode()] 
#' will return the results
#' @param output_file storage location for the returned data
#' @param budget numeric, maximum spendable amount for testing
#' @param plan character, selection of a service plan
#' @param as_stars logical to indicate if the data shall be interpreted as a stars object
#' @param format character or `FileFormat` specifying the File format for the output, if 'save_result' is not
#' set in the process then it will be added otherwise the value stated here will replace the original value.
#' @param additional Additional, non-standardized job settings to send to the back-end
#' @param con connected and authenticated openEO client (optional) otherwise [active_connection()]
#' is used.
#' @param ... additional parameters passed to jsonlite::toJSON() (like 'digits') or additional arguments that shall 
#' be passed to the openEO process 'save_result'
#' 
#' @return a local path to the downloaded file or a `stars` object if `as_stars=TRUE`
#' 
#' @importFrom methods as
#' @export
compute_result = function(graph, output_file = NULL, budget=NULL, plan=NULL, as_stars=FALSE, format = NULL, additional = NULL, con=NULL, ...) {
    tryCatch({
        con = .assure_connection(con)
        output = list()
        
        if (is.null(graph)) 
            stop("No process graph was defined. Please provide a process graph.")
        if (!"Process" %in% class(graph)) {
          if ("Graph" %in% class(graph))  {
              # OK
          } else if (is.list(graph)) {
              graph = parse_graph(json=graph)
          } else if ("function" %in% class(graph)){
              graph = as(graph, "Graph")
          } else {
              stop("Parameter graph is not a Graph object. Expecting a Graph, a list object or a function.")
          }
          
          process = Process$new(id=NA,description = NA,
                                summary = NA,process_graph=graph)
        } else {
          if ("ProcessNode" %in% class(graph)) process = as(graph,"Process")
          else process = graph
        }
        
        save_node = .find_process_by_name(process,"save_result")
        
        if (length(save_node) == 0 && length(format) == 0) {
          warning("No 'save_result' used in the process graph and no 'format' specified. Relying on the default setting of the back-end provider, which might result in an error or an unexpected file format.")
        }
        
        # if format is set check if save_result is set, if not do that with the format stated, if it is 
        # check if the formats match else replace
        # more or less replace save_result of the graph with the customization stated in this function
        if (length(format) > 0) {
          
          
          p = processes()
          
          dots = list(...)
          
          call_args = list(format = format)
          
          arg_names = names(formals(p$save_result))
          save_result_dots = dots[which(arg_names %in% names(dots))]
          
          call_args = append(call_args, save_result_dots)
          
          if (length(save_node) == 0) {
            # not existent
            call_args$data = process$getProcessGraph()$getFinalNode()
            
            # check for potentially multiple end nodes
            
          } else {
            # check only first (or look for the final node)
            saved_graph = save_node[[1]]
            call_args$data = saved_graph$parameters$data
            
            if ("Argument" %in% class(call_args$data)) {
              call_args$data = call_args$data$getValue()
            }
          }
          
          saved_graph = do.call(p$save_result,call_args)
          process = as(saved_graph,"Process")
        }
        job = list(
            process = process$serialize()
        )
        
        if (length(budget) > 0) {
            job$budget = budget
        }
        
        if (length(plan) > 0) {
            job$plan = plan
        }
        
        if (!is.null(additional)) {
            job = c(job, additional)
        }
        
        is_tempfile = ifelse(length(output_file) == 0,TRUE,FALSE)
        
        tag = "execute_sync"
        res = con$request(tag = tag, authorized = TRUE, data = job, encodeType = "json", parsed=FALSE, ...)
        
        if (length(format) == 0 && length(save_node) > 0) {
          format=save_node[[1]]$parameters$format
        }
        
        # find a suitable file suffix if it is just a tempfile
        if (length(format) > 0 && length(output_file) == 0) {
          if (is.character(format)) {
            driver = format
          } else if ("FileFormat" %in% class(format)) {
            driver = format$name
          } else {
            message("Cannot derive a file extension. Using none which might affect data interpretation.")
            driver = ""
          }
          
          suffix = switch(tolower(driver), netcdf=".nc",gtiff=".tif",json=".json",default="")
          output_file = tempfile(fileext = suffix)
        }
        
        tryCatch({
          if (!dir.exists(dirname(output_file))) {
            dir.create(dirname(output_file), recursive = TRUE)
          }
          writeBin(resp_body_raw(res), output_file)
        }, error = function(err) {
          stop(err)
        })
        
        if (isTRUE(as_stars) && .is_package_installed("stars")) {
          
          obj=stars::read_stars(output_file,proxy=FALSE,quiet=TRUE)
          
          tryCatch({
            return(obj)
          }, finally={
            if (is_tempfile) unlink(output_file)
          })
        } else {
          return(output_file)
        }
        
    }, error = .capturedErrorToMessage)
}


#' Creates a new job on the back-end 
#' 
#' In preparation to execute the users analysis workflow (user defined process) asynchronously, they need to register a job that
#' will be scheduled when the required resources are available. To do so the user provides the process graph with optional descriptive meta
#' data and the desired execution plan or the maximum amount of credits spent.
#' 
#' @param graph A [Graph()], a function returning a [ProcessNode()] as an endpoint or the [ProcessNode()] 
#' will return the results
#' @param title Optional title of a job
#' @param description Optional detailed information about a job
#' @param plan An optional execution plan offered by the back-end, determining how the job will be executed
#' @param budget An optional budget, which sets the maximum amount of credits to be used by the job
#' @param additional Additional, non-standardized job settings to send to the back-end
#' @param con connected and authenticated openEO client (optional) otherwise [active_connection()]
#' is used.
#' @param ... additional parameters passed to jsonlite::toJSON() (like 'digits')
#' 
#' @return the id of the job
#' @export
create_job = function(graph = NULL, title = NULL, description = NULL, plan = NULL, budget = NULL, additional = NULL, con=NULL, ...) {
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
        if (!is.null(additional))
            job = c(job, additional)
        
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
        
        # response = con$request(tag = tag, authorized = TRUE, data = job, raw = TRUE, ...)
        response = con$request(tag = tag, authorized = TRUE, data = job, parsed=FALSE, ...)
        
        message("Job was sucessfully registered on the back-end.")
        
        job_id = resp_headers(response)$`openeo-identifier`
        
        job = describe_job(job = job_id, con=con)
        
        return(job)
    }, error = .capturedErrorToMessage)
}

#' Starts remote asynchronous evaluation of a job
#' 
#' The function sends a start signal to the back-end triggering a defined job.
#' 
#' @param job the job object or the job id
#' @param log logical - whether to enable automatic logging after starting the job
#' @param con connected and authenticated openEO client (optional) otherwise [active_connection()]
#' is used.
#' 
#' @return the job object of the now started job
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
        
        invisible(describe_job(job,con = con))
    }, error = .capturedErrorToMessage)
}

#' Modifies a job with given parameter
#' 
#' The function modifies a stores a job with a given parameter. The dot parameter contains all the values
#' that will be replaced or removed. The return shows a message of result or failure.
#' 
#' @details The '...' operator shall contain all the values that are to be replaced in the job. There are some reserved
#' keys. 
#' The 'process_graph' option will replace the process graph with a newly defined one, therefore the process graph needs to be a Graph object.
#' The 'format' option will change the desired output format.
#' All other parameter will be assumed to be special output parameter. Remember, you don't need to specify a process graph or graph_id,
#' e.g. if you just want to update the output format. 
#' To leave parameter unchanged, then don't mention it. If you want to delete some, then set them to NA.
#' 
#' @param id the job id of a created job
#' @param title update title for the job
#' @param description update description
#' @param process A [Graph()], a function returning a [ProcessNode()] as an endpoint, the [ProcessNode()] 
#' will return the results or a self defined [Process()]
#' @param plan replaces plan with the set value
#' @param budget replaces or sets the credits that can be spent at maximum
#' @param additional Additional, non-standardized job settings to send to the back-end
#' @param con connected and authenticated openEO client (optional) otherwise [active_connection()]
#' is used.
#' @param ... additional parameters passed to jsonlite::toJSON() (like 'digits')
#' 
#' @export
update_job = function(id, title = NULL, description = NULL, process = NULL, plan = NULL, budget = NULL, additional = NULL, con=NULL, ...) {
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
        
        if (!is.null(additional)) {
            patch = c(patch, additional)
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
#' @param con connected and authenticated openEO client object (optional) otherwise [active_connection()]
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

#' Downloads the results of a job
#' 
#' The function will fetch the results of a asynchronous job and will download all files stated in the links. The parameter
#' 'folder' is the target location on the local computer.
#' 
#' @param job job object or the job_id for which the results are fetched. Also the return value of 
#' [list_results()] or its 'assets' field is also accepted.
#' @param folder a character string that is the target path on the local computer
#' @param con a connected and authenticated openEO connection (optional) otherwise [active_connection()]
#' is used.
#' 
#' @return a list of the target file paths or NULL if 'job' was incorrect
#' 
#' @importFrom utils download.file
#' @importFrom rlang is_na
#' @export
download_results = function(job, folder, con=NULL) {
    con = .assure_connection(con)
    
    if (missing(job) || is.null(job) || rlang::is_na(job)) {
      message("Parameter 'job' is not set.")
      return(invisible(NULL))
    }
    
    if (missing(folder) || is.null(folder) || rlang::is_na(folder) || length(folder) != 1) {
      folder = tempdir()
    }
    
    if (!dir.exists(folder)) 
        dir.create(folder, recursive = TRUE)
    
    if ("Job" %in% class(job) || is.character(job)) {
      results = list_results(con=con, job=job)
      assets = results$assets
    } else if ("AssetList" %in% class(job)) {
      assets = job
    } else if ("ResultList" %in% class(job)) {
      assets = job$assets
    } else {
      message("Parameter 'job' was not interpretable.")
      return(invisible(NULL))
    }
    
    target_files = lapply(names(assets), function(file_name) {
        link = assets[[file_name]]
        
        href = link$href
        type = link$type
        
        if (!endsWith(x = folder, suffix = "/")) 
            folder = paste0(folder, "/")
        
        file_path = paste0(folder, file_name)
        
        req = request(href)
        req = req_method(req, method="GET")
        response = req_perform(req)
        
        writeBin(resp_body_raw(response), file_path)
        
        return(file_path)
    })
    
    return(target_files)
}

#' Terminates a running job
#'
#' Informs the server that the specified job needs to be terminated to prevent further costs.
#'
#' @param job the job object or the id of job that will be canceled
#' @param con authenticated Connection (optional) otherwise [active_connection()]
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
#' Returns a detailed description about a specified job (e.g., the status)
#'
#' @param job the job object or the id of the job
#' @param con authenticated Connection (optional) otherwise [active_connection()]
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
        
        info$currency = con$getCapabilities()$billing$currency
        
        class(info) = "Job"
        class(info$process) = "ProcessInfo"
        class(info$process$process_graph) = "Json_Graph"
        
        return(info)
    }, error = .capturedErrorToMessage)
}

# to make a class definition for the returned lists
setOldClass("Job")

#' Delete a job
#'
#' Deletes a job from the back-end.
#'
#' @param job the job or the id of the job
#' @param con authenticated Connection (optional) otherwise [active_connection()]
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
#' Calls the back-end and asks for an approximation about the monetary costs, the required time, 
#' and whether or not the job owners data download is already included in the monetary costs.
#'
#' @param job the job or the id of the job
#' @param con authenticated Connection (optional) otherwise [active_connection()]
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
        success$currency = con$getCapabilities()$billing$currency
        class(success) = "JobCostsEstimation"
        return(success)
    }, error = .capturedErrorToMessage)
}

#' Job log
#' 
#' Opens the log of job.
#' 
#' @param job the job or the job_id
#' @param offset the id of the log entry to start from
#' @param limit the limit of lines to be shown
#' @param con an optional connection if you want to address a specific service
#' 
#' @return a `Log` object
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

serialize_session = function(file=NULL, ...) {
  
  if (length(file) == 0) {
    file = tempfile(fileext = ".RData")
  }
  
  serialize_connection = active_connection()
  serialize_process_list = active_process_list()
  serialize_process_collection = active_process_collection()
  serialize_data_collection = active_data_collection()
  
  save(list=names(pkgEnvironment),envir=pkgEnvironment,file = file)
  
  return(file)
}

load_serialized_session = function(file) {
  load(file = file, envir = pkgEnvironment)
  
  if (!rlang::is_null(active_connection())) {
    .fill_rstudio_connection_observer()
  }
}

