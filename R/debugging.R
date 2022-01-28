
#' Triggers debugging mode
#' 
#' The debugging mode is created to investigate the communication between server and client. The mode can be
#' turned on or off, depending on the selected function (debug, debug.off). It is stored as an package internal environment 
#' and other package functions can access it naturally. By using the environment object, entries can be changed.
#' 
#' @rdname debugging
#' @export
debug = function() {
    assign(x = "DEBUG_MODE", value = TRUE, envir = pkgEnvironment)
}

#' @rdname debugging
#' @export
debug.off = function() {
    assign(x = "DEBUG_MODE", value = FALSE, envir = pkgEnvironment)
}

#' @rdname debugging
#' @export
is.debugging = function() {
    return(get(x = "DEBUG_MODE", envir = pkgEnvironment))
}

print.response = function(x) {
    print(x$request)
    
    cat("\n*** Response ***", paste("Status:", x$status_code), "Headers:", sep = "\n")
    
    headers = as.data.frame(cbind(resp_headers(x)))
    names(headers) = NULL
    
    print(headers)
    
    if (!is.null(headers["content-length", ]) && unlist(headers["content-length", ]) != "0") {
        cat("\nBody:", sep = "\n")
        
        if ("content-disposition" %in% rownames(headers)) {
            cat(paste("File attachment:", unlist(strsplit(x = unlist(headers["content-disposition", ]), split = "filename=")))[2], sep = "\n")
        }
        
        if ("content-type" %in% rownames(headers) && unlist(headers["content-type", ]) == "application/json") {
            print(toJSON(resp_body_json(x), auto_unbox = TRUE, pretty = TRUE))
        } else {
            print("Other formats not yet implemented.")
        }
        
    }
}

print.request = function(x) {
    call = paste(x$method, x$url)
    headers = x$headers
    if (!is.null(x$options$httpauth) && x$options$httpauth == 1 && !is.null(x$options$userpwd)) {
        headers = c(headers, Authorization = paste("Basic", base64_enc(x$options$userpwd)))
    }
    headers = as.data.frame(headers)
    names(headers) = NULL
    
    cat("*** Request ***", call, "Headers:", sep = "\n")
    print(headers)
    
    if (!is.null(x$options$postfields)) {
        cat("Body:", sep = "\n")
        print(prettify(rawToChar(x$options$postfields)))
    }
}

#' Access logs of a Service or Job
#' 
#' Prints contents of the log file of a Job or Service to the console. Requests the log every second if the service is enabled or the batch job is
#' active. If the log response always empty for a given timeout, the logging stops. Also if the job or service is not active at the moment timeout
#' is ignored and the log is just printed once. To call the different logs \code{\link{log_job}} or \code{\link{log_service}} are used internally.
#' 
#' In Jupyter, RMarkdown and knitr HTML environments the timeout parameter does not apply and this function only returns the
#' logs that are available at the time of the request. To refresh the logs, you have to re-execute the function again.
#' 
#' @param obj Service or Job object
#' @param job_id character the jobs ID
#' @param service_id character - the services ID
#' @param con a connected openEO client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param timeout integer the timeout for the logging of active jobs or services after no update in seconds, if omitted it is determined internally (running / queued / enabled -> 60s)
#' 
#' @seealso \code{\link{log_job}} or \code{\link{log_service}}
#' 
#' @export
logs = function(obj=NULL,job_id=NULL,service_id=NULL, con=NULL, timeout = NULL) {
    
    tryCatch({
        con = .assure_connection(con)
        
        if (length(timeout) > 0 && !is.numeric(timeout)) {
          stop("parameter 'timeout' is set, but is not numerical")
        }
        
        if (length(obj) == 0 && length(job_id) == 0 && length(service_id) == 0) {
            message("No service or job stated to be logged.")
            return(invisible(NULL))
        }
        
        if ((length(obj) > 0 && "Job" %in% class(obj)) || length(job_id) > 0) {
            log_fun = log_job
            
            if (length(obj) == 0 && length(job_id) > 0) {
                obj = describe_job(job_id)
            }
        } else if ((length(obj) > 0 && "Service" %in% class(obj)) || length(service_id) > 0) {
            log_fun = log_service
            
            if (length(obj) == 0 && length(service_id) > 0) {
                obj = describe_service(service_id)
            }
        }
        
        status = status(obj)
        is_active = status %in% c("running","queued","enabled") # enabled for services then we might also observe
        if (length(timeout) == 0) {
          if (is_active) {
            timeout = 60
          }
        }
        
        log = log_fun(obj, con=con)
        if (is_html_context()) {
            return(print_html("logs", log$logs))
        }
        
        # maybe the log has not initialized yet, then wait a second
        while (length(log$logs) == 0) {
            Sys.sleep(1)
            log = log_fun(obj, con=con)
        }
        
        if (is_active) {
          last_message_id = log$logs[[length(log$logs)]]$id
          print(log)
          
          start = Sys.time()
          while(difftime(Sys.time(),start,units="secs") <= timeout) {
              log = log_fun(obj,offset = last_message_id, con=con)
              if (length(log$logs) > 0) {
                  start = Sys.time()
                  last_message_id = log$logs[[length(log$logs)]]$id
                  
                  print(log)
              }
              Sys.sleep(1)
          }
          message("Log ended or had a timeout.")
        }
    }, error = function(e){
        message(e$message)
    }, finally={
        return(invisible())
    })
    
    
}
