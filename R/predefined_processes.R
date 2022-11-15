#' @include user_defined_processes.R
NULL

# processes endpoint ----

#' List available processes on server
#'
#' List all processes available on the back-end. This returns the R translation of the JSON metadata as lists. This process description
#' is stored internally at the environment package variable `process_list`, which is not directly accessible apart from this function.
#' 
#' @param con Connection object (optional) otherwise [active_connection()]
#' is used.
#' @return a list of lists with process_id and description
#' @export
list_processes = function(con=NULL) {
  
  process_list = get(x = "process_list", envir = pkgEnvironment)

  # if the list was not called already, then fetch the processes from the back-end, otherwise return the stored data
  if (is.null(process_list)) {
    tryCatch({
      con = .assure_connection(con)
      
      tag = "process_overview"
      
      listOfProcesses = con$request(tag = tag, authorized=con$isLoggedIn(), type = "application/json")
      process_list = lapply(listOfProcesses$processes, function(process) {
        class(process) = "ProcessInfo"
        return(process)
      })
      
      names(process_list) = sapply(process_list, function(p) p$id)
      class(process_list) = "ProcessList"
      
      assign(x = "process_list", value = process_list, envir = pkgEnvironment)
      
    }, error = .capturedErrorToMessage)
    
  }
  
  return(process_list)
}

#' Describe a process
#'
#' Queries an openEO back-end and retrieves more detailed information about offered processes
#' @param con Authentication object (optional) otherwise [active_connection()]
#' is used.
#' @param process id of a process to be described or the ProcessInfo object
#'
#' @return a list of detailed information
#' @export
#' @importFrom rlang is_na
describe_process = function(process = NA, con=NULL) {
    tryCatch({
        process_list = list_processes(con=con)
        
        describeProcess = !missing(process) && !rlang::is_na(process)
        
        if (!describeProcess) {
            message("No or invalid process_id(s) or process")
            invisible(NULL)
        }
        
        if ("ProcessInfo" %in% class(process)) {
            return(process)
        }
        
        if (is.null(process_list)) {
            message("No processes found or loaded from the back-end")
            invisible(NULL)
        }
        
        if (!process %in% names(process_list)) {
            message(paste("Cannot describe process '", process, "'. Process does not exist.", sep = ""))
            invisible(NULL)
        } else {
            return(process_list[[process]])
        }
    }, error = .capturedErrorToMessage)
}

# ProcessCollection ====
#' Process Collection
#' 
#' This object contains template functions for process graph building from the processes offered by an openEO service. This object is
#' an unlocked R6 object, in order to add new functions at runtime. 
#' 
#' @name ProcessCollection
#' 
#' @section Methods:
#' \describe{
#'    \item{`$new(con = NULL)`}{The object creator created an openEO connection.} 
#' } 
#' @section Arguments:
#' \describe{
#'    \item{con}{optional an active and authenticated Connection (optional) otherwise [active_connection()]
#' is used.}
#' }
#' 
#' @seealso [`processes()`]
NULL 

ProcessCollection = R6Class(
    "ProcessCollection",
    lock_objects = FALSE,
    public = list(
        # public ====
        initialize = function(con=NULL) {
            tryCatch({
                process_list = list_processes(con=con)
                private$processes = lapply(process_list, function(process_description) {
                    process_description$process_graph = NULL #remove the optional process_graph part as it is confusing here
                    return(processFromJson(process_description))
                })
                
                if (!is.list(private$processes)) stop("Processes are not provided as list")
                
                for (index in 1:length(private$processes)) {
                    if (is.null(private$processes[[index]])) {
                        next
                    }
                    
                    pid = private$processes[[index]]$getId()
                    function_formals = private$processes[[index]]$getFormals()
                    
                    f = function() {}
                    formals(f) = function_formals
                    
                    
                    # probably do a deep copy of the object
                    # for the body we have the problem that index is addressed as variable in the parent environment. This
                    # causes a problem at call time, where index is resolve and this means that usually the last element
                    # of the list will be used as process all the time -> solution: serialize index, gsub on quote, make "{" as.name
                    # and then as.call
                    body(f) = quote({
                        exec_process = private$processes[[index]]$clone(deep=TRUE)
                        # find new node id:
                        node_id = .randomNodeId(exec_process$getId(),sep="_")
                        
                        while (node_id %in% private$getNodeIds()) {
                            node_id = .randomNodeId(exec_process$getId(),sep="_")
                        }
                        
                        private$node_ids = c(private$node_ids,node_id)
                        
                        #map given parameter of this function to the process parameter / arguments and set value
                        arguments = exec_process$parameters
                        
                        # parameter objects should be updated directly, since there is a real object reference
                        this_param_names = names(formals())
                        
                        # used match.call before, but it seem that it doesn't resolve the pipe - it is something like data = .
                        this_arguments = lapply(this_param_names, function(param) get(param))
                        names(this_arguments) = this_param_names
                        
                        # special case: value is of type Argument
                        node = ProcessNode$new(node_id = node_id,process=exec_process,graph=self)
                        
                        lapply(names(this_arguments), function(param_name, arguments){
                            call_arg = this_arguments[[param_name]]
                            arguments[[param_name]]$setProcess(node)
                            arguments[[param_name]]$setValue(call_arg)
                        }, arguments = arguments)
                        
                        return(node)
                    })
                    # replace index with the actual number!
                    tmp = gsub(body(f),pattern="index",replacement = eval(index))
                    body(f) = as.call(c(as.name(tmp[1]),parse(text=tmp[2:length(tmp)])))
                    
                    # register the ProcessNode creator functions on the Graph class
                    self[[pid]] = f
                }
            }, error = .capturedErrorToMessage)
        }
    ),
    private = list(
      # private ====
        node_ids = character(),
        processes = list(),
        getNodeIds = function() {private$node_ids}
    )
)

#' Get a process graph builder / process collection from the connection
#' 
#' Queries the connected back-end for all available processes and collection names and registers them via R functions on
#' a [`ProcessCollection`] object to build a process graph in R. The current [`ProcessCollection`] is stored internally at the package environment
#' variable `process_collection`, which can be fetched and set with `active_process_collection`.
#' 
#' @param con a connection to an openEO back-end (optional) otherwise [active_connection()]
#' is used.
#' @param processes the [`ProcessCollection`] that is obtained from `processes()` to be set as the active process collection or left empty to fetch the `ProcessCollection` from the package variable.
#' 
#' @return a [`ProcessCollection`] object with the offered processes of the back-end
#' 
#' @rdname processes
#' @export
processes = function(con = NULL) {
  process_collection = active_process_collection()
  
  if (rlang::is_null(process_collection)) {
  
    tryCatch({
        process_collection = ProcessCollection$new(con = con)
        void = active_process_collection(processes = process_collection)
    }, error = .capturedErrorToMessage)
  }
  
  return(process_collection)
}


#' @rdname processes
#' @export
active_process_collection = function(processes=NULL) {
  if (is.null(processes)) {
    return(get(x = "process_collection", envir = pkgEnvironment))
  } else if ("ProcessCollection" %in% class(processes)) {
    assign(x = "process_collection", value = processes, envir = pkgEnvironment)
    invisible(processes)
  } else {
    stop(paste0("Cannot set processes collection with object of class '",utils::head(class(processes),1),"'"))
  }
}

active_process_list = function(process_list=NULL) {
  if (is.null(process_list)) {
    return(get(x = "process_list", envir = pkgEnvironment))
  } else if ("ProcessList" %in% class(process_list)) {
    assign(x = "process_list", value = process_list, envir = pkgEnvironment)
    invisible(process_list)
  } else {
    stop(paste0("Cannot set processes list with object of class '",utils::head(class(process_list),1),"'"))
  }
}

demo_processes = function() {
  process_list = readRDS(system.file("openeo_processes/openeo_processes_1.2.0.rds",package = "openeo"))
  active_process_list(process_list = process_list)
  invisible(processes())
}
