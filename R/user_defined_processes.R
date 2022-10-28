#' @include process_graph_building.R
NULL

# process graph endpoint ----

#' Lists the IDs of the process graphs from the current user.
#' 
#' Queries the back-end to retrieve a list of graph ids that the current user has stored on the back-end.
#' 
#' @param con connected and authenticated openEO client object (optional) otherwise [active_connection()]
#' is used.
#' @return a named list of user defined processes (`ProcessInfo`)
#' @export
list_user_processes = function(con=NULL) {
  tryCatch({
    tag = "graph_overview"
    
    con = .assure_connection(con)
    
    listOfUserDefinedProcesses = con$request(tag = tag, authorized = TRUE)$processes
    
    listOfUserDefinedProcesses = lapply(listOfUserDefinedProcesses, function(process) {
      class(process) = "ProcessInfo"
      return(process)
    })
    
    names(listOfUserDefinedProcesses) = sapply(listOfUserDefinedProcesses, function(p) p$id)
    class(listOfUserDefinedProcesses) = "ProcessList"
    
    return(listOfUserDefinedProcesses)
    
  }, error = .capturedErrorToMessage)
}

#' Fetches the representation of a stored user defined process
#' 
#' The function queries the back-end for a specific user defined process and returns detailed information.
#' 
#' @param con connected and authenticated openEO client object (optional) otherwise [active_connection()]
#' is used.
#' @param id The id of the user process on the back-end
#' 
#' @return the user process as a ProcessInfo class (list object)
#' @export
describe_user_process = function(id, con=NULL) {
  tryCatch({
    con = .assure_connection(con)
    
    if (is.null(id)) {
      stop("No graph id specified. Cannot fetch unknown graph.")
    }
    
    if ("ProcessInfo" %in% class(id)) {
      id = id$id
    }
    
    tag = "graph_details"
    graph = con$request(tag = tag, parameters = list(id), authorized = TRUE, type = "application/json", auto_unbox = TRUE)
    
    class(graph) = "ProcessInfo"
    class(graph$process_graph) = "Json_Graph"
    return(graph)
  }, error = .capturedErrorToMessage)
}

#' Deletes a user process
#' 
#' The function initiates the deletion of a user defined process on the back-end. Only the owning user can delete
#' their process. The user defined process also should not be part of any particular job.
#' 
#' @param con connected and authorized openEO client object (optional) otherwise [active_connection()]
#' is used.
#' @param id the id of the user process
#' 
#' @export
delete_user_process = function(id, con=NULL) {
  tryCatch({
    tag = "graph_delete"
    
    con = .assure_connection(con)
    
    success = con$request(tag = tag, parameters = list(id), authorized = TRUE)
    if (success) {
      message(paste("Process '", id, "' was successfully deleted from the back-end", sep = ""))
    }
    return(success)
  }, error = .capturedErrorToMessage)
}

#' Stores a graph as user defined process on the back-end
#' 
#' Uploads the process graph information to the back-end and stores it. This can be used as a user defined process.
#' 
#' The parameter `submit` will be deprecated in the future. Please use `as(obj, "Process")`.
#' This function is useful when copying a JSON representation of your process graph to
#' another software. In that case use `udp = as(obj, "Process")` and simply print or call 
#' object `udp` on the console.
#' 
#' @param con connected and authorized openEO client object (optional) otherwise [active_connection()]
#' is used.
#' @param graph a process graph definition
#' @param id the title of the user process
#' @param summary the summary for the user process (optional)
#' @param description the description for the user process (optional)
#' @param submit whether to create a new user process at the openEO service or to create it for local use (default set to submit = TRUE)
#' @param ... additional parameters passed to jsonlite::toJSON() (like 'digits')
#' 
#' @return a list assembling a process graph description or the graph id if send
#' @export
create_user_process = function(graph, id=NULL, summary=NULL, description = NULL, submit=TRUE, con=NULL, ...) {
  tryCatch({
    con = .assure_connection(con)
    
    if (is.function(graph)) {
      graph = as(graph,"Graph")
    }
    
    if (isFALSE(submit)) warning("Parameter 'submit' is deprecated and will be removed in the future. Please use \"as(obj,\"Process\")\" to obtain a process locally.\n\n")
    
    if (length(id) == 0) {
      stop("No ID for was stated. Please name it before transmitting to the back-end.")
    }
    
    if ("ProcessNode" %in% class(graph)){
      # final node!
      graph = Graph$new(final_node = graph)
      
      p = Process$new(id = id, summary = summary, description = description, process_graph = graph)
    } else if ("Process" == class(graph)[1]){
      p = graph
      
      p$setId(id)
      p$setSummary(summary)
      p$setDescription(description)
    } else {
      if (length(graph) > 0 && "Graph" %in% class(graph)) {
        p = as(graph,"Process")
        
        p$setId(id)
        p$setSummary(summary)
        p$setDescription(description)
      } else {
        stop("The graph information is missing or not a list")
      }
    }
    
    
    process_graph_description = p$serialize()
    
    if (isTRUE(submit)) {
      tag = "graph_create_replace"
      
      # response = con$request(tag = tag, parameters = list(
      #   process_graph_id = id
      # ), authorized = TRUE, data = process_graph_description, raw = TRUE, ...)
      response = con$request(tag = tag, parameters = list(
          process_graph_id = id
        ),authorized = TRUE, data = process_graph_description, encodeType = "json", parsed=FALSE,...)
      message("Process was sucessfully stored on the back-end.")
      return(id) 
    } else {
      class(process_graph_description) = "ProcessInfo"
      return(process_graph_description)
    }
    
    
    
  }, error = .capturedErrorToMessage)
}

#' Update an user defined process
#' 
#' You can change details on an already created user defined process. You can either edit the meta data like the summary or the description. Or
#' you can replace the process graph. However, you cannot delete the process graph, but by passing NA to the meta data fields you can empty those
#' fields in the user defined process.
#' 
#' @param con connected and authorized openEO client object (optional) otherwise [active_connection()]
#' is used.
#' @param id process graph id
#' @param graph a process graph definition created by combining 'process()', 'collection()' or using a `ProcessGraphBuilder`
#' @param summary summary of the process graph (optional)
#' @param description description of the process graph (optional)
#' @param ... additional parameters passed to jsonlite::toJSON() (like 'digits')
#' @export
update_user_process = function(id, graph = NULL, summary = NULL, description = NULL, con=NULL, ...) {
  tryCatch({
    if (is.null(id)) {
      stop("Cannot replace unknown graph. If you want to store the graph / user defined process, use 'create_user_process' instead")
    }
    
    con = .assure_connection(con)
    
    graph_info = describe_user_process(con = con, id = id)
    process = processFromJson(json=graph_info)
    
    if (!is.null(graph)) {
      
      if (length(graph) == 1) {
        if (is.na(graph)) {
          stop("Cannot remove process graph from the element. Please replace it with another process graph, or ignore it via setting NULL")
        }
      }
      
      if ("Process" %in% class(graph)) {
        process = graph
        process$setId(id)
      } else {
        process$setProcessGraph(process_graph = graph)
      }
    }
    
    process$setSummary(summary)
    process$setDescription(description)
    
    requestBody = process$serialize()
    
    tag = "graph_create_replace"
    
    message = con$request(tag = tag, parameters = list(id), authorized = TRUE, data = requestBody, encodeType = "json", ...)
    
    if (is.null(message)) {
      message(paste("Process '", id, "' was successfully modified.", sep = ""))
      return(id)
    }
  }, error = .capturedErrorToMessage)
}

#' Validate a user process
#' 
#' Sends the process graph as a user process to the openEO service and validates it with the predefined and user-defined 
#' processes of the service.
#' 
#' @param con connected and authorized openEO client object (optional) otherwise [active_connection()]
#' is used.
#' @param graph the process graph that will be sent to the service to be validated
#' @param ... additional parameters passed to jsonlite::toJSON() (like 'digits')
#' 
#' @export
validate_process = function(graph, con=NULL, ...) {
  tryCatch({
    con = .assure_connection(con)
    
    if ("Graph" %in% class(graph)) {
      graph = graph$serialize()
    } else if ("ProcessNode" %in% class(graph)) {
      graph = Graph$new(final_node = graph)$serialize()
    } else if ("function" %in% class(graph)) {
      graph = as(graph,"Graph")$serialize()
    } else if ("Process" %in% class(graph)) {
      graph = (graph$serialize())$process_graph
    }
    
    
    if (!is.list(graph) || is.null(graph)) {
      stop("The graph information is missing or not a list")
    }
    
    requestBody = list(
      process_graph = graph)
    
    tag = "process_graph_validate"
    response = con$request(tag = tag, authorized = con$isLoggedIn(), data = requestBody, encodeType = "json", ...)
    
    if (length(response$errors) == 0) {
      message("Process graph was successfully validated.")
    } else {
      void = sapply(response$errors, function(obj) {
        paste0("[",obj$code,"] ",obj$message)
      })
      cat(paste0(void,collapse="\n"))
    }
    
    invisible(response)
  }, error = .capturedErrorToMessage)
}

# User Defined Process Collection ====
#' User Defined Process Collection
#' 
#' This object contains template functions from the users stored user defined processes (UDP), which can be reused in other process graphs.
#' 
#' @details
#' This object is an unlocked R6 object, that allows us to add new functions to this object at runtime. It is structured in the same way 
#' as the [ProcessCollection()] for predefined processes by the openEO back-end. A [UserProcessCollection()] is usually created at 
#' [user_processes()]. If you have submitted new user defined processes to the back-end, make sure to call [user_processes()] again
#' to fetch the latest status.
#' 
#' @name UserProcessCollection
#' 
#' @section Methods:
#' \describe{
#'    \item{`$new(con = NULL)`}{The object creator created an openEO connection.} 
#' } 
#' @section Arguments:
#' \describe{
#'    \item{con}{optional - an active and authenticated Connection (optional) otherwise [active_connection()]
#' is used.}
#' }
NULL 

#' @export
UserProcessCollection = R6Class(
  "UserProcessCollection",
  lock_objects = FALSE,
  public = list(
    initialize = function(con=NULL) {
      tryCatch({
        con = .assure_connection(con)
        udps = list_user_processes(con=con)
        process_names = names(udps)
        udps = lapply(udps,function(udp) as(describe_user_process(udp,con = con),"Process"))
        names(udps) = process_names
        
        private$processes = udps
        
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
    conection = NULL,
    node_ids = character(),
    processes = list(),
    getNodeIds = function() {private$node_ids}
  )
)

#' Process collection for user defined processes
#' 
#' The created process graphs via [create_user_process()] at the openEO service are user defined processes. 
#' They can be used for the creation of process graphs themselves. For processes provided by the 
#' particular openEO service the [processes()] function can be used to obtain a builder for those processes. 
#' Analogous to this idea, this function creates a builder object for user defined processes listed and described
#' in [describe_user_process()] and [list_user_processes()].
#' 
#' @param con a connection to an openEO back-end (optional). Otherwise [active_connection()]
#' is used in order to access personal user defined processes. You need to be logged in
#' 
#' @return [UserProcessCollection()]
#' 
#' @export
user_processes = function(con = NULL) {
  tryCatch({
    con = .assure_connection(con)
    return(UserProcessCollection$new(con=con))
  }, error = .capturedErrorToMessage)
}

setClass("ProcessInfo")
