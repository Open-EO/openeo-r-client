#' @include process_graph_building.R
NULL

# process graph endpoint ----

#' Lists the Ids of the process graphs from the current user
#' 
#' Queries the back-end to retrieve a list of graph ids that the current user has stored on the back-end.
#' 
#' @param con connected and authenticated openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return a named list of user defined processes (\code{ProcessInfo})
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
    
    return(listOfUserDefinedProcesses)
    
  }, error = .capturedErrorToMessage)
}

#' Fetches the representation of a stored user defined process
#' 
#' The function queries the back-end for a specific user defined process and returns detailled information.
#' 
#' @param con connected and authenticated openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param id The id of the user process at the back-end
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
#' The function initiates the deletion of a user defined process at the back-end. Only the owning user can delete
#' their process. The user defined process also should not be part of any particular job.
#' 
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
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
      message(paste("Graph '", id, "' was successfully deleted from the back-end", sep = ""))
    }
    return(success)
  }, error = .capturedErrorToMessage)
}

#' Stores a graph as user defined process at the back-end
#' 
#' Uploads the process graph information to the back-end and stores it for reuse as a user defined process.
#' 
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param graph a process graph definition
#' @param id the title of the user process
#' @param summary the summary for the user process (optional)
#' @param description the description for the user process (optional)
#' @param submit whether to create a new user process at the openEO service or to create it for local use (default set to submit = TRUE)
#' 
#' @return a list assembling a process graph description or the graph id if send
#' @export
create_user_process = function(graph, id, summary=NULL, description = NULL, submit=TRUE, con=NULL) {
  tryCatch({
    con = .assure_connection(con)
    
    if (is.function(graph)) {
      graph = as(graph,"Graph")
    }
    
    if ("ProcessNode" %in% class(graph)){
      # final node!
      graph = Graph$new(final_node = graph)
    }
    
    if (!"Graph" %in% class(graph) || is.null(graph)) {
      stop("The graph information is missing or not a list")
    }
    
    p = Process$new(id = id, summary = summary, description = description, process_graph = graph)
    process_graph_description = p$serialize()
    
    if (submit) {
      tag = "graph_create_replace"
      
      response = con$request(tag = tag, parameters = list(
        process_graph_id = id
      ), authorized = TRUE, data = process_graph_description, raw = TRUE)
      
      message("Graph was sucessfully stored on the backend.")
      return(id) 
    } else {
      return(process_graph_description)
    }
    
    
    
  }, error = .capturedErrorToMessage)
}

#' Modify the current graph with a given
#' 
#' Upload a process graph to the back-end under a given (existing) process graph.
#' 
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param id process graph id
#' @param graph a process graph definition created by chaining 'process()', 'collection()' or using a ProcessGraphBuilder
#' @param summary summary of the process graph (optional)
#' @param description description of the process graph (optional)
#' @export
update_user_process = function(id, graph = NULL, summary = NULL, description = NULL, con=NULL) {
  tryCatch({
    if (is.null(id)) {
      stop("Cannot replace unknown graph. If you want to store the graph / user defined process, use 'create_user_process' instead")
    }
    
    con = .assure_connection(con)
    
    graph_info = describe_user_process(con = con, id = id)
    process = processFromJson(json=graph_info)
    
    if (!is.null(graph)) {
      
      
      if (is.na(graph)) {
        stop("Cannot remove process graph from the element. Please replace it with another process graph, or ignore it via setting NULL")
      } else {
        process$setProcessGraph(process_graph = graph)
      }
    }
    
    process$setSummary(summary)
    process$setDescription(description)
    
    requestBody = process$serialize()
    
    tag = "graph_create_replace"
    
    message = con$request(tag = tag, parameters = list(id), authorized = TRUE, data = requestBody, encodeType = "json")
    
    if (is.null(message)) {
      message(paste("Process graph '", id, "' was successfully modified.", sep = ""))
      return(id)
    }
  }, error = .capturedErrorToMessage)
}

#' Validates a user process
#' 
#' Sends the process graph as a user process to the openEO service and validates it against the predefined and user-defined 
#' processes of the service.
#' 
#' @param con connected and authorized openeo client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param graph the process graph that will be sent to the service and is going to be validated
#' 
#' @export
validate_process = function(graph, con=NULL) {
  tryCatch({
    con = .assure_connection(con)
    
    if ("Graph" %in% class(graph)) {
      graph = graph$serialize()
    } else if ("ProcessNode" %in% class(graph)) {
      graph = Graph$new(final_node = graph)$serialize()
    } else if ("function" %in% class(graph)) {
      graph = as(graph,"Graph")$serialize()
    }
    
    if (!is.list(graph) || is.null(graph)) {
      stop("The graph information is missing or not a list")
    }
    
    requestBody = list(
      process_graph = graph)
    
    tag = "process_graph_validate"
    response = con$request(tag = tag, authorized = con$isLoggedIn(), data = requestBody, encodeType = "json")
    
    if (length(response$errors) == 0) {
      message("Graph was sucessfully validated.")
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
#' This object contains template functions for process graph building from the user defined processes at an openEO service. 
#' This object is an R6 object that is not locked, in order to add new functions at runtime, in the same fashion as 
#' \code{\link{ProcessCollection}} for predefined processes. The object is usually created at 
#' \code{\link{user_processes}} and resembles a given status, meaning, when a new user process is created or another one
#' is updated the \code{UserProcessCollection} needs to be recreated with "fresh" data.
#' 
#' @name UserProcessCollection
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{$new(con = NULL)}}{The object creator created an openEO connection.} 
#' } 
#' @section Arguments:
#' \describe{
#'    \item{con}{optional an active and authenticated Connection (optional) otherwise \code{\link{active_connection}}
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
#' The created process graphs via \code{\link{create_user_process}} at the openEO service are user defined processes. 
#' This means that they can be used within the creation of process graphs themselves. For processes provided by the 
#' particular openEO service the \code{\link{processes}} function can be used to obtain a builder for those processes. 
#' Analoguous to this idea, this function creates a builder object for user defined proceses which are listed and described
#' with \code{\link{describe_user_process}} and \code{\link{list_user_processes}}.
#' 
#' @param con a connection to an openeo back-end (optional) otherwise \code{\link{active_connection}}
#' is used in order to access personal user defined processes you need to be logged in
#' 
#' @return \code{\link{UserProcessCollection}}
#' 
#' @export
user_processes = function(con = NULL) {
  tryCatch({
    con = .assure_connection(con)
    return(UserProcessCollection$new(con=con))
  }, error = .capturedErrorToMessage)
}

setClass("ProcessInfo")