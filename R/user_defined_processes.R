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
    
    graph_info = get_user_process(con = con, id = id)
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

#' Process collection for user defined processes
#' 
#' The created process graphs via \code{\link{create_user_process}} at the openEO service are user defined processes. 
#' This means that they can be used within the creation of process graphs themselves. For processes provided by the 
#' particular openEO service the \code{\link{processes}} function can be used to obtain a builder for those processes. 
#' Analoguous to this idea, this function creates a builder object for user defined proceses which are listed and described
#' with \code{\link{get_user_process}} and \code{\link{list_user_processes}}.
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