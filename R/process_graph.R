# process graph endpoint ----

#' Lists the Ids of the process graphs from the current user
#' 
#' Queries the back-end to retrieve a list of graph ids that the current user has stored on the back-end.
#' 
#' @param con connected and authenticated openeo client object
#' @return vector of process graph ids
#' @export
list_process_graphs = function(con) {
    tryCatch({
        tag = "graph_overview"
        
        listOfGraphShortInfos = con$request(tag = tag, authorized = TRUE)
        listOfGraphShortInfos = listOfGraphShortInfos$process_graphs
        
        table = .listObjectsToDataFrame(listOfGraphShortInfos)
        if (ncol(table) == 0 || nrow(table) == 0) {
            message("No process graphs are currently stored on the back-end.")
            invisible(table)
        }
        
        table = table[, c("id", "title", "description")]
        if (isNamespaceLoaded("tibble")) 
            table = tibble::as_tibble(table)
        
        return(table)
    }, error = .capturedErrorToMessage)
}

#' Fetches the representation of a stored graph
#' 
#' The function queries the back-end for a specific user defined process graph
#' 
#' @param con connected and authenticated openeo client object
#' @param id The id of a process graph on the back-end
#' 
#' @return the process graph as list
#' @export
describe_process_graph = function(con, id) {
    tryCatch({
        if (is.null(id)) {
            stop("No graph id specified. Cannot fetch unknown graph.")
        }
        
        tag = "graph_details"
        graph = con$request(tag = tag, parameters = list(id), authorized = TRUE, type = "application/json", auto_unbox = TRUE)
        
        class(graph) = "ProcessGraphInfo"
        class(graph$process_graph) = "Json_Graph"
        return(graph)
    }, error = .capturedErrorToMessage)
}

#' Deletes a previously stored process graph
#' 
#' The function initiates the deletion of a process graph on the back-end. Only the owning user can delete
#' a graph. The graph also should not be part of any particular job.
#' 
#' @param con connected and authorized openeo client object
#' @param id the id of the graph
#' 
#' @export
delete_process_graph = function(con, id) {
    tryCatch({
        tag = "graph_delete"
        
        success = con$request(tag = tag, parameters = list(id), authorized = TRUE)
        if (success) {
            message(paste("Graph '", id, "' was successfully deleted from the back-end", sep = ""))
        }
        return(success)
    }, error = .capturedErrorToMessage)
}

#' Stores a graph on the back-end
#' 
#' Uploads the process graph information to the back-end and stores it for reuse.
#' 
#' @param con connected and authorized openeo client object
#' @param graph a process graph definition
#' @param title the title of the process graph (optional)
#' @param description the description of a process graph (optional)
#' @export
create_process_graph = function(con, graph, title = NULL, description = NULL) {
    tryCatch({
        
        
        if (!"Graph" %in% class(graph) || is.null(graph)) {
            stop("The graph information is missing or not a list")
        }
        
        requestBody = list(title = title, description = description, process_graph = graph$serialize())
        
        tag = "new_graph"
        response = con$request(tag = tag, authorized = TRUE, data = requestBody, raw = TRUE)
        
        message("Graph was sucessfully stored on the backend.")
        locationHeader = headers(response)$location
        split = unlist(strsplit(locationHeader, "/"))
        return(trimws(split[length(split)]))
    }, error = .capturedErrorToMessage)
}

#' Modify the current graph with a given
#' 
#' Upload a process graph to the back-end under a given (existing) process graph.
#' 
#' @param con connected and authorized openeo client object
#' @param id process graph id
#' @param graph a process graph definition created by chaining 'process()', 'collection()' or using a ProcessGraphBuilder
#' @param title title of the process graph (optional)
#' @param description description of the process graph (optional)
#' @export
update_process_graph = function(con, id, graph = NULL, title = NULL, description = NULL) {
    tryCatch({
        if (is.null(id)) {
            stop("Cannot replace unknown graph. If you want to store the graph, use 'create_process_graph' instead")
        }
        
        requestBody = list()
        
        if (!is.null(graph)) {
            if (is.na(graph)) {
                stop("Cannot remove process graph from the element. Please replace it with another process graph, or ignore it via setting NULL")
            } else if (!is.list(graph)) {
                stop("The graph information is missing or not a list")
            } else {
                requestBody[["process_graph"]] = graph
            }
        }
        
        if (!is.null(title)) {
            if (is.na(title)) {
                requestBody[["title"]] = NULL
            } else {
                requestBody[["title"]] = title
            }
        }
        if (!is.null(description)) {
            if (is.na(description)) {
                requestBody[["description"]] = NULL
            } else {
                requestBody[["description"]] = description
            }
        }
        
        tag = "graph_replace"
        
        message = con$request(tag = tag, parameters = list(id), authorized = TRUE, data = requestBody, encodeType = "json")
        
        if (is.null(message)) {
            message(paste("Process graph '", id, "' was successfully modified.", sep = ""))
            invisible(TRUE)
        }
    }, error = .capturedErrorToMessage)
}

#' Validates a process graph
#' 
#' Sends the process graph to the back-end and validates it against the offered processes by the backend.
#' 
#' @param con connected and authorized openeo client object
#' @param graph the process graph that will be sent to the back-end and is being validated
#' 
#' @export
validate_process_graph = function(con, graph) {
    tryCatch({
        if ("Graph" %in% class(graph)) 
            graph = graph$serialize()
        
        if (!is.list(graph) || is.null(graph)) {
            stop("The graph information is missing or not a list")
        }
        
        requestBody = list(process_graph = graph)
        
        tag = "process_graph_validate"
        response = con$request(tag = tag, authorized = con$isLoggedIn(), data = requestBody, encodeType = "json")
        
        message("Graph was sucessfully validated.")
        invisible(response)
    }, error = .capturedErrorToMessage)
}

#' Get a process graph builder from the connection
#' 
#' Queries the connected back-end for all available processes and collection names and registers them via R functions on
#' a Graph object to model a process graph in R. To get a better overview about the process graph building, please have
#' a look at \url{https://github.com/Open-EO/openeo-r-client/wiki/Process-Graph-Building}
#' 
#' @param con a connection to an openeo back-end
#' @return a Graph object with the offered processes of the backend
#' @export
process_graph_builder = function(con) {
    tryCatch({
        if (is.null(con$processes)) {
            temp = list_processes(con)
        }
        
        collections = list_collections(con)$collections
        cids = sapply(collections, function(coll) coll$id)
        collections = as.list(cids)
        names(collections) = cids
        
        return(Graph$new(con, collections))
    }, error = .capturedErrorToMessage)
}
