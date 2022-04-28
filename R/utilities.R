#' @include process_graph_building.R
#' @importFrom jsonlite toJSON
NULL

# utility functions ----
.not_implemented_yet = function() {
    message("Not implemented yet.")
}

.not_supported_by_client = function() {
    message("Not supported by the current client version.")
}

.not_supported_by_backend = function() {
    message("The function is not supported by the current back-end version.")
}

.no_information_by_backend = function(what) {
    message(paste0("There are no information about ",what," available at the service."))
}


.toJSON = function(x, ...) {
  if (any(c("Graph","Process") %in% class(x))) {
    return(jsonlite::toJSON(x$serialize(), auto_unbox = TRUE, pretty = TRUE, force = TRUE, digits = NA))
  } else {
    stop("Parameter is no Graph or Process object.")
    invisible(NULL)
  }
}

#' *toJSON functions
#' 
#' Those functions serialized a Graph or Process object into JSON text. They are deprecated. Use \code{toJSON} instead.
#' 
#' @name graphToJSON-deprecated
#' @rdname toJSON-deprecated
#' @param x Graph or Process object
#' @param ... arguments for jsonlite::toJSON
#' @export
graphToJSON = function(x,...) {
  .Deprecated(new="toJSON",package = "openeo")
  do.call("toJSON",args=c(x=x,list(...)))
}

#' @name processToJSON-deprecated
#' @rdname toJSON-deprecated
#' @export
processToJSON = function(x, ...) {
  .Deprecated(new="toJSON",package = "openeo")
  do.call("toJSON",args=c(x=x,list(...)))
}

#' Wrapper for toJSON
#' 
#' This function is intended to have a preconfigured toJSON function
#' to allow a user to visualize a process or graph in JSON. The JSON representation
#' of a process is the same as it will be sent to the back-end.
#' 
#' @param x a Process or Graph object
#' @param ... additional parameters that are passed to jsonlite::toJSON
#' @return JSON string of the process as a character string 
#' 
#' @name toJSON
#' @rdname toJSON
#' 
#' @examples
#' \dontrun{
#' # node is a defined process node
#' process = as(node, "Process")
#' toJSON(process)
#' 
#' graph = process$getProcessGraph()
#' toJSON(graph)
#' }
NULL

#' @rdname toJSON
setMethod(f="toJSON", signature = "Process",definition = .toJSON)

#' @rdname toJSON
setMethod(f="toJSON", signature = "Graph",definition = .toJSON)

.capturedErrorToMessage = function(e) {
    message(e)
    invisible(NULL)
}

.version_sort = function(v) {
    sem_parser = function(ver) {
        if (length(ver) == 1) {
            split = strsplit(ver,"[.-]")[[1]]
            result = list(0,0,0,NA)
            charMask = sapply(split,function(x) {
                suppressWarnings({
                    i = as.integer(x)
                    if (is.na(i)) TRUE else FALSE
                })
            })
            
            if (any(charMask)) {
                result[[4]] = split[which(charMask)]
                split[-which(charMask)]
                result[1:(which(charMask)-1)] = as.integer(split[1:(which(charMask)-1)])
            } else {
                result[1:length(split)] = as.integer(split)
            }
            result
        } else {
            lapply(ver,sem_parser)
        }
    }
    
    if (length(v$api_version) == 1) {
        versions = as.data.frame(do.call(cbind,sem_parser(v$api_version)))    
    } else {
        versions = as.data.frame(do.call(rbind,sem_parser(v$api_version)))
    }
    
    for (i in 1:ncol(versions)) {
        versions[,i] = unlist(versions[,i])
        
        if (i == 4) {
            versions[,4] = sapply(versions[,4],function(x){
                ifelse(x=="NA",NA,x)
            })
        }
    }
    
    temp = merge(versions,v,by=0)
    v[with(temp,order(production,V1,V2,V3,V4,na.last=FALSE,decreasing = TRUE)),]
}

.assure_connection = function(con) {
    if (missing(con) || is.null(con)) {
        con = active_connection()
    }
    
    if (is.null(con)) stop("Cannot find a valid and active openEO connection. Please use 'connect' to connect with an openEO service.")
    
    return(con)
}

.is_package_installed = function(pkg) {
    return(nzchar(system.file(package=pkg)))
}

#' Retrieves the status
#' 
#' The function refreshes the passed object and returns its status.
#' 
#' @param x an object like Job
#' @param ... currently not used
#' @return status as character
#' @export
status = function(x, ...) {
    UseMethod("status",x)
}

.clean_empty_fields = function(l) {
    fields = names(l)
    
    for(f in fields) {
        if (length(l[[f]]) == 0) {
            l[[f]] = NULL
        } else if (is.list(l[[f]])) {
            r = .clean_empty_fields(l[[f]])
            if (length(r) == 0) {
                r = NULL
            }
            
            l[[f]] = r
        }
    }
    return(l)
}

.find_process_by_name = function(graph, process_id) {
  
  if ("Graph" != class(graph)[1]) {
    graph = parse_graph(graph$serialize())
  }
  
  ns=graph$getNodes()
  subset = which(sapply(ns, function(x) {
    id = x$getId()
    !is.null(id) && id == process_id
  }))
  if (length(subset) == 0) {
    return(list())
  } else {
    return(ns[subset])
  }
}

# Is this in a Jupyter notebook?
is_jupyter = function() {
  return (isTRUE(getOption('jupyter.in_kernel')))
}

# Is this in a RStudio notebook?
is_rstudio_nb = function() {
  return (isTRUE(getOption('rstudio.notebook.executing')))
}

# Is this in a RMarkdown / knitr context?
is_rmd = function() {
  return (isTRUE(getOption('knitr.in.progress')) && knitr::is_html_output() == TRUE)
}