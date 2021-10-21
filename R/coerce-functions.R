#' @include process_graph_building.R
#' @include user_defined_processes.R
#' @importFrom methods S3Part<-
NULL

.removeNullEntries = function(list) {
    list[-which(sapply(list, is.null))]
}

# x has to be an unnamed list
.listObjectsToDataFrame = function(x, extract = NULL) {
    if (is.null(extract)) {
        # extract types
        template = do.call(c, lapply(x, function(col) {
            lapply(col, function(row) {
                if (is.null(row)) {
                  return(character())
                }
                
                if (is.list(row)) {
                  return(character())
                }
                
                return(do.call(class(row), list(length = 0)))
            })
        }))
        template = template[unique(names(template))]
    } else {
        template = extract
        names(template) = extract
    }
    
    table = do.call("data.frame", args = append(template, list(stringsAsFactors = FALSE)))
    colnames(table) = names(template)
    
    for (index in seq_along(x)) {
        # new empty row in the table
        table[index, ] = NA
        
        entry = x[[index]]
        if (length(entry) > 0) {
            for (i in seq_along(entry)) {
                val = entry[[i]]
                if (is.list(val)) {
                  entry[[i]] = list(val)  # data.frames allow only list with one value as column data type
                }
            }
            # select all entry values that available at max
            entry = entry[names(template)]
            # if some entries are not there replace NULL with NA to be shown in the data.frame
            entry[sapply(entry, is.null)] = NA
            names(entry) = names(template)
            
            for (name in names(entry)) {
                val = entry[[name]]
                if (length(val) > 1) 
                  val = list(val)
                
                table[nrow(table), name][[1]] = val
            }
        }
        
    }
    return(table)
}

#' Coercions into data.frame objects
#' 
#' The openeo package offers functions to transform list objects obtained from JSON
#' into data.frames. This is mostly applied in list_* functions.
#' 
#' @name as.data.frame
#' @param x the list object that will be coerced
#' @param ... potentially additional parameters to pass on to internal functions like 'extract'
#' 
#' @details 
#' The parameter 'extract' is used as an additional parameter to extract specific values of the output list
#' / json. The value for the parameters is a vector of character like c('id','title')
#' 
#' @return a data.frame
#' 
#' @export
as.data.frame.JobList = function(x, ...) {
    x = unname(x)
    params = list(...)
    return(.listObjectsToDataFrame(x, extract = params$extract))
}

#' @rdname as.data.frame
#' @export
as.data.frame.ServiceList = function(x, ...) {
    x = unname(x)
    params = list(...)
    table = .listObjectsToDataFrame(x, extract = params$extract)
    return(table)
}

#' @rdname as.data.frame
#' @export
as.data.frame.BandList = function(x, ...) {
    x = unname(x)
    params = list(...)
    table = .listObjectsToDataFrame(x, extract = params$extract)
    return(table)
}

#' @rdname as.data.frame
#' @export
as.data.frame.CollectionList = function(x, ...) {
    colls = x
    
    params = list(...)
    
    colls = lapply(colls, function(collection) {
        extent = collection$extent
        collection$extent = NULL
        collection$extent.spatial = extent$spatial
        collection$extent.temporal = extent$temporal
        return(collection)
    })
    
    table = .listObjectsToDataFrame(colls, extract = params$extract)
    
    return(table)
}

#' @rdname as.data.frame
#' @export
as.data.frame.VersionsList = function(x, ...) {
    versions = x$versions
    params = list(...)
    table = .listObjectsToDataFrame(versions, extract = params$extract)
    
    # production is here an optional field and defaults to FALSE. In case it was not set do it here
    if (!"production" %in% names(table)) table$production = rep(FALSE,1:nrow(table))
    
    return(table[c("api_version", "production", "url")])
}

#' @rdname as.data.frame
#' @export
as.data.frame.FileFormatList = function(x, ...) {
    # this will just be an overview
    output = .listObjectsToDataFrame(unname(x$output),extract = c("name","title"))
    output$direction = "output"
    
    if (nrow(x$input) > 0) {
        input = .listObjectsToDataFrame(unname(x$input),extract = c("name","title"))
        input$direction = "input"
        table = rbind(output,input)
    } else {
        table = output
    }
    
    
    return(table)
}

#' @export
as.data.frame.AssetList = function(x, ...) {
    params = list(...)
    x = lapply(names(x), function(asset) {
        return(c(asset_name=asset,x[[asset]]))
    })
    table = .listObjectsToDataFrame(x, extract = params$extract)
    return(table)
}

#' Coercion into Graph
#' 
#' Creates a \code{Graph} object from a \code{\link{ProcessNode}}, \code{function} or \code{ProcessInfo} (Exchange object for 
#' predefined and stored user-defined processes).
#' 
#' Those pure \code{Graph} objects shall only be used internally. If you want to use this 
#' information in terms of interacting with the back-end directly via JSON please use 
#' \code{\link{as.Process}}. This function might be removed from the package function export in the future.
#' 
#' @name as.Graph
#' 
#' @param from the source from which to coerce (\code{ProcessNode}, \code{function} or \code{ProcessInfo})
#' @return \code{\link{Graph}}
#' 
#' @export
as.Graph.ProcessNode = function(from) {
    return(Graph$new(final_node=from))
}

#' @rdname as.Graph
#' @export
as.Graph.function = function(from) {
    return(.function_to_graph(value=from))
}

#' @rdname as.Graph
#' @export
as.Graph.ProcessInfo = function(from) {
    return(parse_graph(json=from))
}

#' @export
as.character.UdfRuntime = function(x, ...) {
    return(x$id)
}

#' @export
as.character.UdfRuntimeVersion = function(x, ...) {
    return(x$version)
}

#' @export
as.character.CubeDimension = function(x, ...) {
    return(x$name)
}


#' Coerce into a Process
#' 
#' This function converts objects into a process. If no meta data is provided it will return a valid, but yet not back-end storeable user defined process.
#' 
#' @name as.Process
#' 
#' @param from the source from which to coerce (\code{ProcessInfo}, \code{\link{Graph}} or \code{\link{ProcessNode}})
#' @return \code{\link{Process}}
#'    
#' @export
as.Process.ProcessInfo = function(from) {
    return(processFromJson(from))
}

#' @name as.Process
#' @export
as.Process.Graph = function(from) {
    Process$new(id=NULL,process_graph=from)
}

#' @name as.Process
#' @export
as.Process.ProcessNode = function(from) {
    Process$new(id=NULL,process_graph=from)
}

suppressWarnings({
    setAs(from="ProcessNode",to="Graph",as.Graph.ProcessNode)
    setAs(from="function",to="Graph",as.Graph.function)
    setAs(from="ProcessInfo",to="Graph",as.Graph.ProcessInfo)
    setAs(from="ProcessInfo",to="Process",as.Process.ProcessInfo)
    setAs(from="Graph",to="Process",as.Process.Graph)
    setAs(from="ProcessNode",to="Process",as.Process.ProcessNode)
})

