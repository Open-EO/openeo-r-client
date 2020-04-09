# utility functions ----
.not_implemented_yet = function() {
    warning("Not implemented yet.")
}

.not_supported_by_client = function() {
    warning("Not supported by the current client version.")
}

.not_supported_by_backend = function() {
    warning("The function is not supported by the current back-end version.")
}

.no_information_by_backend = function(what) {
    warning(paste0("There are no information about ",what," available at the service."))
}

#' Wrapper for toJSON
#' 
#' This function is intended to have a preconfigured toJSON function
#' to allow a user to visualize the process graph in JSON (like it will
#' be sent to the back-end)
#' 
#' @param graph a list / nested list representing the process graph
#' @return JSON string of the process graph as a character string 
#' 
#' @export
# dont't expose it later
graphToJSON = function(graph) {
    # task is a Graph object
    
    if ("Graph" %in% class(graph)) {
        return(toJSON(graph$serialize(), auto_unbox = T, pretty = T, force = TRUE))
    } else {
        stop("Parameter is no Graph object.")
        invisible(NULL)
    }
    
}

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
    
    versions = as.data.frame(do.call(rbind,sem_parser(v$api_version)))
    
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
