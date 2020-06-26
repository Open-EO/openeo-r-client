# data endpoint ----
#' List Data on conected server
#'
#' List available collections stored on a openEO server and return them as a CollectionList which is a named list of Collection objects. 
#' The names are the collection IDs. Although the result at 'describe_collection' is also a Collection, this functions result will contain
#' lesser information than the detailed description received at the other function.
#' 
#' @param con Connection object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return object of class 'CollectionList'
#' 
#' @export
list_collections = function(con=NULL) {
    con = .assure_connection(con)
    
    return(con$getDataCollection())
}

#' Describe a product
#' 
#' Queries an openeo back-end and retrieves a detailed description about one or more collections offered by the back-end
#' 
#' @param collection Collection object or the collections id
#' @param con Authentication object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return a Collection object with detailed information about a collection.
#' @export
describe_collection = function(collection = NA, con=NULL) {
    con = .assure_connection(con)
    
    missing_collection = missing(collection) || length(collection) == 0 || is.na(collection) || nchar(collection) == 0
    
    if (missing_collection) {
        message("No or invalid collection id(s)")
        invisible(NULL)
    }
    
    if (length(collection) > 1 && !"Collection" %in% class(collection)) {
        return(lapply(collection, function(cid) {
            describe_collection(con, cid)
        }))
    } else {
        tryCatch({
            tag = "data_details"
            
            if ("Collection" %in% class(collection)) {
                collection = collection$id
            }
            
            info = con$request(tag = tag, parameters = list(collection), authorized = con$isLoggedIn(), type = "application/json", auto_unbox = TRUE)
            
            class(info) = "Collection"
            
            if (!is.null(info$summaries$`eo:bands`)) {
                if (length(info$summaries$`eo:bands`) == 1 && is.null(info$summaries$`eo:bands`[[1]]$name)) {
                    info$summaries$`eo:bands` = info$summaries$`eo:bands`[[1]]
                }
                class(info$summaries$`eo:bands`) = "BandList"
            }
            
            if (!is.null(info$summaries$`sar:bands`)) {
                if (length(info$summaries$`sar:bands`) == 1 && is.null(info$summaries$`sar:bands`[[1]]$name)) {
                    info$summaries$`sar:bands` = info$summaries$`sar:bands`[[1]]
                }
                class(info$summaries$`sar:bands`) = "BandList"
            }
            
            if (!is.null(info$`cube:dimensions`)) {
                class(info$`cube:dimensions`) = "CubeDimensions"
            } else {
                warning(paste0("Description of collection '","' does not contain the mandatory data cube dimensions field."))
            }
            
            info$extent$spatial = unlist(info$extent$spatial$bbox)
            
            # replace null in temporal extent with NA (which will be transformed into JSON null)
            info$extent$temporal = lapply(info$extent$temporal$interval, function(t) {
                # t is list
                return(lapply(t,function(elem) {
                    if (is.null(elem)) return(NA)
                    else return(elem)
                }))
                
            })
            
            return(info)
        }, error = .capturedErrorToMessage)
    }
}
