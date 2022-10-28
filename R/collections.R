# data endpoint ----
#' List data on connected server
#'
#' List available collections stored on an openEO server and return them as a `CollectionList` - a named list of `Collection` objects. 
#' The names are the collection IDs. Although the result at [describe_collection()] is also a Collection, the Collection object of returned
#' from `list_collections()` is considered a list entry with less detailed information.
#' 
#' @param con Connection object (optional) otherwise [active_connection()]
#' is used.
#' 
#' @return object of class 'CollectionList'
#' 
#' @export
list_collections = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
    }, error = function(e){
        message("Not connected to an openEO service.")
        return(NULL)
    })
    
    return(con$getDataCollection())
}

#' Describe a collection
#' 
#' Queries an openEO back-end and retrieves a detailed description about one or more collections offered by the back-end.
#' 
#' @param collection Collection object or the collections id
#' @param con Authentication object (optional) otherwise [active_connection()]
#' is used.
#' 
#' @return a Collection object with detailed information about a collection.
#' @export
describe_collection = function(collection = NA, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
    }, error = function(e){
        message("Not connected to an openEO service.")
        return(NULL)
    })
    
    missing_collection = missing(collection) || length(collection) == 0 || is.na(collection) || nchar(collection) == 0
    
    if (missing_collection) {
        message("No or invalid collection id(s)")
        invisible(NULL)
    }
    
    if (length(collection) > 1 && !"Collection" %in% class(collection)) {
        return(lapply(collection, function(cid) {
            describe_collection(collection=cid, con=con)
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
                
                dim_names = names(info$`cube:dimensions`)
                info$`cube:dimensions` = lapply(dim_names, function(dimname) {
                    dim = info$`cube:dimensions`[[dimname]]
                    dim$name = dimname
                    class(dim) = "CubeDimension"
                    
                    return(dim)
                })
                names(info$`cube:dimensions`) = dim_names
                
                class(info$`cube:dimensions`) = "CubeDimensions"
            } else {
                warning(paste0("Description of collection '",collection,"' does not contain the mandatory data cube dimensions field."))
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
