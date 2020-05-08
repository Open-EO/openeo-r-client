# data endpoint ----
#' List Data on conected server
#'
#' List available collections stored on a openEO server
#' @param con Connection object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @export
list_collections = function(con=NULL) {
    con = .assure_connection(con)
    
    return(con$getDataCollection())
    
}

#' Describe a product
#' 
#' Queries an openeo back-end and retrieves a detailed description about one or more collections offered by the back-end
#' 
#' @param con Authentication object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param id id of a product/collection to be described
#' 
#' @return a list of detailed information about a product/collection
#' @export
describe_collection = function(con=NULL, id = NA) {
    con = .assure_connection(con)
    
    missing_id = !missing(id) && !is.na(id)
    
    if (!missing_id) {
        message("No or invalid collection id(s)")
        invisible(NULL)
    }
    if (length(id) > 1) {
        return(lapply(id, function(cid) {
            describe_collection(con, cid)
        }))
    } else {
        tryCatch({
            tag = "data_details"
            
            info = con$request(tag = tag, parameters = list(id), authorized = con$isLoggedIn(), type = "application/json", auto_unbox = TRUE)
            
            class(info) = "CollectionInfo"
            
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
            
            # replace null in temporal extent with NA (which will be transformed into JSON null)
            info$extent$temporal = lapply(info$extent$temporal, function(t) {
                if (is.null(t)) return(NA)
                else return(t)
            })
            
            return(info)
        }, error = .capturedErrorToMessage)
    }
}
