# data endpoint ----
#' List Data on conected server
#'
#' List available collections stored on a openEO server
#' @param con Connection object
#' @export
list_collections = function(con) {
    con = .assure_connection(con)
    
    tryCatch({
        tag = "data_overview"
        
        listOfProducts = con$request(tag = tag, authorized = con$isLoggedIn(), type = "application/json")
        class(listOfProducts) = "CollectionList"
        return(listOfProducts)
    }, error = .capturedErrorToMessage)
    
}

#' Describe a product
#' 
#' Queries an openeo back-end and retrieves a detailed description about one or more collections offered by the back-end
#' 
#' @param con Authentication object
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
            
            if (!is.null(info$properties$`eo:bands`)) {
                if (length(info$properties$`eo:bands`) == 1 && is.null(info$properties$`eo:bands`[[1]]$name)) {
                    info$properties$`eo:bands` = info$properties$`eo:bands`[[1]]
                }
                class(info$properties$`eo:bands`) = "BandList"
            }
            
            if (!is.null(info$properties$`sar:bands`)) {
                if (length(info$properties$`sar:bands`) == 1 && is.null(info$properties$`sar:bands`[[1]]$name)) {
                    info$properties$`sar:bands` = info$properties$`sar:bands`[[1]]
                }
                class(info$properties$`sar:bands`) = "BandList"
            }
            
            if (!is.null(info$properties$`cube:dimensions`)) {
                class(info$properties$`cube:dimensions`) = "CubeDimensions"
            } else {
                warning(paste0("Description of collection '","' does not contain the mandatory data cube dimensions field."))
            }
            
            
            return(info)
        }, error = .capturedErrorToMessage)
    }
}
