# data endpoint ----
#' List data on connected server
#'
#' List available collections stored on an openEO server and return them as a `CollectionList` - a named list of `Collection` objects. 
#' The names are the collection IDs. Although the result at [describe_collection()] is also a Collection, the Collection object of returned
#' from `list_collections()` is considered a list entry with less detailed information. The collection list is stored internally as the package
#' environment variable `data_collection`, which can be accessed and set with `active_data_collection()`.
#' 
#' @param con Connection object (optional) otherwise [active_connection()]
#' is used.
#' @param collection the 'CollectionList' object of list_collections to be set as the active data collection in the package environment or left empty or NULL to return the package environment variable.
#' 
#' @return object of class 'CollectionList'
#' 
#' @importFrom rlang is_null 
#' @rdname list_collections
#'  
#' @export
list_collections = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
    }, error = function(e){
        message("Not connected to an openEO service.")
        return(NULL)
    })
  
  collection_list = active_data_collection()
  if (rlang::is_null(collection_list)) {
    tryCatch({
      tag = "data_overview"
      con = active_connection()
      
      collection_list = con$request(tag = tag, authorized = con$isLoggedIn(), type = "application/json")
      collection_list = collection_list$collections
      
      collection_list = lapply(collection_list, function(coll) {
        coll$extent$spatial = unlist(coll$extent$spatial$bbox)
        coll$extent$temporal = lapply(coll$extent$temporal$interval, function(t) {
          # t is list
          return(lapply(t,function(elem) {
            if (is.null(elem)) return(NA)
            else return(elem)
          }))
          
        })
        
        class(coll) = "Collection"
        return(coll)
      })
      
      class(collection_list) = "CollectionList"
      
      collection_names = sapply(collection_list, function(coll) {
        return(coll$id)
      })
      
      names(collection_list) = collection_names
      
      collection_list = active_data_collection(collection=collection_list)
      
    }, error = .capturedErrorToMessage)
  } 
  
  return(collection_list)
}

#' @rdname list_collections
#' @export
active_data_collection = function(collection=NULL) {
  if (is.null(collection)) {
    return(get(x = "data_collection", envir = pkgEnvironment))
  } else if ("CollectionList" %in% class(collection)) {
    assign(x = "data_collection", value = collection, envir = pkgEnvironment)
    invisible(collection)
  } else {
    stop(paste0("Cannot set data collection with object of class '",utils::head(class(collection),1),"'"))
  }
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
    
    missing_collection = length(collection) == 0 || any(is.na(collection)) || (is.character(collection) && length(collection) == 1 && nchar(collection) == 0)
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
