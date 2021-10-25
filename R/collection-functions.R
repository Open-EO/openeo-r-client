# The /collections/{collection_id} endpoint returns a detailed description of a collection. While print.Collection visualizes the most important
# readable components and writes it to console, these functions provide functions for using specific components of the collection, as for example the
# dimensionality

#' Returns dimension
#' 
#' @param x an object from which dimension information is returned
#' @param ... additional parameters to pass on to internal functions
#' 
#' @return dimension information as list
#' 
#' @export
dimensions = function(x, ...) {
    UseMethod("dimensions", x)
}

#' Returns dimension information
#' 
#' The function returns the dimension information of a Collection object. This object is usually obtained when calling 
#' \link{describe_collection}. It returns the meta data information for the cube dimensions.
#' 
#' @param x a Collection object
#' @param ... parameters to pass on (not used)
#' 
#' @return dimension information as list
#' 
#' @export
dimensions.Collection = function(x, ...) {
  if (length(x$`cube:dimensions`) > 0) {
    return(x$`cube:dimensions`)
  } else {
    return(dimensions(describe_collection(collection=x$id)))
  }
  
    
}
