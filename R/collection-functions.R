# The /collections/{collection_id} endpoint returns a detailed description of a collection. While print.CollectionInfo visualizes
# only the most important readable components and writes it to console the functions here aim at providing functions for using
# specific components of the collection for example the dimensionality

#' Returns dimension
#' 
#' @param x an object from which dimension information shall be returned
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
#' The function returns the dimension information of a CollectionInfo object. This object is usually obtained when calling 
#' \link{describe_collection}. It returns than the meta data information for the cube dimensions.
#' 
#' @param x a CollectionInfo object
#' @param ... parameters to pass on (not used)
#' 
#' @return dimension information as list
#' 
#' @export
dimensions.CollectionInfo = function(x, ...) {
    return(x$properties$`cube:dimensions`)
}
