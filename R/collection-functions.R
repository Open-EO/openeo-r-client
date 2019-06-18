# The /collections/{collection_id} endpoint returns a detailed description of a collection. While print.CollectionInfo visualizes only the most important
# readable components and writes it to console the functions here aim at providing functions for using specific components of the collection 
# for example the dimensionality

#' @export
dimensions = function(x, ...) {
  UseMethod("dimensions",x)
}

#' @export
dimensions.CollectionInfo = function(x) {
  return(x$properties$`cube:dimensions`)
}