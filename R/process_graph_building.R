#' Create a process object
#'
#' defines a process with arguments
#' @param process A process that might be chained with this process
#' @param process_id ID of the process offered by the connected openEO backend
#' @param prior.name The name of the prior process / collection, default "collections"
#' @param ... named arguments that are passed to the process description
#' @export
process = function(process=NULL, process_id, prior.name="collections", ...) {
  # type check "process" either collection or process
  res = list()
  arguments = list()
  if (!missing(process) && !is.null(process)) {
    if (is.list(process)) {
      
      if ("collection_id" %in% names(process)) {
        arguments[[prior.name]] = process
      } else if ("process_id" %in% names(process)){
        arguments[[prior.name]] = process
      } else {
        stop("Chain corrupted. prior elemente is neither a process or a collection")
      }
    }
  }
  additionalParameter = list(...)
  
  res$process_id=process_id
  res$args = append(arguments,additionalParameter)
  
  return(res)
  
}


#' A collection object
#'
#' creates a list representation of a collection object
#' @param collection_id the id of the product
#' @param id_name the name of the identifier for this collection, default "collection_id"
#' @return a list represenation for a collection / product
#' @export
collection = function(collection_id,id_name = "collection_id") {
  result = list(collection_id)
  names(result) = id_name
  return(result)
}
