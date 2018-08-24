#' Create a process object
#'
#' defines a process with arguments
#' @param process A process that might be chained with this process
#' @param process_id ID of the process offered by the connected openEO backend
#' @param prior.name The name of the prior process / collection, default "imagery"
#' @param ... named arguments that are passed to the process description
#' @export
process = function(process=NULL, process_id, prior.name="imagery", ...) {
  # type check "process" either collection or process
  process_body = list()
  if (!missing(process) && !is.null(process)) {
    if (is.list(process)) {
      

      if (attr(process,"type") %in% c("process","udf","collection")){
        process_body[[prior.name]] = process
      } else {
        stop("Chain corrupted. Prior element is neither a process nor a collection")
      }
    }
  }
  
  process_body$process_id=process_id
  process_body = append(process_body,list(...))
  attr(process_body,"type") <- "process"
  
  return(process_body)
  
}


#' A collection object
#'
#' creates a list representation of a collection object
#' @param product_id the id of the product
#' @param id_name the name of the identifier for this collection, default "product_id"
#' @return a list represenation for a collection / product
#' @export
collection = function(product_id,id_name = "product_id") {
  result = list(product_id)
  names(result) = id_name
  
  attr(result, "type") <- "collection"
  return(result)
}
