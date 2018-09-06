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
#' Creates the get_collection process to load data into the process graph.
#' 
#' @param name the id of the product
#' @param ... additional parameters that are passed on to get_collection process
#' @return a list represenation for the get_data process
#' @export
collection = function(name, ...) {
  #TODO change to get_collection
  return(process(process_id = "get_collection", name=name, ...))
}
