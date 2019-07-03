
#' @export
callback = function(con,process, parameter=NULL) {
  if (!"Process" %in% class(process)) stop("Parameter process is no process for a openeo graph")
  
  # iterate over parameters and state callback possibilities
  callbacksParameterNames = unname(unlist(sapply(process$parameters, function(param) {
    if ("callback" %in% class(param)) return(param$getName())
  })))
  
  if (!is.null(callbacksParameterNames)) {
    # if parameter is not null check if it exists and is callback
    if (!is.null(parameter) && is.character(parameter) && parameter %in% callbacksParameterNames) {
      callback_arg = process$getParameter(name=parameter)
      
      cb_parameters=callback_arg$getCallbackParameters() # all the possible data exports offered by the argument
      
      processes = list_processes(con)
      # json processes -> process objects
      
      names(processes) = sapply(processes,function(p)p$id)
      
      
      plist = lapply(processes,processFromJson)
      
      cb_graph = Graph$new(plist,cb_parameters)
      
      callback_arg$setValue(cb_graph)
      return(cb_graph)
    } else {
      cat("Parameter that expect a callback: ")
      cat(paste(callbacksParameterNames,collapse=", "))
    }
    
    # get the callback values that are available for this callback (data that will be used in the graph like load_collection in the main graph)
  } else {
    message("No callbacks found in the parameters of the stated process")
  }
  invisible(callbacksParameterNames)

}