
#' @export
callback = function(con,callback_arg) {
  cb_parameters=callback_arg$getCallbackParameters() # all the possible data exports offered by the argument
  
  processes = con$listProcesses()
  # json processes -> process objects
  
  names(processes) = sapply(processes,function(p)p$id)

  
  plist = lapply(processes,processFromJson)
  
  cb_graph = Graph$new(plist,cb_parameters)
  
  callback_arg$setValue(cb_graph)
  return(cb_graph)
}