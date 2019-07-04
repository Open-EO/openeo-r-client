 
#' Prints a User object
#' 
#' A nice visualization for the user account information obtained by /me
#' 
#' @param x an User object that can be retrieved at \link{describe_account}
#' @param ... additional parameters (not used)
#' 
#' @export
print.User = function(x, ...) {
  cat(paste("ID:","\t",x$user_id,"\n",sep=""))
  cat(paste("Name:","\t",x$user_name,"\n",sep=""))
  cat(paste("Budget:","\t",x$budget,"\n",sep=""))
  cat("File Storage:\n")
  cat(paste("   Quota:","\t",x$storage$quota, " Bytes","\n",sep=""))
  cat(paste("   Free:","\t",x$storage$free, " Bytes","\n",sep=""))
}


#' Pretty print an openeo process
#' 
#' Print function to visualize relevant information about an openeo process
#' 
#' @param x process info that is received on \link{list_processes} and \link{describe_process}
#' @param ... additional parameters (not used)
#' 
#' @export
print.ProcessInfo <- function(x, ...) {
  title = paste("Process:\t",x$id,sep="")
  summary = paste("Summary:\t",x$summary,sep="")
  description = paste("Description:\t",x$description,sep="")
  result = paste("Returns:\t",x$returns$description,sep="")
  
  if (length(x$parameters) > 0) {
    d = data.frame(Parameter = names(x$parameters), 
                   Description=sapply(x$parameters,function(arg) arg$description),
                   Required=sapply(x$parameters,function(arg) {
                     if(!is.null(arg$required)) {
                      return(arg$required)
                     } else {
                       return(FALSE)
                     }
                   }),
                   stringsAsFactors = FALSE)
    rownames(d) <- NULL
    cat(paste(title,summary,description,result,"",sep="\n"))
    cat("\n")
    print(d)
  }
}

#' @export
print.ServiceType = function(x,...) {
  service_type = paste(x$service,"\n")
  parameters = paste("Parameters","(used on service creation)\n")
  
  d1 = data.frame(Parameter = names(x$parameters), 
                 Description=sapply(x$parameters,function(arg) arg$description),
                 Type=sapply(x$parameters,function(arg) arg$type),
                 default=sapply(x$parameters,function(arg) arg$default),
                 example=sapply(x$parameters,function(arg) {
                   enum = arg$enum
                   return(paste("[",paste(enum,sep="", collapse=", "),"]",sep=""))
                 }),
                 stringsAsFactors = FALSE)
  row.names(d1) = NULL
  
  attributes = paste("Attributes","(used during request)\n")
  
  d2 = data.frame(Attributes = names(x$attributes), 
                  Description=sapply(x$attributes,function(arg) arg$description),
                  example=sapply(x$attributes,function(arg) {
                    example = arg$example
                    return(paste("[",paste(example,sep="", collapse=", "),"]",sep=""))
                  }),
                  stringsAsFactors = FALSE)
  row.names(d2) = NULL
  
  cat(service_type,parameters)
  print(d1)
  cat(attributes)
  print(d2)
}

#' @export
print.CollectionInfo = function(x, ...) {
  id = paste(x$id)
  if (is.null(x$title)) x$title = "---"
  title = paste("Title:\t\t\t\t",x$title,sep="")
  
  description = paste("Description:\t\t\t",x$description,sep="")
  
  if (is.null(x$provider)) x$provider = list(list(name="---"))
  source = paste("Source:\t\t\t\t",paste(sapply(x$provider, function(p) {
    p$name
  }),sep="",collapse = ", "),sep="")
  
  if (!is.null(x$properties)) {
    if (is.null(x$properties$`eo:platform`))x$properties$`eo:platform` = "---"
    platform = paste("Platform:\t\t\t",x$properties$`eo:platform`,sep="")
    if (is.null(x$properties$`eo:constellation`))x$properties$`eo:constellation` = "---"
    constellation = paste("Constellation:\t\t\t",x$properties$`eo:constellation`,sep="")
    if (is.null(x$properties$`eo:instrument`))x$properties$`eo:instrument` = "---"
    instrument = paste("Instrument:\t\t\t",x$properties$`eo:instrument`,sep="")
    
    crs = paste("Data SRS (EPSG-code):\t\t",x$properties$`eo:epsg`,sep="")
  }
  
  
  
  spatial.extent = paste("(",x$extent$spatial[1],", ",x$extent$spatial[2],"), (",x$extent$spatial[3],", ", x$extent$spatial[4],")",sep="")
  extent = paste("Spatial extent (lon,lat):\t",spatial.extent,sep="")
  
  time = paste("Temporal extent:\t\t",paste(sapply(x$extent$temporal,function(obj) {
    if (is.null(obj) || is.na(obj) || length(obj) == 0) return(NA)
    else return(format(as_datetime(obj),format="%Y-%m-%dT%H:%M:%SZ"))
  }),collapse="/"),sep="")
  
  cat(c(id,title,description,source,
            platform,constellation,instrument,
            extent,crs,time),sep="\n")
  
  if (!is.null(x$properties$`eo:bands`)) {
    cat("Bands:\n")
    print(as.data.frame(as_tibble(x$properties$`eo:bands`)))
  }
  
}

#' @export
print.JobInfo = function(x,...) {
  id = paste("Job ID:\t\t", x$id, "\n",sep="")
  if (is.null(x$title)) x$title = "---"
  title = paste("Title:\t\t",x$title,"\n",sep="")
  if (is.null(x$description)) x$description = "---"
  description = paste("Description:\t",x$description,"\n",sep="")
  status = paste("Status:\t\t",x$status,"\n",sep="")
  submitted = paste("Submitted:\t",x$submitted,"\n",sep="")
  updated = paste("Updated:\t",x$updated,"\n",sep="")
  
  if (is.null(x$progress)) x$progress = "---"
  progress = paste("Progress:\t",x$progress,"\n",sep="")
  
  if (is.null(x$error)) x$error$message = "---"
  error = paste("Error:\t\t",x$error$message,"\n",sep="")
    
  if (is.null(x$plan)) x$plan = "---"
  plan = paste("Plan:\t\t",x$plan,"\n",sep="")
  costs = paste("Costs:\t\t",x$costs,"\n",sep="")
  if (is.null(x$budget)) x$budget = "---"
  budget = paste("Budget:\t\t",x$budget,"\n",sep="")
  
  cat(id,title,description,status,submitted,updated,
      progress, error,
      plan,costs,budget,sep = "")
  
  output = "Output:"
  cat(output)
  if (is.null(x$output)) {
    cat("\t\t---\n")
  } 
  else {
    cat("\n")
    cat(toJSON(x$output,pretty=TRUE,auto_unbox = TRUE))
    cat("\n")
  }
  
  process_graph = "Process graph:\n"
  cat(process_graph)
  print(x$process_graph)
}

#' @export
print.ServiceInfo = function(x,...) {

  id = paste("ID:\t\t",x$id,"\n",sep="")
  
  type = paste("Type:\t\t",x$type,"\n",sep="")
  
  enabled = paste("Enabled:\t",x$enabled,"\n",sep="")
  
  submitted = paste("Submitted:\t",x$submitted,"\n",sep="")
  
  if (is.null(x$title) || is.na(x$title))x$title = "---"
  title = paste("Title:\t\t",x$title,"\n",sep="")
  
  if (is.null(x$description) || is.na(x$description))x$description = "---"
  description = paste("Description:\t",x$description,"\n",sep="")
  
  if (is.na(x$url)) x$url = "---"
  url = paste("Endpoint:\t",x$url,"\n",sep="")
  
  plan = paste("Plan:\t\t",x$plan,"\n",sep="")
  
  costs = paste("Costs:\t\t",x$costs,"\n",sep="")
  
  if (length(x$budget) == 0 || is.na(x$budget)) x$budget ="---"
  budget = paste("Budget:\t\t",x$budget,"\n",sep="")
  
  cat(id,type,enabled,title,submitted,description,url,plan,costs,budget,sep="")
  
  #parameters, attributes, process_graph
  if (is.null(x$parameters)) {
    x$parameters = "---"
    cat(paste("Parameters:\t",x$parameters,"\n",sep=""))
  } else {
    x$parameters = toJSON(x$parameters,pretty=TRUE,auto_unbox = TRUE)
    cat(paste("Parameters:\n",x$parameters,"\n",sep=""))
  } 
  
  if (is.null(x$attributes)) {
    x$attributes = "---"
    cat(paste("Attributes:\t",x$attributes,"\n",sep=""))
  } else {
    x$attributes = toJSON(x$attributes,pretty=TRUE,auto_unbox = TRUE)
    cat(paste("Attributes:\n",x$attributes,"\n",sep=""))
  }
  
  if (is.null(x$process_graph)) {
    x$process_graph = "---"
    cat(paste("Process graph:\t",x$process_graph,"\n",sep=""))
  } else {
    x$process_graph = toJSON(x$process_graph,pretty=TRUE,auto_unbox = TRUE)
    cat(paste("Process graph:\n",x$process_graph,"\n",sep=""))
  }
}

#' @export
print.JobCostsEstimation = function(x,...){
  header = "Job costs estimation\n"
  line =   "====================\n"
  costs = paste("Costs: \t\t\t\t",x$costs,"\n",sep="")
  duration = paste("Duration: \t\t\t",x$duration,"\n", sep="")
  downloads = paste("Downloads for owner included: \t")
  yesno = if (x$downloads_included) "yes" else "no"
  
  cat(header,line,costs,duration,downloads,yesno,sep="")
}

#' @export
print.CollectionList = function(x, ...) {
  print(dplyr::select(tibble::as_tibble(x),
                      id,
                      title,
                      description))
}

#' @export
print.Graph = function(x, ...) {
  print(graphToJSON(x))
}

#' @export
print.Json_Graph = function(x, ...) {
  print(toJSON(x,auto_unbox = TRUE,force=TRUE,pretty=TRUE))
}

#' @export
print.ProcessGraphInfo = function(x, ...) {
  id = paste("Job ID:\t\t", x$id,sep="")
  if (is.null(x$title)) x$title = "---"
  title = paste("Title:\t\t",x$title,sep="")
  if (is.null(x$description)) x$description = "---"
  description = paste("Description:\t",x$description,sep="")
  graph = "Process graph:"
  cat(id,title,description,graph,sep = "\n")
  print(x$process_graph)
}

#' @export
print.OpenEOCapabilities = function(x, ...) {
  capabilities = x

  title = capabilities$title
  backend_version = capabilities$backend_version
  description=capabilities$description
  
  version = capabilities$api_version
  endpoints = capabilities$endpoints
  billing = capabilities$billing #not used right now
  
  cat(paste0("Back-end:\t\t",title),
      paste0("Back-end version: \t",backend_version),
      paste0("Description:\t\t",description),
      paste0("API-version:\t\t",version),
      sep = "\n") 
       
  
  server_offering = tibble(path=character(),method=character())
  for (i in 1:length(endpoints)) {
    entry = endpoints[[i]]
    path = entry$path
    
    for (j in 1:length(entry$method)) {
      method = entry$method[[j]]
      
      server_offering = add_row(server_offering,path=path,method=method)
    }
  }
  print(server_offering)
}
