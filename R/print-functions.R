 
#' Prints a User object
#' 
#' A nice visualization for the user account information obtained by /me
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
#' @export
print.ProcessInfo <- function(x, ...) {
  title = paste("Process:\t",x$name,sep="")
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
  id = paste(x$name)
  if (is.null(x$title)) x$title = "---"
  title = paste("Title:\t\t\t\t",x$title,sep="")
  
  description = paste("Description:\t\t\t",x$description,sep="")
  
  if (is.null(x$provider)) x$provider = list(list(name="---"))
  source = paste("Source:\t\t\t\t",paste(sapply(x$provider, function(p) {
    p$name
  }),sep="",collapse = ", "),sep="")
  
  if (is.null(x$`eo:platform`))x$`eo:platform` = "---"
  platform = paste("Platform:\t\t\t",x$`eo:platform`,sep="")
  if (is.null(x$`eo:constellation`))x$`eo:constellation` = "---"
  constellation = paste("Constellation:\t\t\t",x$`eo:constellation`,sep="")
  if (is.null(x$`eo:instrument`))x$`eo:instrument` = "---"
  instrument = paste("Instrument:\t\t\t",x$`eo:instrument`,sep="")
  
  
  spatial.extent = paste("(",x$extent$spatial[1],", ",x$extent$spatial[2],"), (",x$extent$spatial[3],", ", x$extent$spatial[4],")",sep="")
  extent = paste("Spatial extent (lon,lat):\t",spatial.extent,sep="")
  crs = paste("Data SRS (EPSG-code):\t\t",x$`eo:epsg`,sep="")
  time = paste("Temporal extent:\t\t",paste(sapply(x$extent$temporal,function(obj) {
    if (is.null(obj) || is.na(obj) || length(obj) == 0) return(NA)
    else return(format(as_datetime(obj),format="%Y-%m-%dT%H:%M:%SZ"))
  }),collapse="/"),sep="")
  
  cat(c(id,title,description,source,
            platform,constellation,instrument,
            extent,crs,time),sep="\n")
  
  if (!is.null(x$`eo:bands`)) {
    cat("Bands:\n")
    print(as.data.frame(as_tibble(x$`eo:bands`)))
  }
  
}

#' @export
print.JobInfo = function(x,...) {
  job_id = paste("Job ID:\t\t", x$job_id, "\n",sep="")
  if (is.null(x$title)) x$title = "---"
  title = paste("Title:\t\t",x$title,"\n",sep="")
  if (is.null(x$description)) x$description = "---"
  description = paste("Description:\t",x$description,"\n",sep="")
  status = paste("Status:\t\t",x$status,"\n",sep="")
  submitted = paste("Submitted:\t",x$submitted,"\n",sep="")
  updated = paste("Updated:\t",x$updated,"\n",sep="")
  if (is.null(x$plan)) x$plan = "---"
  plan = paste("Plan:\t\t",x$plan,"\n",sep="")
  costs = paste("Costs:\t\t",x$costs,"\n",sep="")
  if (is.null(x$budget)) x$budget = "---"
  budget = paste("Budget:\t\t",x$budget,"\n",sep="")
  
  cat(job_id,title,description,status,submitted,updated,plan,costs,budget,sep = "")
  
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

  service_id = paste("ID:\t\t",x$service_id,"\n",sep="")
  
  type = paste("Type:\t\t",x$type,"\n",sep="")
  
  enabled = paste("Enabled:\t",x$enabled,"\n",sep="")
  
  submitted = paste("Submitted:\t",x$submitted,"\n",sep="")
  
  if (is.na(x$title) || is.na(x$title))x$title = "---"
  title = paste("Title:\t\t",x$title,"\n",sep="")
  
  if (is.null(x$description) || is.na(x$description))x$description = "---"
  description = paste("Description:\t",x$description,"\n",sep="")
  
  if (is.na(x$url)) x$url = "---"
  url = paste("Endpoint:\t",x$url,"\n",sep="")
  
  plan = paste("Plan:\t\t",x$plan,"\n",sep="")
  
  costs = paste("Costs:\t\t",x$costs,"\n",sep="")
  
  if (is.na(x$budget)) x$budget ="---"
  budget = paste("Budget:\t\t",x$budget,"\n",sep="")
  
  cat(service_id,type,enabled,title,submitted,description,url,plan,costs,budget,sep="")
  
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
  print(as_tibble(x) %>% select(name,title,description))
}

#' @export
print.process = function(x, ...) {
  print(taskToJSON(x))
}

#' @export
print.ProcessGraphInfo = function(x, ...) {
  job_id = paste("Job ID:\t\t", x$process_graph_id,sep="")
  if (is.null(x$title)) x$title = "---"
  title = paste("Title:\t\t",x$title,sep="")
  if (is.null(x$description)) x$description = "---"
  description = paste("Description:\t",x$description,sep="")
  graph = "Process graph:"
  cat(job_id,title,description,graph,sep = "\n")
  print(x$process_graph)
}