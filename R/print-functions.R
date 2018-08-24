 
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
  
  d = data.frame(Parameter = names(x$parameters), 
                 Description=sapply(x$parameters,function(arg) arg$description),
                 Required=sapply(x$parameters,function(arg) arg$required),
                 stringsAsFactors = FALSE)
  rownames(d) <- NULL
  cat(paste(title,summary,description,result,"",sep="\n"))
  cat("\n")
  print(d)
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
print.openeo_product = function(x, ...) {
  id = paste(x$product_id)
  description = paste("Description:\t\t",x$description,sep="")
  source = paste("Source:\t\t\t",x$source,sep="")
  extent = paste("Spatial extent:\t\t",paste(x$spatial_extent,sep="",collapse=", "),sep="")
  crs = paste("SRS:\t\t\t",x$crs@projargs,sep="")
  time = paste("Temporal extent:\t",paste(sapply(x$temporal_extent,function(obj) {format(obj,format="%Y-%m-%dT%H:%M:%SZ")}),collapse="/"),sep="")
  
  cat(paste(id,description,source,extent,crs,time,"Bands:\n",sep="\n"))
  print(bandlist_to_df(x$bands),row.names=FALSE)
}

bandlist_to_df = function(bands) {
  u=sapply(bands,function(band){
    c(band)
  })
  u = as.data.frame(t(u),stringsAsFactors = FALSE)
  
  for (j in 1:ncol(u)) {
    for (i in 1:nrow(u)) {
      if (length(unlist(u[i,j])) == 0 ) {
        u[i,j] = NA
      }
    }
    if (length(unlist(u[,j])) == nrow(u)) {
      u[,j] = unlist(u[,j])
    }
  }
  
  return(u)
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
  cat(toJSON(x$process_graph,pretty = TRUE,auto_unbox = TRUE))
  cat("\n")
}