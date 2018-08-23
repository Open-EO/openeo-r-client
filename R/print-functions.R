 
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
print.openeo_product = function(x, ...) {
  id = paste(x$product_id)
  description = paste("Description:\t\t",x$description,sep="")
  source = paste("Source:\t\t\t",x$source,sep="")
  extent = paste("Spatial extent:\t\t",paste(x$extent,sep="",collapse=", "),sep="")
  crs = paste("SRS:\t\t\t",x$crs@projargs,sep="")
  time = paste("Temporal extent:\t", paste(x$time$from,x$time$to,sep=", "),sep="")
  
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