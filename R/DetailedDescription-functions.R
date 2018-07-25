# describeProcess will return a list which is labeled as class "ProcessInfo"

#' @export
print.ProcessInfo <- function(x) {
  title = paste("Process:\t",x$process_id,sep="")
  description = paste("Description:\t",x$description,sep="")
  
  d = data.frame(Parameter = names(x$args), Description=sapply(x$args,function(arg) arg$description),stringsAsFactors = FALSE)
  rownames(d) <- NULL
  cat(paste(title,description,"",sep="\n"))
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