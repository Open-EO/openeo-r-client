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