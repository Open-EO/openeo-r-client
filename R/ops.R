# Definitions for operators to make it easier to create arithmetic callbacks

# it works on process nodes and callback-value

# TODO callback-value as array needs to store process nodes for subsetting to avoid having the same 
# requests multiple times
# TODO graph needs to be available in the ProcessNode

#' @export
`[.callback-value` <- function(x,i,...,drop=TRUE) {
  # check x for being an array
  if (! x$getSchema()$type == "array") stop("Non-array callback value cannot be addressed by index. Check if the callback requires a binary operator")
  graph = x$getProcess()$getGraph()
  graph$array_element(data = x, index = i)
}

#' @export
`+.ProcessNode` <- function(e1,e2) {
  # GEE uses only sum, this needs to be accounted... usually its "add" or "plus"
  # both nodes?
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
    graph$sum(data = c(e1,e2))
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
    graph$sum(data = c(e1,e2))
  } else {
    # should not happen
  }
}

#' @export
`-.ProcessNode` <- function(e1,e2) {
  
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
    graph$subtract(data = c(e1,e2))
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
    graph$subtract(data = c(e1,e2))
  } else {
    # should not happen
  }
}

#' @export
`*.ProcessNode` <- function(e1,e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
    graph$multiply(data = c(e1,e2))
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
    graph$multiply(data = c(e1,e2))
  } else {
    # should not happen
  }
}

#' @export
`/.ProcessNode` <- function(e1,e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
    graph$divide(data = c(e1,e2))
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
    graph$divide(data = c(e1,e2))
  } else {
    # should not happen
  }
}