# Definitions for operators to make it easier to create arithmetic callbacks

# it works on process nodes and callback-value

# TODO callback-value as array needs to store process nodes for subsetting to avoid having the same 
# requests multiple times
# TODO graph needs to be available in the ProcessNode

# 1. Group
#' @export
`abs.ProcessNode` <- function(x) {
  graph = x$getGraph()

  FUN = "absolute"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`sign.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "sgn"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`sqrt.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "sqrt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`trunc.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "int"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`floor.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "floor"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`ceiling.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "ceil"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`round.ProcessNode` <- function(x,digits=0) {
  graph = x$getGraph()
  
  FUN = "round"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x,p=digits) 
}

#' @export
`exp.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "exp"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`log.ProcessNode` <- function(x,base=exp(1)) {
  graph = x$getGraph()
  
  if (base==exp(1)) {
    FUN = "ln"
  } else {
    FUN = "log"
  }
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x,base=base) 
}

#' @export
`log10.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "log"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x,base=10) 
}

# trigonometric functions ----
#' @export
`cos.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "cos"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`sin.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "sin"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`tan.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "tan"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`cosh.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "cosh"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`sinh.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "sinh"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`tanh.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "tanh"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`acos.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "arccos"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`asin.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "arcsin"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`atan.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "arctan"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`acosh.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "arccosh"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`asinh.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "arcsinh"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`atanh.ProcessNode` <- function(x) {
  graph = x$getGraph()
  
  FUN = "arctanh"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`atan2.ProcessNode` <- function(y,x) {
  graph = x$getGraph()
  
  FUN = "arctan2"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](y=y,x=x) 
}

# cummulative ----
# cumsum ====
.cumsum <- function(x) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "cumsum"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}
#' @export
`cumsum.ProcessNode` <- .cumsum

#' @export
`cumsum.callback-value` <- .cumsum

# cummin ====
#' @export
.cummin <- function(x) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "cummin"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

`cummin.ProcessNode` <- .cummin

#' @export
`cummin.callback-value` <- .cummin

# cummax ====
.cummax <- function(x) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "cummax"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`cummax.ProcessNode` <- .cummax

#' @export
`cummax.callback-value` <- .cummax

# cumprod ====
.cumprod <- function(x) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "cumproduct"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=x) 
}

#' @export
`cumprod.ProcessNode` <- .cumprod

#' @export
`cumprod.callback-value` <- .cumprod

# 2. Group
# [ (subset) ====
#' @export
`[.callback-value` <- function(x,i,...,drop=TRUE) {
  # TODO think if i is a vector?
  
  # check x for being an array
  if (! x$getSchema()$type == "array") stop("Non-array callback value cannot be addressed by index. Check if the callback requires a binary operator")
  graph = x$getProcess()$getGraph()
  
  FUN = "array_element"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data=x,index=i-1) # do index shift because javascript / JSON addresses an element of an array from 0 to n-1
}

# mathematical operators ----
# + (plus / add) ====

.plus = function(e1,e2){
  .genericBinaryFunction(e1,e2,"sum")
}

#' @export
`+.ProcessNode` <- .plus

#' @export
`+.callback-value` <- .plus

# - (minus / subtract) ====
.minus = function(e1,e2) {
  # v0.4.2 -> subtract of array
  # v0.5 -> subtract of two values
  if (missing(e2)) {
    return(-1 * e1)
  }
  
  .genericBinaryFunction(e1,e2,"subtract")
}

#' @export
`-.ProcessNode` <- .minus

#' @export
`-.callback-value` <- .minus

# * (multiply) ====
.multiply = function(e1,e2) {
  # v0.4.2 -> multiply or product of array
  # v0.5 -> multiply of two values, product of an array of values
  
  if ("multiply" %in% names(graph)) {
    FUN = "multiply"
  } else if ("product" %in% names(graph)) {
    FUN = "product"
  } else {
    stop("Neither 'multiply' nor 'product' are available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'.")
  }
  
  .genericBinaryFunction(e1,e2,FUN)
  
}

#' @export
`*.ProcessNode` <- .multiply

#' @export
`*.callback-value` <- .multiply

# / (divide) ====
.divide = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"divide")
}

#' @export
`/.ProcessNode` <- .divide

#' @export
`/.callback-value` <- .divide

# ^ (power) ====
.power = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"power")
}

#' @export
`^.ProcessNode` <- .power

#' @export
`^.callback-value` <- .power

# %% (modulo) ====
#' @export
`%%.ProcessNode` <- function(e1,e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "mod"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](e1,e2) 
  
}

# logical operators ----

# ! (not) ====
#' @export
`!.ProcessNode` <- function(e1,e2) {
  # e2 missing
  graph = e1$getGraph()

  FUN = "not"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](e1) 
}

# & (and) ====
#' @export
`&.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  # v0.5 -> and only has two arguments
  # TODO think to overwrite 'all'
  
  FUN = "and"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}

# | (or) ====
#' @export
`|.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  # v0.5 -> or only has two arguments, now it takes an array
  # TODO think to overwrite 'any'

  FUN = "or"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}

# xor ====
#' @export
`xor.ProcessNode` <- function(x,y) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else if ("ProcessNode" %in% class(y)) {
    graph = y$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "xor"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}

# == (equals) ====
#' @export
`==.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "eq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}

# != (unequal) ====
#' @export
`!=.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "neq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
} 

# < (smaller than) ====
#' @export
`<.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "lt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}

# <= (smaller than equals) ====
#' @export
`<=.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "lte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}

# >= (greater than equals) ====
#' @export
`>=.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "gte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}

# > (greater than) ====
#' @export
`>.ProcessNode` <- function(e1, e2) {
  if ("ProcessNode" %in% class(e1)) {
    graph = e1$getGraph()
  } else if ("ProcessNode" %in% class(e2)) {
    graph = e2$getGraph()
  } else {
    # should not happen
  }
  
  FUN = "gt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}

# 3. group
# summary operators ----
# sum ====
.sum <- function(..., na.rm=FALSE) {
  elems = list(...)
  
  if ("ProcessNode" %in% class(elems[[1]])) {
    graph = elems[[1]]$getGraph()
  } else {
    graph = elems[[1]]$getProcess()$getGraph()
  }
  
  FUN = "sum"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = elems) 
}

#' @export
`sum.ProcessNode` <- .sum

#' @export
`sum.callback-value` <- .sum

# prod ====
.prod <- function(..., na.rm=TRUE) {
  
  elems = list(...)
  
  if ("ProcessNode" %in% class(elems[[1]])) {
    graph = elems[[1]]$getGraph()
  } else {
    graph = elems[[1]]$getProcess()$getGraph()
  }
  
  FUN = "product"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = elems)
}
#' @export
`prod.ProcessNode` <- .prod

#' @export
`prod.callback-value` <- .prod

# min ====
.min <- function(..., na.rm=TRUE) {
  
  elems = list(...)
  
  if ("ProcessNode" %in% class(elems[[1]])) {
    graph = elems[[1]]$getGraph()
  } else {
    graph = elems[[1]]$getProcess()$getGraph()
  }
  
  FUN = "min"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = elems)
}
#' @export
`min.ProcessNode` <- .min

#' @export
`min.callback-value` <- .min

# max ====
.max <- function(..., na.rm=TRUE) {
  
  elems = list(...)
  
  if ("ProcessNode" %in% class(elems[[1]])) {
    graph = elems[[1]]$getGraph()
  } else {
    graph = elems[[1]]$getProcess()$getGraph()
  }
  
  FUN = "max"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = elems)
}
#' @export
`max.ProcessNode` <- .max

#' @export
`max.callback-value` <- .max

# range ====
.range <- function(..., na.rm=TRUE) {
  
  elems = list(...)
  
  if ("ProcessNode" %in% class(elems[[1]])) {
    graph = elems[[1]]$getGraph()
  } else {
    graph = elems[[1]]$getProcess()$getGraph()
  }
  
  FUN = "extrema"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = elems)
}

#' @export
`range.ProcessNode` <- .range

#' @export
`range.callback-value` <- .range

# mean ====
.mean <- function(x, na.rm=FALSE,...) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "mean"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = x)
}

#' @export
`mean.ProcessNode` <- .mean

#' @export
`mean.callback-value` <- .mean

# median ====
#' @export
.median <- function(x, na.rm=FALSE,...) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "median"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = x)
}
`median.ProcessNode`<- .median

#' @export
`median.callback-value`<- .median

# sd ====
.sd <- function(x, na.rm=FALSE) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "sd"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = x)
}
#' @export
`sd.ProcessNode`  <- .sd
#' @export
`sd.callback-value`  <- .sd

# var ====
.var <- function(x, na.rm=FALSE) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "variance"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = x)
}

#' @export
`var.ProcessNode` <- .var
#' @export
`var.callback-value` <- .var

# quantile ====
.quantile <- function(x, probs, na.rm=FALSE) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  
  FUN = "quantiles"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = x,probabilites = probs)
}
#' @export
`quantile.ProcessNode` <- .quantile
#' @export
`quantile.callback-value` <- .quantile


# utility functions ====
.getGraph = function(e1,e2) {
  if (any(c("callback-value","ProcessNode") %in% class(e1))) {
    if ("ProcessNode" %in% class(e1)) {
      return(e1$getGraph())
    } else { # callback-value
      return(e1$getProcess()$getGraph())
    }
    
    
  } else if (any(c("callback-value","ProcessNode") %in% class(e2))) {
    if ("ProcessNode" %in% class(e2)) {
      return(e2$getGraph())
    } else { # callback-value
      return(e2$getProcess()$getGraph())
    } 
    
  } else {
    # should not happen
  }
}

.genericBinaryFunction = function(e1,e2,FUN) {
  graph = .getGraph(e1,e2)
  
  # if callback-value, then check if array or single value
  if ("callback-value" %in% class(e1)) {
    
    if (length(e1$getSchema()$type) > 0 && e1$getSchema()$type == "array") {
      e1 = e1[1]
      warning("Using first index of the input parameter else do explicit subset.")
    }
  }
  
  if ("callback-value" %in% class(e2)) {
    if (length(e2$getSchema()$type) > 0 && e2$getSchema()$type == "array") {
      e2 = e2[1]
      warning("Using first index of the input parameter else do explicit subset.")
    }
  }
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data = c(e1,e2)) 
}