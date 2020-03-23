# Definitions for operators to make it easier to create arithmetic callbacks

# it works on process nodes and callback-value


# 1. Group

.abs = function(x) {
  .genericUnaryFunction(x,"absolute")
}

#' @export
`abs.ProcessNode` <- .abs

#' @export
`abs.callback-value` <- .abs


.sign = function(x) {
  .genericUnaryFunction(x,"sgn")
}
#' @export
`sign.ProcessNode` <- .sign
#' @export
`sign.callback-value` <- .sign

.sqrt <- function(x) {
  .genericUnaryFunction(x,"sqrt")
}
#' @export
`sqrt.ProcessNode` <- .sqrt

#' @export
`sqrt.callback-value` <- .sqrt

.trunc = function(x) {
  .genericUnaryFunction(x,"int")
}
#' @export
`trunc.ProcessNode` <- .trunc

#' @export
`trunc.callback-value` <- .trunc

.floor = function(x) {
  .genericUnaryFunction(x,"floor")
}
#' @export
`floor.ProcessNode` <- .floor

#' @export
`floor.callback-value` <- .floor

.ceiling = function(x) {
  .genericUnaryFunction(x,"ceil")
}
#' @export
`ceiling.ProcessNode` <- .ceiling

#' @export
`ceiling.callback-value` <- .ceiling

.round = function(x,digits=0) {
  graph = .getGraph(x)
  
  FUN = "round"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  graph[[FUN]](x=x,p=digits) 
}
#' @export
`round.ProcessNode` <- .round

#' @export
`round.callback-value` <- .round


.exp = function(x) {
  .genericUnaryFunction(x,"exp")
}

#' @export
`exp.ProcessNode` <- .exp

#' @export
`exp.callback-value` <- .exp


.log = function(x,base=exp(1)) {
  graph = .getGraph(x)
  
  if (base==exp(1)) {
    FUN = "ln"
  } else {
    FUN = "log"
  }
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  
  if (FUN == "log") {
    graph[[FUN]](x=x,base=base) 
  } else {
    graph[[FUN]](x=x) 
  }
  
}

#' @export
`log.ProcessNode` <- .log

#' @export
`log.callback-value` <- .log


.log10 = function(x) {
  .log(x,10)
}
#' @export
`log10.ProcessNode` <- .log10

#' @export
`log10.callback-value` <- .log10

# trigonometric functions ----
.cos = function(x) {
  .genericUnaryFunction(x,"cos")
}

#' @export
`cos.ProcessNode` <- .cos

#' @export
`cos.callback-value` <- .cos


.sin = function(x) {
  .genericUnaryFunction(x,"sin")
}
#' @export
`sin.ProcessNode` <- .sin

#' @export
`sin.callback-value` <- .sin


.tan = function(x) {
  .genericUnaryFunction(x,"tan")
}
#' @export
`tan.ProcessNode` <- .tan

#' @export
`tan.callback-value` <- .tan


.cosh = function(x) {
  .genericUnaryFunction(x,"cosh")
}

#' @export
`cosh.ProcessNode` <- .cosh

#' @export
`cosh.callback-value` <- .cosh


.sinh = function(x) {
  .genericUnaryFunction(x,"sinh")
}

#' @export
`sinh.ProcessNode` <- .sinh

#' @export
`sinh.callback-value` <- .sinh


.tanh = function(x) {
  .genericUnaryFunction(x,"tanh")
}

#' @export
`tanh.ProcessNode` <- .tanh

#' @export
`tanh.callback-value` <- .tanh


.acos = function(x) {
  .genericUnaryFunction(x,"arccos")
}

#' @export
`acos.ProcessNode` <- .acos

#' @export
`acos.callback-value` <- .acos


.asin = function(x) {
  .genericUnaryFunction(x,"arcsin")
}

#' @export
`asin.ProcessNode` <- .asin

#' @export
`asin.callback-value` <- .asin


.atan = function(x) {
  .genericUnaryFunction(x,"arctan")
}

#' @export
`atan.ProcessNode` <- .atan

#' @export
`atan.callback-value` <- .atan


.acosh = function(x) {
  .genericUnaryFunction(x,"arccosh")
}
#' @export
`acosh.ProcessNode` <- .acosh

#' @export
`acosh.callback-value` <- .acosh


.asinh = function(x) {
  .genericUnaryFunction(x,"arcsinh")
}
#' @export
`asinh.ProcessNode` <- .asinh

#' @export
`asinh.callback-value` <- .asinh


.atanh = function(x) {
  .genericUnaryFunction(x,"arcsinh")
}
#' @export
`atanh.ProcessNode` <- .atanh

#' @export
`atanh.callback-value` <- .atanh


# internal function does not work
# @export
# `atan2` = function(y,x) {
#   UseMethod("atan2")
# }
# 
# .atan2 = function(y,x) {
#     graph = .getGraph(y,x)
# 
#     FUN = "arctan2"
#     if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
#     graph[[FUN]](y=y,x=x)
# }
# @export
# `atan2.ProcessNode` <- .atan2
# 
# @export
# `atan2.callback-value` <- .atan2

# cummulative ----
.cumsum <- function(x) {
  .genericUnaryFunction(x,"cumsum")
}
#' @export
`cumsum.ProcessNode` <- .cumsum

#' @export
`cumsum.callback-value` <- .cumsum


.cummin <- function(x) {
  .genericUnaryFunction(x,"cummin")
}

#' @export
`cummin.ProcessNode` <- .cummin

#' @export
`cummin.callback-value` <- .cummin


.cummax <- function(x) {
  .genericUnaryFunction(x,"cummax")
}

#' @export
`cummax.ProcessNode` <- .cummax

#' @export
`cummax.callback-value` <- .cummax


.cumprod <- function(x) {
  .genericUnaryFunction(x,"cumproduct")
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
  if (! isTRUE(x$getSchema()$type == "array")) stop("Non-array callback value cannot be addressed by index. Check if the callback requires a binary operator")
  graph = x$getProcess()$getGraph()
  
  FUN = "array_element"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](data=x,index=i-1) # do index shift because javascript / JSON addresses an element of an array from 0 to n-1
}

# mathematical operators ----
.plus = function(e1,e2){
  .genericBinaryFunction(e1,e2,"sum")
}

#' @export
`+.ProcessNode` <- .plus

#' @export
`+.callback-value` <- .plus


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


.multiply = function(e1,e2) {
  # v0.4.2 -> multiply or product of array
  # v0.5 -> multiply of two values, product of an array of values
  graph = .getGraph(e1,e2)
  
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


.divide = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"divide")
}

#' @export
`/.ProcessNode` <- .divide

#' @export
`/.callback-value` <- .divide


.power = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"power")
}

#' @export
`^.ProcessNode` <- .power

#' @export
`^.callback-value` <- .power


.mod = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"mod")
}

#' @export
`%%.ProcessNode` <- .mod

#' @export
`%%.callback-value` <- .mod

# logical operators ----

.not = function(e1,e2) {
  .genericUnaryFunction(e1,"not")
}
#' @export
`!.ProcessNode` <- .not

#' @export
`!.callback-value` <- .not


.and = function(e1,e2) {
  graph = .getGraph(e1,e2)
  
  # v0.5 -> and only has two arguments
  # TODO think to overwrite 'all'
  
  FUN = "and"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}
#' @export
`&.ProcessNode` <- .and

#' @export
`&.callback-value` <- .and


.or = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  # v0.5 -> or only has two arguments, now it takes an array
  # TODO think to overwrite 'any'
  
  FUN = "or"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}
#' @export
`|.ProcessNode` <- .or

#' @export
`|.callback-value` <- .or


.xor = function(x,y) {
  graph = .getGraph(x,y)
  
  FUN = "xor"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](expressions=c(x,y)) 
}

#' @export
`xor.ProcessNode` <- .xor

#' @export
`xor.callback-value` <- .xor


.equals = function(e1,e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "eq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`==.ProcessNode` <- .equals

#' @export
`==.callback-value` <- .equals


.notequal = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "neq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
} 
#' @export
`!=.ProcessNode` <- .notequal

#' @export
`!=.callback-value` <- .notequal


.smaller = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "lt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`<.ProcessNode` <- .smaller

#' @export
`<.callback-value` <- .smaller


.smaller_eq = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "lte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`<=.ProcessNode` <- .smaller_eq

#' @export
`<=.callback-value` <- .smaller_eq


.greater_eq = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "gte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`>=.ProcessNode` <- .greater_eq

#' @export
`>=.callback-value` <- .greater_eq


.greater = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "gt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  graph[[FUN]](x=e1,y=e2) 
}

#' @export
`>.ProcessNode` <- .greater

#' @export
`>.callback-value` <- .greater

# 3. group
# summary operators ----
.sum <- function(..., na.rm=FALSE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="sum")
}
#' @export
`sum.ProcessNode` <- .sum
#' @export
`sum.callback-value` <- .sum
#' @export
`sum.list` <- .sum


.prod <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="product")
}
#' @export
`prod.ProcessNode` <- .prod
#' @export
`prod.callback-value` <- .prod
#' @export
`prod.list` <- .prod


.min <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="min")
}
#' @export
`min.ProcessNode` <- .min
#' @export
`min.callback-value` <- .min
#' @export
`min.list` <- .min


.max <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="max")
}
#' @export
`max.ProcessNode` <- .max

#' @export
`max.callback-value` <- .max

#' @export
`max.list` <- .max


.range <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="extrema")
}

#' @export
`range.ProcessNode` <- .range

#' @export
`range.callback-value` <- .range

#' @export
`range.list` <- .range


.mean <- function(x, na.rm=FALSE,...) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="mean")
}

#' @export
`mean.ProcessNode` <- .mean

#' @export
`mean.callback-value` <- .mean

#' @export
`mean.list` <- .mean




.median <- function(x, na.rm=FALSE,...) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="median")
}
#' @export
`median.ProcessNode`<- .median

#' @export
`median.callback-value`<- .median

#' @export
`median.list`<- .median


.sd <- function(x, na.rm=FALSE) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="sd")
}
#' @export
`sd.ProcessNode`  <- .sd
#' @export
`sd.callback-value`  <- .sd

#' @export
`sd.list`  <- .sd


.var <- function(x, na.rm=FALSE) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="variance")
}

#' @export
`var.ProcessNode` <- .var
#' @export
`var.callback-value` <- .var

#' @export
`var.list` <- .var


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
  if (!missing(e1) && any(c("callback-value","ProcessNode") %in% class(e1))) {
    if ("ProcessNode" %in% class(e1)) {
      return(e1$getGraph())
    } else { # callback-value
      return(e1$getProcess()$getGraph())
    }
  }
  
  if (!missing(e1) && "list" %in% class(e1)) {
    if ("ProcessNode" %in% class(e1[[1]])) {
      return(e1[[1]]$getGraph())
    } else { # callback-value
      return(e1[[1]]$getProcess()$getGraph())
    }
  }
  
  if (!missing(e2) && any(c("callback-value","ProcessNode") %in% class(e2))) {
    if ("ProcessNode" %in% class(e2)) {
      return(e2$getGraph())
    } else { # callback-value
      return(e2$getProcess()$getGraph())
    } 
    
  } else {
    # should not happen
  }
}

.checkMathConstants = function(x, graph) {
  if (is.numeric(x)) {
    if (isTRUE(x == exp(1))) {
      FUN = "e"
      if (FUN %in% names(graph)) {
        return(graph[[FUN]]())
      } else {
        warning("Mathematical constant 'e' detected, but back-end does not support function 'e()'. Using approximated value instead.")
      }
    }
    
    if (isTRUE(x == pi)) {
      FUN = "pi"
      if (FUN %in% names(graph)) {
        return(graph[[FUN]]())
      } else {
        warning("Mathematical constant 'pi' detected, but back-end does not support function 'pi()'. Using approximated value instead.")
      }
    }
  }
  
  return(x)
}

.autoArraySubset = function(x) {
  if ("callback-value" %in% class(x)) {
    
    if (length(x$getSchema()$type) > 0 && x$getSchema()$type == "array") {
      x = x[1]
      warning("Using first index of the input parameter else do explicit subset.")
    }
  }
  return(x)
}

.genericUnaryFunction = function(x,FUN) {
  graph = .getGraph(e1=x)
  
  if (!FUN %in% names(graph)) {
    stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  }
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  graph[[FUN]](x=x) 
}

.genericBinaryFunction = function(e1,e2,FUN) {
  graph = .getGraph(e1,e2)
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
  
  # if callback-value, then check if array or single value
  e1 = .autoArraySubset(e1)
  e2 = .autoArraySubset(e2)
  
  e1 = .checkMathConstants(e1,graph)
  e2 = .checkMathConstants(e2,graph)
  graph[[FUN]](data = c(e1,e2)) 
}

.genericAggregationFunction = function(x, ..., FUN) {
    graph = .getGraph(x)
    params = list(...)
    na.rm = FALSE
    
    if ("na.rm" %in% names(params)) {
      na.rm = params$`na.rm`
    }
    
    # check x
    if ("callback-value" %in% class(x)) {
      
    }
    
    if ("ProcessNode" %in% class(x)) {
      
    } 
    
    if ("list" %in% class(x)) {
      # do type checking? everything should me a single value with the same type or it should
      # be of any type
      x = lapply(x, function(elem){
        .checkMathConstants(elem,graph)
      })
    }
    
    if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a callback graph via the function 'openeo::callback'."))
    
    if ("ignore_nodata" %in% names(formals(graph[[FUN]]))) {
      graph[[FUN]](data = x,ignore_nodata=na.rm) 
    } else {
      graph[[FUN]](data = x) 
    }
}