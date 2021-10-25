# Definitions for operators to make it easier to create arithmetic ProcessGraphs

# it works on process nodes and ProcessGraphParameter


# 1. Group

#' Unary function wrappers
#' 
#' The functions here are used in combination with \code{ProcessGraphParameter} and \code{ProcessNode} and facilitate
#' writing arithmetic functions for openEO user defined processes in R. The functions translate into their openEO 
#' processes counterparts.
#' 
#' @param x \code{ProcessGraphParameter}, \code{ProcessNode} or a list or vector. Passes internal data to the function
#' @param ... further arguments to pass on, see the documentation of primitive functions of R for further information
#' @param digits the amount of decimal digits to round to
#' @param i the index of the element in a vector or list
#' @param drop listed for completeness but not used in openEO processes.
#' @param base the base of the exponential operation
#' 
#' @return a \code{ProcessNode}
#' 
#' @name unary_ops
NULL

.abs = function(x) {
  .genericUnaryFunction(x,"absolute")
}

#' @rdname unary_ops
#' @export
`abs.ProcessNode` <- .abs

#' @rdname unary_ops
#' @export
`abs.ProcessGraphParameter` <- .abs


.sign = function(x) {
  .genericUnaryFunction(x,"sgn")
}
#' @rdname unary_ops
#' @export
`sign.ProcessNode` <- .sign
#' @rdname unary_ops
#' @export
`sign.ProcessGraphParameter` <- .sign

.sqrt <- function(x) {
  .genericUnaryFunction(x,"sqrt")
}
#' @rdname unary_ops
#' @export
`sqrt.ProcessNode` <- .sqrt
#' @rdname unary_ops
#' @export
`sqrt.ProcessGraphParameter` <- .sqrt

.trunc = function(x, ...) {
  .genericUnaryFunction(x,"int")
}
#' @rdname unary_ops
#' @export
`trunc.ProcessNode` <- .trunc
#' @rdname unary_ops
#' @export
`trunc.ProcessGraphParameter` <- .trunc

.floor = function(x) {
  .genericUnaryFunction(x,"floor")
}
#' @rdname unary_ops
#' @export
`floor.ProcessNode` <- .floor
#' @rdname unary_ops
#' @export
`floor.ProcessGraphParameter` <- .floor

.ceiling = function(x) {
  .genericUnaryFunction(x,"ceil")
}
#' @rdname unary_ops
#' @export
`ceiling.ProcessNode` <- .ceiling
#' @rdname unary_ops
#' @export
`ceiling.ProcessGraphParameter` <- .ceiling

.round = function(x,digits=0) {
  graph = .getProcessCollection(x)
  
  FUN = "round"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  graph[[FUN]](x=x,p=digits) 
}
#' @rdname unary_ops
#' @export
`round.ProcessNode` <- .round
#' @rdname unary_ops
#' @export
`round.ProcessGraphParameter` <- .round

.exp = function(x) {
  .genericUnaryFunction(x,"exp")
}
#' @rdname unary_ops
#' @export
`exp.ProcessNode` <- .exp
#' @rdname unary_ops
#' @export
`exp.ProcessGraphParameter` <- .exp


.log = function(x,base=exp(1)) {
  graph = .getProcessCollection(x)
  
  if (base==exp(1)) {
    FUN = "ln"
  } else {
    FUN = "log"
  }
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  
  if (FUN == "log") {
    graph[[FUN]](x=x,base=base) 
  } else {
    graph[[FUN]](x=x) 
  }
  
}
#' @rdname unary_ops
#' @export
`log.ProcessNode` <- .log
#' @rdname unary_ops
#' @export
`log.ProcessGraphParameter` <- .log


.log10 = function(x) {
  .log(x,10)
}
#' @rdname unary_ops
#' @export
`log10.ProcessNode` <- .log10
#' @rdname unary_ops
#' @export
`log10.ProcessGraphParameter` <- .log10

# trigonometric functions ----
.cos = function(x) {
  .genericUnaryFunction(x,"cos")
}
#' @rdname unary_ops
#' @export
`cos.ProcessNode` <- .cos
#' @rdname unary_ops
#' @export
`cos.ProcessGraphParameter` <- .cos


.sin = function(x) {
  .genericUnaryFunction(x,"sin")
}
#' @rdname unary_ops
#' @export
`sin.ProcessNode` <- .sin
#' @rdname unary_ops
#' @export
`sin.ProcessGraphParameter` <- .sin


.tan = function(x) {
  .genericUnaryFunction(x,"tan")
}
#' @rdname unary_ops
#' @export
`tan.ProcessNode` <- .tan
#' @rdname unary_ops
#' @export
`tan.ProcessGraphParameter` <- .tan


.cosh = function(x) {
  .genericUnaryFunction(x,"cosh")
}
#' @rdname unary_ops
#' @export
`cosh.ProcessNode` <- .cosh
#' @rdname unary_ops
#' @export
`cosh.ProcessGraphParameter` <- .cosh


.sinh = function(x) {
  .genericUnaryFunction(x,"sinh")
}
#' @rdname unary_ops
#' @export
`sinh.ProcessNode` <- .sinh
#' @rdname unary_ops
#' @export
`sinh.ProcessGraphParameter` <- .sinh


.tanh = function(x) {
  .genericUnaryFunction(x,"tanh")
}
#' @rdname unary_ops
#' @export
`tanh.ProcessNode` <- .tanh
#' @rdname unary_ops
#' @export
`tanh.ProcessGraphParameter` <- .tanh


.acos = function(x) {
  .genericUnaryFunction(x,"arccos")
}
#' @rdname unary_ops
#' @export
`acos.ProcessNode` <- .acos
#' @rdname unary_ops
#' @export
`acos.ProcessGraphParameter` <- .acos


.asin = function(x) {
  .genericUnaryFunction(x,"arcsin")
}
#' @rdname unary_ops
#' @export
`asin.ProcessNode` <- .asin
#' @rdname unary_ops
#' @export
`asin.ProcessGraphParameter` <- .asin


.atan = function(x) {
  .genericUnaryFunction(x,"arctan")
}
#' @rdname unary_ops
#' @export
`atan.ProcessNode` <- .atan
#' @rdname unary_ops
#' @export
`atan.ProcessGraphParameter` <- .atan


.acosh = function(x) {
  .genericUnaryFunction(x,"arccosh")
}
#' @rdname unary_ops
#' @export
`acosh.ProcessNode` <- .acosh
#' @rdname unary_ops
#' @export
`acosh.ProcessGraphParameter` <- .acosh


.asinh = function(x) {
  .genericUnaryFunction(x,"arcsinh")
}
#' @rdname unary_ops
#' @export
`asinh.ProcessNode` <- .asinh
#' @rdname unary_ops
#' @export
`asinh.ProcessGraphParameter` <- .asinh


.atanh = function(x) {
  .genericUnaryFunction(x,"arcsinh")
}
#' @rdname unary_ops
#' @export
`atanh.ProcessNode` <- .atanh
#' @rdname unary_ops
#' @export
`atanh.ProcessGraphParameter` <- .atanh


# internal function does not work
# @export
# `atan2` = function(y,x) {
#   UseMethod("atan2")
# }
# 
# .atan2 = function(y,x) {
#     graph = .getProcessCollection(y,x)
# 
#     FUN = "arctan2"
#     if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
#     graph[[FUN]](y=y,x=x)
# }
# @export
# `atan2.ProcessNode` <- .atan2
# 
# @export
# `atan2.ProcessGraphParameter` <- .atan2

# cummulative ----
.cumsum <- function(x) {
  .genericUnaryFunction(x,"cumsum")
}
#' @rdname unary_ops
#' @export
`cumsum.ProcessNode` <- .cumsum
#' @rdname unary_ops
#' @export
`cumsum.ProcessGraphParameter` <- .cumsum


.cummin <- function(x) {
  .genericUnaryFunction(x,"cummin")
}
#' @rdname unary_ops
#' @export
`cummin.ProcessNode` <- .cummin
#' @rdname unary_ops
#' @export
`cummin.ProcessGraphParameter` <- .cummin


.cummax <- function(x) {
  .genericUnaryFunction(x,"cummax")
}
#' @rdname unary_ops
#' @export
`cummax.ProcessNode` <- .cummax
#' @rdname unary_ops
#' @export
`cummax.ProcessGraphParameter` <- .cummax


.cumprod <- function(x) {
  .genericUnaryFunction(x,"cumproduct")
}
#' @rdname unary_ops
#' @export
`cumprod.ProcessNode` <- .cumprod
#' @rdname unary_ops
#' @export
`cumprod.ProcessGraphParameter` <- .cumprod

# 2. Group
# [ (subset) ====
#' @rdname unary_ops
#' @export
`[.ProcessGraphParameter` <- function(x,i,...,drop=TRUE) {
  # check x for being an array
  if (length(x$getSchema()$type) > 0 && 
      !isTRUE(x$getSchema()$type == "array")) stop("Non-array ProcessGraph value cannot be addressed by index. Check if the ProcessGraph requires a binary operator")
  
  graph = .getProcessCollection(e1=x)
  
  # if (is.null(x$getProcess())) {
  #   graph = processes()
  # } else {
  #   graph = x$getProcess()$getGraph()
  # }
  
  FUN = "array_element"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  
  if (is.numeric(i)) {
    graph[[FUN]](data=x,index=i-1) # do index shift because javascript / JSON addresses an element of an array from 0 to n-1
  } else if (is.character(i)) {
    graph[[FUN]](data=x,label=i) # do index shift because javascript / JSON addresses an element of an array from 0 to n-1
  } else {
    stop("Subsetting is neither done by integer nor with a label/character.")
  }
  
}

#' Binary function wrappers
#' 
#' The functions here are used in combination with \code{ProcessGraphParameter} and \code{ProcessNode} in order to make
#' it easier to write arithmetic functions for openEO user defined processes in R. The functions map into their openEO 
#' processes counterparts.
#' 
#' @param e1 \code{ProcessGraphParameter}, \code{ProcessNode} or a list or vector, which internal data is passed into 
#' the function or a numeric value
#' @param e2 same as e1
#' @param x the first expression in the xor statement
#' @param y the seconde expression in the xor statement
#' 
#' @return a \code{ProcessNode}
#' 
#' @name binary_ops
NULL

# mathematical operators ----
.plus = function(e1,e2){
  .genericBinaryFunction(e1,e2,"add")
}
#' @rdname binary_ops
#' @export
`+.ProcessNode` <- .plus
#' @rdname binary_ops
#' @export
`+.ProcessGraphParameter` <- .plus


.minus = function(e1,e2) {
  # v0.4.2 -> subtract of array
  # v0.5 -> subtract of two values
  if (missing(e2)) {
    return(-1 * e1)
  }
  
  .genericBinaryFunction(e1,e2,"subtract")
}
#' @rdname binary_ops
#' @export
`-.ProcessNode` <- .minus
#' @rdname binary_ops
#' @export
`-.ProcessGraphParameter` <- .minus


.multiply = function(e1,e2) {
  # v0.4.2 -> multiply or product of array
  # v0.5 -> multiply of two values, product of an array of values
  graph = .getProcessCollection(e1,e2)
  
  if ("multiply" %in% names(graph)) {
    FUN = "multiply"
  } else if ("product" %in% names(graph)) {
    FUN = "product"
  } else {
    stop("Neither 'multiply' nor 'product' are available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'.")
  }
  .genericBinaryFunction(e1,e2,FUN)
}
#' @rdname binary_ops
#' @export
`*.ProcessNode` <- .multiply
#' @rdname binary_ops
#' @export
`*.ProcessGraphParameter` <- .multiply


.divide = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"divide")
}
#' @rdname binary_ops
#' @export
`/.ProcessNode` <- .divide
#' @rdname binary_ops
#' @export
`/.ProcessGraphParameter` <- .divide


.power = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"power")
}
#' @rdname binary_ops
#' @export
`^.ProcessNode` <- .power
#' @rdname binary_ops
#' @export
`^.ProcessGraphParameter` <- .power


.mod = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"mod")
}
#' @rdname binary_ops
#' @export
`%%.ProcessNode` <- .mod
#' @rdname binary_ops
#' @export
`%%.ProcessGraphParameter` <- .mod

# logical operators ----

.not = function(x) {
  .genericUnaryFunction(x,"not")
}
#' @rdname unary_ops
#' @export
`!.ProcessNode` <- .not
#' @rdname unary_ops
#' @export
`!.ProcessGraphParameter` <- .not


.and = function(e1,e2) {
  graph = .getProcessCollection(e1,e2)
  
  # v0.5 -> and only has two arguments
  FUN = "and"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}
#' @rdname binary_ops
#' @export
`&.ProcessNode` <- .and
#' @rdname binary_ops
#' @export
`&.ProcessGraphParameter` <- .and


.or = function(e1, e2) {
  graph = .getProcessCollection(e1,e2)
  
  # v0.5 -> or only has two arguments, now it takes an array
  FUN = "or"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}
#' @rdname binary_ops
#' @export
`|.ProcessNode` <- .or
#' @rdname binary_ops
#' @export
`|.ProcessGraphParameter` <- .or


.xor = function(x,y) {
  graph = .getProcessCollection(x,y)
  
  FUN = "xor"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](expressions=c(x,y)) 
}
#' @rdname binary_ops
#' @export
`xor.ProcessNode` <- .xor
#' @rdname binary_ops
#' @export
`xor.ProcessGraphParameter` <- .xor


.equals = function(e1,e2) {
  graph = .getProcessCollection(e1,e2)
  
  FUN = "eq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @rdname binary_ops
#' @export
`==.ProcessNode` <- .equals
#' @rdname binary_ops
#' @export
`==.ProcessGraphParameter` <- .equals


.notequal = function(e1, e2) {
  graph = .getProcessCollection(e1,e2)
  
  FUN = "neq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @rdname binary_ops
#' @export
`!=.ProcessNode` <- .notequal
#' @rdname binary_ops
#' @export
`!=.ProcessGraphParameter` <- .notequal


.smaller = function(e1, e2) {
  graph = .getProcessCollection(e1,e2)
  
  FUN = "lt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @rdname binary_ops
#' @export
`<.ProcessNode` <- .smaller
#' @rdname binary_ops
#' @export
`<.ProcessGraphParameter` <- .smaller


.smaller_eq = function(e1, e2) {
  graph = .getProcessCollection(e1,e2)
  
  FUN = "lte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @rdname binary_ops
#' @export
`<=.ProcessNode` <- .smaller_eq
#' @rdname binary_ops
#' @export
`<=.ProcessGraphParameter` <- .smaller_eq


.greater_eq = function(e1, e2) {
  graph = .getProcessCollection(e1,e2)
  
  FUN = "gte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @rdname binary_ops
#' @export
`>=.ProcessNode` <- .greater_eq
#' @rdname binary_ops
#' @export
`>=.ProcessGraphParameter` <- .greater_eq


.greater = function(e1, e2) {
  graph = .getProcessCollection(e1,e2)
  
  FUN = "gt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @rdname binary_ops
#' @export
`>.ProcessNode` <- .greater
#' @rdname binary_ops
#' @export
`>.ProcessGraphParameter` <- .greater

# 3. group

#' Group operator wrappers
#' 
#' R's mathematical group primitives that are translated to openEO processes.
#' 
#' @param ... multiple arguments that start with a \code{ProcessNode} or a \code{ProcessGraphParameter}
#' @param na.rm logical to determine if NA values shall be removed in the calculation
#' @param x a vector or list of values that are mixed or consist fully of \code{ProcessNode}, 
#' \code{ProcessGraphParameter} or numerical values
#' 
#' @return \code{ProcessNode}
#' @name group_ops
#' 
NULL

# summary operators ----
.sum <- function(..., na.rm=FALSE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="sum")
}
#' @rdname group_ops
#' @export
`sum.ProcessNode` <- .sum
#' @export
#' @rdname group_ops
`sum.ProcessGraphParameter` <- .sum
#' @rdname group_ops
#' @export
`sum.list` <- .sum


.prod <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="product")
}
#' @rdname group_ops
#' @export
`prod.ProcessNode` <- .prod
#' @rdname group_ops
#' @export
`prod.ProcessGraphParameter` <- .prod
#' @rdname group_ops
#' @export
`prod.list` <- .prod


.min <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="min")
}
#' @rdname group_ops
#' @export
`min.ProcessNode` <- .min
#' @rdname group_ops
#' @export
`min.ProcessGraphParameter` <- .min
#' @rdname group_ops
#' @export
`min.list` <- .min


.max <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="max")
}
#' @rdname group_ops
#' @export
`max.ProcessNode` <- .max
#' @rdname group_ops
#' @export
`max.ProcessGraphParameter` <- .max
#' @rdname group_ops
#' @export
`max.list` <- .max


.range <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="extrema")
}
#' @rdname group_ops
#' @export
`range.ProcessNode` <- .range
#' @rdname group_ops
#' @export
`range.ProcessGraphParameter` <- .range
#' @rdname group_ops
#' @export
`range.list` <- .range


.mean <- function(x, na.rm=FALSE,...) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="mean")
}
#' @rdname group_ops
#' @export
`mean.ProcessNode` <- .mean
#' @rdname group_ops
#' @export
`mean.ProcessGraphParameter` <- .mean
#' @rdname group_ops
#' @export
`mean.list` <- .mean




.median <- function(x, na.rm=FALSE,...) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="median")
}
#' @rdname group_ops
#' @importFrom stats median
#' @export
`median.ProcessNode`<- .median
#' @rdname group_ops
#' @export
`median.ProcessGraphParameter`<- .median
#' @rdname group_ops
#' @export
`median.list`<- .median


.sd <- function(x, na.rm=FALSE) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="sd")
}
#' @rdname group_ops
#' @importFrom stats sd
#' @export
`sd.ProcessNode`  <- .sd
#' @rdname group_ops
#' @export
`sd.ProcessGraphParameter`  <- .sd
#' @rdname group_ops
#' @export
`sd.list`  <- .sd


.var <- function(x, na.rm=FALSE) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="variance")
}
#' @rdname group_ops
#' @importFrom stats var
#' @export
`var.ProcessNode` <- .var
#' @rdname group_ops
#' @export
`var.ProcessGraphParameter` <- .var
#' @rdname group_ops
#' @export
`var.list` <- .var


.quantile <- function(x, ...) {
  args = list(...)
  probs = args$probs
  na.rm = args$na.rm
  
  if (length(na.rm) == 0) na.rm = FALSE
  
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  
  FUN = "quantiles"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  graph[[FUN]](data = x,probabilites = probs)
}
#' @rdname unary_ops
#' @importFrom stats quantile
#' @export
`quantile.ProcessNode` <- .quantile
#' @rdname unary_ops
#' @export
`quantile.ProcessGraphParameter` <- .quantile


# utility functions ====
# returns the graph / or better the process collection used to create the process
.getProcessCollection = function(e1,e2) {
  tryCatch({
    if (!missing(e1) && any(c("ProcessGraphParameter","ProcessNode") %in% class(e1))) {
      if ("ProcessNode" %in% class(e1)) {
        return(e1$getGraph())
      } else { # ProcessGraphParameter
        if (is.null(e1$getProcess())) { # will happen with untyped parameter (store a graph that is just a subgraph)
          return(processes())
        } else {
          return(e1$getProcess()$getGraph())
        }
        
      }
    }
    
    if (!missing(e1) && "list" %in% class(e1)) {
      if ("ProcessNode" %in% class(e1[[1]])) {
        return(e1[[1]]$getGraph())
      } else { # ProcessGraphParameter
        if (is.null(e1[[1]]$getProcess())) { # will happen with untyped parameter (store a graph that is just a subgraph)
          return(processes())
        } else {
          return(e1[[1]]$getProcess()$getGraph())
        }
        
      }
    }
    
    if (!missing(e2) && any(c("ProcessGraphParameter","ProcessNode") %in% class(e2))) {
      if ("ProcessNode" %in% class(e2)) {
        return(e2$getGraph())
      } else { # ProcessGraphParameter
        return(e2$getProcess()$getGraph())
        if (is.null(e2$getProcess())) { # will happen with untyped parameter (store a graph that is just a subgraph)
          return(processes())
        } else {
          return(e2$getProcess()$getGraph())
        }
        
      } 
      
    } else {
      # should not happen
    }
  }, error = function(e) {
    var = .find_var_in_stack(varname = ".__process_collection__") # if we don't have any clue which builder to use, then
    return(var)
  })
}

.find_var_in_stack = function(varname, env) {
  # no recursion
  if(missing(env) || is.null(env)) {
    env = parent.frame()
  }
  
  pos = 1
  while(!(all(names(env) %in% names(.GlobalEnv)) && 
          all(names(.GlobalEnv) %in% names(env)))) {
    if (varname %in% names(env)) {
      return(get(x = varname,envir = env))
    } else {
      pos = pos+1
      env = parent.frame(n=pos)
    }
  }

  return(NULL) # variable not found
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
  if ("ProcessGraphParameter" %in% class(x)) {
    
    if (length(x$getSchema()$type) > 0 && x$getSchema()$type == "array") {
      x = x[1]
      warning("Using first index of the input parameter else do explicit subset.")
    }
  }
  return(x)
}

.genericUnaryFunction = function(x,FUN) {
  graph = .getProcessCollection(e1=x)
  
  if (!FUN %in% names(graph)) {
    stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  }
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  graph[[FUN]](x=x) 
}

.genericBinaryFunction = function(e1,e2,FUN) {
  graph = .getProcessCollection(e1,e2)
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
  
  # if ProcessGraphParameter, then check if array or single value
  e1 = .autoArraySubset(e1)
  e2 = .autoArraySubset(e2)
  
  e1 = .checkMathConstants(e1,graph)
  e2 = .checkMathConstants(e2,graph)
  
  p = as.list(formals(graph[[FUN]]))
  
  if (length(p) == 1) {
    p[[1]] = c(e1,e2)
  } else {
    p[[1]] = e1
    p[[2]] = e2
  }
  
  do.call(graph[[FUN]],args = p)
}

.genericAggregationFunction = function(x, ..., FUN) {
    graph = .getProcessCollection(x)
    params = list(...)
    na.rm = FALSE
    
    if ("na.rm" %in% names(params)) {
      na.rm = params$`na.rm`
    }
    
    # check x
    if ("ProcessGraphParameter" %in% class(x)) {
      
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
    
    if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraphArgument'."))
    
    if ("ignore_nodata" %in% names(formals(graph[[FUN]]))) {
      graph[[FUN]](data = x,ignore_nodata=na.rm) 
    } else {
      graph[[FUN]](data = x) 
    }
}