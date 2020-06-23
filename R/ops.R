# Definitions for operators to make it easier to create arithmetic ProcessGraphs

# it works on process nodes and ProcessGraphParameter


# 1. Group

.abs = function(x) {
  .genericUnaryFunction(x,"absolute")
}

#' @export
`abs.ProcessNode` <- .abs

#' @export
`abs.ProcessGraphParameter` <- .abs


.sign = function(x) {
  .genericUnaryFunction(x,"sgn")
}
#' @export
`sign.ProcessNode` <- .sign
#' @export
`sign.ProcessGraphParameter` <- .sign

.sqrt <- function(x) {
  .genericUnaryFunction(x,"sqrt")
}
#' @export
`sqrt.ProcessNode` <- .sqrt

#' @export
`sqrt.ProcessGraphParameter` <- .sqrt

.trunc = function(x) {
  .genericUnaryFunction(x,"int")
}
#' @export
`trunc.ProcessNode` <- .trunc

#' @export
`trunc.ProcessGraphParameter` <- .trunc

.floor = function(x) {
  .genericUnaryFunction(x,"floor")
}
#' @export
`floor.ProcessNode` <- .floor

#' @export
`floor.ProcessGraphParameter` <- .floor

.ceiling = function(x) {
  .genericUnaryFunction(x,"ceil")
}
#' @export
`ceiling.ProcessNode` <- .ceiling

#' @export
`ceiling.ProcessGraphParameter` <- .ceiling

.round = function(x,digits=0) {
  graph = .getGraph(x)
  
  FUN = "round"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  graph[[FUN]](x=x,p=digits) 
}
#' @export
`round.ProcessNode` <- .round

#' @export
`round.ProcessGraphParameter` <- .round


.exp = function(x) {
  .genericUnaryFunction(x,"exp")
}

#' @export
`exp.ProcessNode` <- .exp

#' @export
`exp.ProcessGraphParameter` <- .exp


.log = function(x,base=exp(1)) {
  graph = .getGraph(x)
  
  if (base==exp(1)) {
    FUN = "ln"
  } else {
    FUN = "log"
  }
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  
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
`log.ProcessGraphParameter` <- .log


.log10 = function(x) {
  .log(x,10)
}
#' @export
`log10.ProcessNode` <- .log10

#' @export
`log10.ProcessGraphParameter` <- .log10

# trigonometric functions ----
.cos = function(x) {
  .genericUnaryFunction(x,"cos")
}

#' @export
`cos.ProcessNode` <- .cos

#' @export
`cos.ProcessGraphParameter` <- .cos


.sin = function(x) {
  .genericUnaryFunction(x,"sin")
}
#' @export
`sin.ProcessNode` <- .sin

#' @export
`sin.ProcessGraphParameter` <- .sin


.tan = function(x) {
  .genericUnaryFunction(x,"tan")
}
#' @export
`tan.ProcessNode` <- .tan

#' @export
`tan.ProcessGraphParameter` <- .tan


.cosh = function(x) {
  .genericUnaryFunction(x,"cosh")
}

#' @export
`cosh.ProcessNode` <- .cosh

#' @export
`cosh.ProcessGraphParameter` <- .cosh


.sinh = function(x) {
  .genericUnaryFunction(x,"sinh")
}

#' @export
`sinh.ProcessNode` <- .sinh

#' @export
`sinh.ProcessGraphParameter` <- .sinh


.tanh = function(x) {
  .genericUnaryFunction(x,"tanh")
}

#' @export
`tanh.ProcessNode` <- .tanh

#' @export
`tanh.ProcessGraphParameter` <- .tanh


.acos = function(x) {
  .genericUnaryFunction(x,"arccos")
}

#' @export
`acos.ProcessNode` <- .acos

#' @export
`acos.ProcessGraphParameter` <- .acos


.asin = function(x) {
  .genericUnaryFunction(x,"arcsin")
}

#' @export
`asin.ProcessNode` <- .asin

#' @export
`asin.ProcessGraphParameter` <- .asin


.atan = function(x) {
  .genericUnaryFunction(x,"arctan")
}

#' @export
`atan.ProcessNode` <- .atan

#' @export
`atan.ProcessGraphParameter` <- .atan


.acosh = function(x) {
  .genericUnaryFunction(x,"arccosh")
}
#' @export
`acosh.ProcessNode` <- .acosh

#' @export
`acosh.ProcessGraphParameter` <- .acosh


.asinh = function(x) {
  .genericUnaryFunction(x,"arcsinh")
}
#' @export
`asinh.ProcessNode` <- .asinh

#' @export
`asinh.ProcessGraphParameter` <- .asinh


.atanh = function(x) {
  .genericUnaryFunction(x,"arcsinh")
}
#' @export
`atanh.ProcessNode` <- .atanh

#' @export
`atanh.ProcessGraphParameter` <- .atanh


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
#     if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
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
#' @export
`cumsum.ProcessNode` <- .cumsum

#' @export
`cumsum.ProcessGraphParameter` <- .cumsum


.cummin <- function(x) {
  .genericUnaryFunction(x,"cummin")
}

#' @export
`cummin.ProcessNode` <- .cummin

#' @export
`cummin.ProcessGraphParameter` <- .cummin


.cummax <- function(x) {
  .genericUnaryFunction(x,"cummax")
}

#' @export
`cummax.ProcessNode` <- .cummax

#' @export
`cummax.ProcessGraphParameter` <- .cummax


.cumprod <- function(x) {
  .genericUnaryFunction(x,"cumproduct")
}

#' @export
`cumprod.ProcessNode` <- .cumprod

#' @export
`cumprod.ProcessGraphParameter` <- .cumprod

# 2. Group
# [ (subset) ====
#' @export
`[.ProcessGraphParameter` <- function(x,i,...,drop=TRUE) {
  # check x for being an array
  if (length(x$getSchema()$type) > 0 && 
      !isTRUE(x$getSchema()$type == "array")) stop("Non-array ProcessGraph value cannot be addressed by index. Check if the ProcessGraph requires a binary operator")
  
  if (is.null(x$getProcess())) {
    graph = processes()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  FUN = "array_element"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  
  if (is.numeric(i)) {
    graph[[FUN]](data=x,index=i-1) # do index shift because javascript / JSON addresses an element of an array from 0 to n-1
  } else if (is.character(i)) {
    graph[[FUN]](data=x,label=i) # do index shift because javascript / JSON addresses an element of an array from 0 to n-1
  } else {
    stop("Subsetting is neither done by integer nor with a label/character.")
  }
  
}

# mathematical operators ----
.plus = function(e1,e2){
  .genericBinaryFunction(e1,e2,"sum")
}

#' @export
`+.ProcessNode` <- .plus

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

#' @export
`-.ProcessNode` <- .minus

#' @export
`-.ProcessGraphParameter` <- .minus


.multiply = function(e1,e2) {
  # v0.4.2 -> multiply or product of array
  # v0.5 -> multiply of two values, product of an array of values
  graph = .getGraph(e1,e2)
  
  if ("multiply" %in% names(graph)) {
    FUN = "multiply"
  } else if ("product" %in% names(graph)) {
    FUN = "product"
  } else {
    stop("Neither 'multiply' nor 'product' are available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'.")
  }
  .genericBinaryFunction(e1,e2,FUN)
}

#' @export
`*.ProcessNode` <- .multiply

#' @export
`*.ProcessGraphParameter` <- .multiply


.divide = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"divide")
}

#' @export
`/.ProcessNode` <- .divide

#' @export
`/.ProcessGraphParameter` <- .divide


.power = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"power")
}

#' @export
`^.ProcessNode` <- .power

#' @export
`^.ProcessGraphParameter` <- .power


.mod = function(e1,e2) {
  .genericBinaryFunction(e1,e2,"mod")
}

#' @export
`%%.ProcessNode` <- .mod

#' @export
`%%.ProcessGraphParameter` <- .mod

# logical operators ----

.not = function(e1,e2) {
  .genericUnaryFunction(e1,"not")
}
#' @export
`!.ProcessNode` <- .not

#' @export
`!.ProcessGraphParameter` <- .not


.and = function(e1,e2) {
  graph = .getGraph(e1,e2)
  
  # v0.5 -> and only has two arguments
  # TODO think to overwrite 'all'
  
  FUN = "and"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}
#' @export
`&.ProcessNode` <- .and

#' @export
`&.ProcessGraphParameter` <- .and


.or = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  # v0.5 -> or only has two arguments, now it takes an array
  # TODO think to overwrite 'any'
  
  FUN = "or"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](expressions=c(e1,e2)) 
}
#' @export
`|.ProcessNode` <- .or

#' @export
`|.ProcessGraphParameter` <- .or


.xor = function(x,y) {
  graph = .getGraph(x,y)
  
  FUN = "xor"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](expressions=c(x,y)) 
}

#' @export
`xor.ProcessNode` <- .xor

#' @export
`xor.ProcessGraphParameter` <- .xor


.equals = function(e1,e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "eq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`==.ProcessNode` <- .equals

#' @export
`==.ProcessGraphParameter` <- .equals


.notequal = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "neq"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](x=e1,y=e2) 
} 
#' @export
`!=.ProcessNode` <- .notequal

#' @export
`!=.ProcessGraphParameter` <- .notequal


.smaller = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "lt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`<.ProcessNode` <- .smaller

#' @export
`<.ProcessGraphParameter` <- .smaller


.smaller_eq = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "lte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`<=.ProcessNode` <- .smaller_eq

#' @export
`<=.ProcessGraphParameter` <- .smaller_eq


.greater_eq = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "gte"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](x=e1,y=e2) 
}
#' @export
`>=.ProcessNode` <- .greater_eq

#' @export
`>=.ProcessGraphParameter` <- .greater_eq


.greater = function(e1, e2) {
  graph = .getGraph(e1,e2)
  
  FUN = "gt"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](x=e1,y=e2) 
}

#' @export
`>.ProcessNode` <- .greater

#' @export
`>.ProcessGraphParameter` <- .greater

# 3. group
# summary operators ----
.sum <- function(..., na.rm=FALSE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="sum")
}
#' @export
`sum.ProcessNode` <- .sum
#' @export
`sum.ProcessGraphParameter` <- .sum
#' @export
`sum.list` <- .sum


.prod <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="product")
}
#' @export
`prod.ProcessNode` <- .prod
#' @export
`prod.ProcessGraphParameter` <- .prod
#' @export
`prod.list` <- .prod


.min <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="min")
}
#' @export
`min.ProcessNode` <- .min
#' @export
`min.ProcessGraphParameter` <- .min
#' @export
`min.list` <- .min


.max <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="max")
}
#' @export
`max.ProcessNode` <- .max

#' @export
`max.ProcessGraphParameter` <- .max

#' @export
`max.list` <- .max


.range <- function(..., na.rm=TRUE) {
  elems = list(...)
  .genericAggregationFunction(x=elems,na.rm=na.rm,FUN="extrema")
}

#' @export
`range.ProcessNode` <- .range

#' @export
`range.ProcessGraphParameter` <- .range

#' @export
`range.list` <- .range


.mean <- function(x, na.rm=FALSE,...) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="mean")
}

#' @export
`mean.ProcessNode` <- .mean

#' @export
`mean.ProcessGraphParameter` <- .mean

#' @export
`mean.list` <- .mean




.median <- function(x, na.rm=FALSE,...) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="median")
}
#' @export
`median.ProcessNode`<- .median

#' @export
`median.ProcessGraphParameter`<- .median

#' @export
`median.list`<- .median


.sd <- function(x, na.rm=FALSE) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="sd")
}
#' @export
`sd.ProcessNode`  <- .sd
#' @export
`sd.ProcessGraphParameter`  <- .sd

#' @export
`sd.list`  <- .sd


.var <- function(x, na.rm=FALSE) {
  .genericAggregationFunction(x=x,na.rm=na.rm,FUN="variance")
}

#' @export
`var.ProcessNode` <- .var
#' @export
`var.ProcessGraphParameter` <- .var

#' @export
`var.list` <- .var


.quantile <- function(x, probs, na.rm=FALSE) {
  if ("ProcessNode" %in% class(x)) {
    graph = x$getGraph()
  } else {
    graph = x$getProcess()$getGraph()
  }
  
  
  FUN = "quantiles"
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  graph[[FUN]](data = x,probabilites = probs)
}
#' @export
`quantile.ProcessNode` <- .quantile
#' @export
`quantile.ProcessGraphParameter` <- .quantile


# utility functions ====
# returns the graph / or better the process collection used to create the process
.getGraph = function(e1,e2) {
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
    var = .find_var_in_stack(varname = ".__process_collection__")
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
  graph = .getGraph(e1=x)
  
  if (!FUN %in% names(graph)) {
    stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  }
  
  x = .autoArraySubset(x)
  x = .checkMathConstants(x,graph)
  graph[[FUN]](x=x) 
}

.genericBinaryFunction = function(e1,e2,FUN) {
  graph = .getGraph(e1,e2)
  
  if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
  
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
    graph = .getGraph(x)
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
    
    if (!FUN %in% names(graph)) stop(paste0("Process '",FUN,"' is not available at the back-end. Please check the provided processes for alternatives and create a ProcessGraph graph via the function 'openeo::ProcessGraph'."))
    
    if ("ignore_nodata" %in% names(formals(graph[[FUN]]))) {
      graph[[FUN]](data = x,ignore_nodata=na.rm) 
    } else {
      graph[[FUN]](data = x) 
    }
}