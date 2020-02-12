# Parameter ====
# should also be abstract

#' Parameter class
#' 
#' This class defines parameters of \code{\link{Process}}. They store information about the type, format and
#' the pattern. Those classes are designed to not carry any value, because if the would it would be a 
#' \code{\link{Argument}}. 
#' 
#' The parameters are parsed from the specific description and format of the JSON
#' objects returned for the parameters in processes. Find a list of openEO specific formats here: \url{https://open-eo.github.io/openeo-api/processes/#openeo-specific-formats}
#' 
#' @name Parameter
#' 
#' @return Object of \code{\link{R6Class}} which represents a parameter.
#' 
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{$new(name, description,required=FALSE)}}{}
#'  \item{\code{$isRequired()}}{return whether a parameter is mandatory or not}
#'  \item{\code{$getName}}{returns the name of a parameter as string}
#'  \item{\code{$setName(value)}}{sets the name of a parameter}
#'  \item{\code{$getPattern()}}{returns a string with the pattern of a parameter description}
#'  \item{\code{$setPattern(pattern)}}{sets the pattern (string) for a parameter}
#'  \item{\code{$matchesSchema(schema)}}{returns TRUE if the given schema - a list of the parsed openEO 
#'  API schema object - matches this parameters schema, which is used for finding the corresponding parameter}
#'  \item{\code{$isNullable()}}{returns TRUE if the parameter is allowed to be nullable, FALSE otherwise}
#' }
#' @section Arguments:
#' \describe{
#'   \item{\code{name}}{The name of a parameter}
#'   \item{\code{description}}{}
#'   \item{\code{required}}{logical - whether or not }
#'   \item{\code{value}}{In this case also the name of a parameter}
#'   \item{\code{pattern}}{the regexp as a string how to formulate the value}
#'   \item{\code{schema}}{the parsed schema object of a process parameter as a list}
#' }
NULL

Parameter = R6Class(
  "Parameter",
  public = list(
    initialize=function(name, description,required=FALSE) {
      private$name = name
      private$description = description
      private$required = FALSE
    },
    
    isRequired = function() {
      return(private$required)
    },
    
    getName = function() {
      return(private$name)
    },
    
    setName = function(value) {
      private$name = value
    },
    
    getPattern = function() {
      return(private$schema$pattern)
    },
    setPattern = function(pattern) {
      private$schema$pattern = pattern
    },
    setDefault = function(value) {
      private$schema$default = value
      invisible(self)
    },
    getDefault = function() {
      return(private$schema$default)
    },
    matchesSchema = function(schema) {
      sel = c("type","format")
      
      if (is.null(schema$type)) schema$type = character()
      if (is.null(schema$format)) schema$format = character()
      
      if (length(schema$type) == 0 && length(schema$format) == 0) return(TRUE) # TODO add unchecked warning?
      
      return(setequal(private$schema[sel], schema[sel]))
    },
    getSchema = function() {
      return(private$schema)
    }
  ),
  active = list(
    isNullable = function(value) {
      if (missing(value)) {
        return(private$nullable)
      } else {
        value = as.logical(value)
        
        if (is.na(value)) {
          warning("Cannot cast value to logical. Assume FALSE.")
          value = FALSE
        }
        
        private$nullable = value
      }
    }
  ),
  private = list(
    name=character(),
    nullable = FALSE,
    schema = list(
      type=character(),
      format = character(),
      pattern = character(),
      parameters = list(), # potential callback parameter
      default = character(),
      
      # items are relevant for arrays
      items = list(
        type=NULL # type name, e.g. "string", "array","number","any", etc.
      ),
      minItems = integer(),
      maxItems = integer()
    ),
    required = logical(),
    description = character()
  )
)

# Argument ====
# should be abstract class
#' Argument class
#' 
#' This class inherits all fields and functions from \code{\link{Parameter}} and augments this class for 
#' managing a value. This includes getter/setter, validation and serilization. Since this is the parent class
#' for the type specific argument classes the inheriting classes implement their own version of the private
#' functions \code{$typeCheck()} and \code{$typeSerialization()}.
#' 
#' @name Argument
#' 
#' @return Object of \code{\link{R6Class}} which represents an argument.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$setValue(value)}}{Assigns a value for this argument}
#'   \item{\code{$getValue()}}{Returns the value of this argument}
#'   \item{\code{$serialize()}}{returns a list representation of a openEO argument}
#'   \item{\code{$validate()}}{return TRUE if the parameter is validated positively by the type check}
#'   \item{\code{$isEmpty()}}{returns TRUE if the value is set}
#' }
#' @section Arguments: 
#' \describe{
#'   \item{\code{value}}{The value for this argument.}
#' }
NULL

Argument = R6Class(
  "Argument",
  inherit=Parameter,
  public = list(
    
    setValue = function(value) {
      private$value = value
    },
    getValue = function() {
      private$value
    },
    serialize = function() {
      # nullable / required / value = NULL
      if (self$isNullable && 
          (length(self$getValue()) == 0 || 
           (!is.environment(self$getValue()) && 
            is.na(self$getValue()))) && 
          self$isRequired()) {
        return(NA)
      }
      
      if ("Graph" %in% class(private$value)) {
        if (!"callback" %in% class(self)) {
          return(private$value$serialize())
        } 
      }
      
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serializeAsReference())
      }
      
      if (any(c("callback-value","variable") %in% class(self$getValue()))) {
        return(self$getValue()$serialize())
      }
      
      # for format specific conversion overwrite this by children
      tryCatch({
        return(private$typeSerialization())
      }, error = function(e) {
        serialization_error = paste0("Error serializing parameter '",self$getName(),
                                     "' in process node '", self$getProcess()$getNodeId(),
                                     "' :",e$message)
        stop(serialization_error)
      })
      
    },
    validate = function() {
      tryCatch(
        {
          private$checkRequiredNotSet()
          
          if (!self$isRequired() && 
              !is.environment(private$value) && 
              self$isEmpty()) {
            
          } else {
            if (!any(c("callback-value","variable") %in% class(self$getValue()))) private$typeCheck()
          }
          
          invisible(NULL)
        }, error = function(e) {
          node_id = self$getProcess()$getNodeId()
          if (!is.null(node_id)) node_id = paste0("[",node_id,"] ")
          
          message = paste0(node_id,"Parameter '",private$name,"': ",e$message)
          
          return(message)
        }
      )
    },
    
    isEmpty = function() {
      return(!is.environment(private$value) && (
                is.null(private$value) ||
                is.na(private$value) ||
                length(private$value) == 0))
    },
    getProcess = function() {
      return(private$process)
    },
    setProcess = function(p) {
      private$process = p
      
      return(invisible(self))
    }
  ),
  private = list(
    value=NULL,
    process = NULL,
    
    checkRequiredNotSet = function() {
      if (private$required && 
          !self$isNullable && 
          !is.environment(private$value) &&
          self$isEmpty()) stop("Argument is required, but has not been set.")
    },
    
    typeCheck = function() {
      # implemented / overwritten by children
    },
    
    typeSerialization = function() {
      # implemented / overwritten by children
      
      #if nothing is done, then simply return the object
      return(private$value)
    },
    
    checkMultiResults = function() {
      if ("ProcessNode" %in% class(private$value)) {
        returns = private$value$getReturns()
        
        if (!is.null(returns$schema)) {
          if (!is.null(returns$schema$anyOf)) {
            output_candidated = returns$schema$anyOf
            
            if (!any(sapply(output_candidated,function(schema){self$matchesSchema(schema)}))) {
              stop("Cannot match any of the provided returns of the prior process to this parameter")
            }
          }
        }
      }
      
      
    }
    
  )
)

# Variable ====

#' Variable class
#' 
#' This class reflects a variable that can be used within a process graph. It inherits all fields and functions
#' from \code{\link{Argument}}. If the value is not set, the variable is serialized as a variable, but if it is, 
#' then the variable is replaced with the set value. The variable is created by \code{\link{create_variable}}.
#' 
#' @name Variable
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a variable.
#'
NULL

Variable = R6Class(
  "variable",
  inherit=Argument,
  public = list(
    initialize=function(id=character(),description=character(),type="string",default=NULL) {
      private$name = id
      private$description = description
      private$schema$default = default
      private$schema$type = type
    }
  ),
  private = list(
    typeSerialization = function() {
      # if we want to bulk set a variable with setValue, e.g. an often used collection replace the variable with the set value
      if (is.null(private$value)) {
        res = list(variable_id = private$name)
        
        if (length(private$description) > 0 && !is.na(private$description)) res = append(res,list(description=private$description))
        if (length(private$schema$type) > 0 && ! is.na(private$schema$type)) res= append(res, list(type=private$schema$type))
        if (length(private$schema$default) > 0 && !is.na(private$schema$default)) res = append(res, list(default=private$schema$default))
        
        return(res)
      } else {
        return(private$value)
      }
      
    }
  )
)

# Integer ====
#' Integer class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a single integer value.
#' 
#' @name Integer
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents an Integer
NULL

Integer = R6Class(
  "integer",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "integer"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.integer(private$value)) {
        suppressWarnings({
          coerced = as.integer(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into integer."))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.integer(private$value))
    }
  )
)

# EPSG-Code ====
#' EPSGCode class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent an EPSG Code as a single integer value.
#' 
#' @name EPSGCode
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents an EPSG code as Integer
NULL

EPSGCode = R6Class(
  "epsg-code",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "integer"
      private$schema$format = "epsg-code"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.integer(private$value)) {
        suppressWarnings({
          coerced = as.integer(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into integer."))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.integer(private$value))
    }
  )
)

# Number ====
#' Number class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a numeric value.
#' 
#' @name Number
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a Number
NULL

Number = R6Class(
  "number",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "number"
    },
    
    setValue = function(value) {
      process_collection = self$getProcess()$getGraph()
      private$value = .checkMathConstants(value,process_collection)
    }
  ),
  private = list(
    typeCheck = function() {
      
      if ("ProcessNode" %in% class(private$value)) {
        # check return value?
        return_schema = private$value$getReturns()$schema
        
        if (!is.null(return_schema$type) && !"number" %in% unlist(return_schema$type))
          stop(paste0("Value 'ProcessNode' does not return the ANY object nor a number."))
      } else if (!is.numeric(private$value)) {
        suppressWarnings({
          coerced = as.numeric(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a number."))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serialize())
      } else {
        return(as.numeric(private$value))
      }
    }
  )
)

# String ====
#' String class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a character string value.
#' 
#' @name String
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a string.
NULL

String = R6Class(
  "string",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# Output Format ====
#' OutputFormat class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent an output format of a back-end as  a 
#' character string value.
#' 
#' @name OutputFormat
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents an output format of a back-end.
NULL

OutputFormat = R6Class(
  "output-format",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "output-format"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# CollectionId ====
#' CollectionId class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a collection id on an openeo back-end.
#' 
#' @name CollectionId
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a collection id.
NULL

CollectionId = R6Class(
  "collection-id",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "collection-id"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (!grepl(pattern=private$schema$pattern,x=private$value,perl=TRUE)) stop(paste0("The provided regexpr pattern does not match the value: ",private$value))
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      } else {
        if (!grepl(pattern=private$schema$pattern,x=private$value,perl=TRUE)) stop(paste0("The provided value does not match the required pattern: ",private$value))
      }
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# JobId ====
#' JobId class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a job id on an openeo back-end.
#' 
#' @name JobId
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents the id of a job.
NULL

JobId = R6Class(
  "job-id",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "job-id"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (!grepl(pattern=private$schema$pattern,x=private$value, perl=TRUE)) stop(paste0("The provided regexpr pattern does not match the value: ",private$value))
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      } else {
        if (!grepl(pattern=private$schema$pattern,x=private$value,perl=TRUE)) stop(paste0("The provided value does not match the required pattern: ",private$value))
      }
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# ProcessGraphId ====
#' ProcessGraphId
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a process graph id on an openeo back-end.
#' 
#' @name ProcessGraphId
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents the id of a process graph.
NULL

ProcessGraphId = R6Class(
  "process-graph-id",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "process-graph-id"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (!grepl(pattern=private$schema$pattern,x=private$value,perl=TRUE)) stop(paste0("The provided regexpr pattern does not match the value: ",private$value))
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      } else {
        if (!grepl(pattern=private$schema$pattern,x=private$value,perl=TRUE)) stop(paste0("The provided value does not match the required pattern: ",private$value))
      }
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# Proj-Definition ====
#' ProjDefinition
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a projection definition as a PROJ string.
#' 
#' @name ProjDefinition
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a projection definition based on PROJ.
NULL

ProjDefinition = R6Class(
  "proj-definition",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "proj-definition"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character proj definition."))
        # correct value if you can
        private$value = coerced
      } 
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# Bounding Box ====
#' BoundingBox
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a bounding box / extent of a certain area of 
#' interest. Its value is usually a named list with "west","south","east" and "north". A NA value means a open
#' interval.
#' 
#' @name BoundingBox
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a bounding box / extent.
NULL

BoundingBox = R6Class(
  "bounding-box",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$format = "bounding-box"
    }
  ),
  private = list(
    typeCheck = function() {
      # should be a list
      if (!is.list(private$value)) {
        tryCatch(
          {
            private$value = as.list(private$value)
          },
          error = function(e) {
            stop("Cannot coerce values of bounding box into a list")
          }
        )
      }
      
      obj_names = names(private$value)
      
      if (length(obj_names) == 0) stop("Bounding box parameter are unnamed. Cannot distinguish values.") 
      
      # check if west, south, east, north are set and all are numeric values or coercable as numeric
      required_dir_params = c("west","south","east","north")
      if (!all(required_dir_params %in% obj_names)) stop(paste0(
        "Bounding box parameters are missing: ", paste(required_dir_params[!required_dir_params %in% obj_names],collapse = ", ")
      ))
      
      suppressWarnings({
        vals = lapply(private$value[obj_names],as.numeric)
        nas = sapply(vals, is.na)
        
        if (any(nas)) {
          stop("Not all bbox parameters are numeric or can be automatically coerced into numeric: ",paste0(obj_names[nas],collapse = ", "))
        } else {
          private$value[obj_names] = vals
        }
        
      })
      
      # check if crs is set (either proj string or epsg code)
      if ("crs" %in% obj_names) {
        crs_value = private$value[["crs"]]
        if (!is.integer(crs_value)) {
          if (!is.character(crs_value)) stop("CRS is not an EPSG identifier or a PROJ string")
          
          # automatical conversion in this EPSG cases
          if (grepl(pattern="epsg:", tolower(crs_value))) {
            private$value[["crs"]] = as.integer(sub(pattern = "epsg:",replacement = "",tolower(crs_value)))
          }
        }
      } # else nothing, since it is not required, but its assumed to be WGS84
      
      # check if base and height are set (both or none), also those have to be numeric
      height_selector = c("base","height")
      if (do.call(xor,as.list(height_selector %in% obj_names))) {
        stop("Height was considered, but either 'base' or 'height' is missing.")
      }
      
      if (all(height_selector %in% obj_names)) {
        height_extent = private$value[height_selector]
        suppressWarnings({
          height_extent = sapply(height_extent,as.numeric)
          
          if (any(sapply(height_extent,is.na))) {
            stop("'Base' or 'height' cannot be interpreted as numeric value")
          } else {
            private$value[height_selector] = height_extent
          }
        })
      }
    },
    typeSerialization = function() {
      if (length(self$getValue()) == 0) {
        return(NULL)
      } else {
        return(self$getValue())
      }
    }
  )
)

# Boolean ====
#' Boolean
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a boolean / logical.
#' 
#' @name Boolean
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a boolean / logical.
NULL

Boolean = R6Class(
  "boolean",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "boolean"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.logical(private$value)) {
        suppressWarnings({
          coerced = as.logical(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a boolean/logical."))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.logical(private$value))
    }
  )
)

# Date ====
#' Date
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a date.
#' 
#' @name Date
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a date.
NULL

Date = R6Class(
  "date",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "date"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.Date(private$value)) {
        suppressWarnings({
          coerced = as_date(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a date"))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.character(format(private$value,format = "%Y-%m-%d")))
    }
  )
)

# DateTime ====
#' DateTime
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a date with time component.
#' 
#' @name DateTime
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a date with time component.
NULL

DateTime = R6Class(
  "date-time",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "date-time"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.POSIXct(private$value)) {
        suppressWarnings({
          coerced = as_datetime(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a date time object"))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.character(format(private$value,format = "%Y-%m-%dT%H:%M%SZ")))
    }
  )
)

# Time ====
#' Time
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the time of a day.
#' 
#' @name Time
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents the time of a day.
NULL

Time = R6Class(
  "time",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$format = "time"
    },
    setValue = function(value) {
      # the value will be a posixct where we just return the time component
      if (is.character(value)) {
        private$value = strptime(value, format="%H:%M:%SZ")
      } else {
        private$value= value
      }
    },
    getValue = function() {
      return(self$serialize())
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.POSIXct(private$value)) {
        suppressWarnings({
          coerced = strptime(value, format="%H:%M:%SZ")
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a time representation"))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      return(as.character(format(private$value,format = "%H:%M:%SZ")))
    }
  )
)

# GeoJson ====
#' GeoJson
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a geojson object. This basically means that this
#' class represents geospatial features.
#' 
#' @name GeoJson
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents an object in geojson.
NULL

GeoJson = R6Class(
  "geojson",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$format = "geojson"
    }
  ),
  private = list(
    typeCheck = function() {
      #TODO implement! object == list in R
      stop("Not implemented")
    },
    typeSerialization = function() {
      return(as.list(private$value))
    }
  )
)

# OutputFormatOptions ====
#' OutputFormatOptions
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the additional output format options of a back-end.
#' 
#' @name OutputFormatOptions
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents output format options.
NULL

OutputFormatOptions = R6Class(
  "output-format-options",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$format = "output-format-options"
    }
  ),
  private = list(
    typeCheck = function() {
      #TODO implement! object == list in R
    },
    typeSerialization = function() {
      return(as.list(private$value))
    }
  )
)

# ProcessGraphVariables ====
#' ProcessGraphVariables
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the process graph variables that enable a mapping. 
#' When the already stored Graph that contains variables is loaded for execution.
#' 
#' @name ProcessGraphVariables
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents the mapping of variables and values at runtime.
NULL

ProcessGraphVariables = R6Class(
  "process-graph-variables",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$format = "process-graph-variables"
    }
  ),
  private = list(
    typeCheck = function() {
      #TODO implement! object == list in R
    },
    typeSerialization = function() {
      return(as.list(private$value))
    }
  )
)

# RasterCube ====
#' RasterCube
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a raster cube. This is usually the in- and 
#' output format of a process. Unless the process operates within a callback on reduced data. Analogous to
#' this the \code{\link{VectorCube}} behaves in the same manner, but with spatial feature data.
#' 
#' @name RasterCube
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a raster cube.
NULL

RasterCube = R6Class(
  "raster-cube",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$format = "raster-cube"
    }
  ),
  private = list(
    typeCheck = function() {
      # a raster data cube can only be derived by process, e.g. get_collection so this
      # value should be a ProcessNode
      if (! "ProcessNode" %in% class(private$value)) stop("RasterCube is not retreived by process.")
      
      private$checkMultiResults()
    },
    typeSerialization = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serializeAsReference())
      }
      
      return(as.character(private$value))
    }
  )
)

# VectorCube ====
#' VectorCube
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a vector cube. This is in analogy to
#' the \code{\link{RasterCube}}.
#' 
#' @name VectorCube
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a vector cube.
NULL

VectorCube = R6Class(
  "vector-cube",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$format = "vector-cube"
    }
  ),
  private = list(
    typeCheck = function() {
      # a vector data cube can only be derived by process, e.g. get_collection so this
      # value should be a ProcessNode
      if (! "ProcessNode" %in% class(private$value)) stop("VectorCube is not retreived by process.")
      
      private$checkMultiResults()
      # if (! "vector-cube" %in% class(private$value$getProcess()$getReturns())) stop("The stated process does not return a VectorCube")
    },
    typeSerialization = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serializeAsReferences())
      }
      
      return(as.character(private$value))
    }
  )
)

# Callback ====
#' Callback
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a callback. The callback operates on reduced data
#' of a data cube. For example reducing the time dimension results in a time series that has to be reduced into a
#' single value. The value of a callback is usually a \code{\link{Graph}} with \code{\link{CallbackValue}} as 
#' injected data. Hints from the openeo api documention:
#' \itemize{
#'   \item \url{https://open-eo.github.io/openeo-api/processes/#callbacks}
#'   \item \url{https://open-eo.github.io/openeo-api/v/0.4.2/processgraphs/#callbacks}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$getCallbackParameters()}}{returns the available list \code{\link{CallbackValue}}}
#'   \item{\code{$setCallbackParameters(parameters)}}{assigns a list of \code{\link{CallbackValue}} to the callback}
#' }
#' 
#' @section Arguments:
#' \describe{
#'   \item{\code{parameters}}{the list \code{\link{CallbackValue}}}
#' }
#' 
#' @name CallbackArgument
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a callback.
NULL

Callback = R6Class(
  "callback",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$format = "callback"
    },
    
    setValue = function(value) {
      # if (! "ProcessNode" %in% class(value)) stop("Callback function is no Process / ProcessNode")
      if ("function" %in% class(value)) {
        # if value is a function -> then make a call with the function and a suitable callback 
        # parameter
        # create a new graph
        # con = private$process$getGraph()$getConnection()
        # new_graph = process_graph_builder(con)
        # old_graph = private$process$getGraph()
        
        process_collection = private$process$getGraph()
        
        # probably switch temporarily the graph of the parent process
        # then all newly created process nodes go into the new graph
        private$process$setGraph(process_collection)
        
        # find suitable callback parameter (mostly array or binary) -> check for length of formals
        callback_parameter = private$parameters
        names(callback_parameter) = names(formals(value))
        
        lapply(callback_parameter, function(cb){cb$setProcess(private$process)})
        
        # make call
        final_node = do.call(value,args = callback_parameter)
        
        # then serialize it via the final node
        
        
        #add the process node list to this graph
        # void = lapply(node_list, function(node) {
        #   if (node$getNodeId() %in% sapply(old_graph$getNodes(),function(x)x$getNodeId())) {
        #     old_graph$removeNode(node$getNodeId())
        #   }
        #   
        #   if (!node$getNodeId() %in% sapply(new_graph$getNodes(),function(x)x$getNodeId())) {
        #     new_graph$addNode(node)
        #   }
        #   
        # })
        
        # switch back the graph
        # private$process$setGraph(old_graph)
        
        # add final node
        # new_graph$setFinalNode(final_node)
        
        # assign new graph as value
        value = Graph$new(final_node = final_node)
      }
      
      
      private$value = value
    },
    
    setCallbackParameters = function(parameters) {
      private$parameters = parameters
    },
    
    getCallbackParameters = function() {
      return(private$parameters)
    }
    
  ),
  private = list(
    parameters = list(),
    
    typeCheck = function() {
      # check the value (graph) for the same callback parameters (CallbackValues)
      if (!"Graph" %in% class(private$value)) stop("The value of a callback argument is usually a graph.")
      
      errors = private$value$validate()
      
      if (any(errors != TRUE)) {
        stop(paste("Errors in subgraph:",paste(errors,collapse=";")))
      }

      
    },
    
    typeSerialization = function() {
      if(!is.null(private$value)) {
        # serialize the graph
        return(list(callback=private$value$serialize()))
      }
    }
  )
)

# CallbackValue ====
#' CallbackValue
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the available data within a callback graph.
#' Hints from the openeo api documention:
#' \itemize{
#'   \item \url{https://open-eo.github.io/openeo-api/processes/#callbacks}
#'   \item \url{https://open-eo.github.io/openeo-api/v/0.4.2/processgraphs/#callbacks}
#' }
#' 
#' @name CallbackValue
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a callback value.
NULL

# in case the callback-value is an arry - which it will be in most cases - we have to store
# process nodes for array subsetting in the object with its index. This should be done to 
# reuse the results of previous steps
CallbackValue = R6Class(
  "callback-value",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),
                        description=character(),
                        type=character(),
                        format=character(),
                        required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = type
      private$schema$format = format
    },
    print = function() {
      cat(toJSON(self$serialize(),pretty = TRUE, auto_unbox = TRUE))
      invisible(self)
    }
  ),
  private = list(
    typeSerialization = function() {
      res = list(from_argument=private$name)
      return(res)
    }
  )
)

setOldClass(c("callback-value","Argument","Parameter","R6"))

# Array ====
#' Array
#' 
#' Inheriting from \code{\link{Argument}} in order to represent an array of a single data type.
#' 
#' @name Array
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$getMinItems}}{returns the minimum number of items}
#'   \item{\code{$getMaxItems}}{returns the maximum number of items}
#'   \item{\code{$setMinItems(value)}}{sets the minimum number of items}
#'   \item{\code{$setMaxItems(value)}}{sets the maximum number of items}
#'   \item{\code{$getItemSchema}}{returns the item schema of the items in the array}
#'   \item{\code{$setItemSchema(value)}}{sets the schema for the items in the array}
#' }
#' 
#' @section Arguments:
#' \describe{
#'   \item{\code{value}}{either a number describing the minimum and maximum number of elements in an array or the
#'   parsed JSON schema of a single item in the array}
#' }
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a single valued array.
NULL

Array = R6Class(
  "array",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),
                        description=character(),
                        type=character(),
                        format=character(),
                        items=list(),
                        required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "array"
      private$schema$format = format
      
      private$schema$items = items
    },

    getMinItems = function() {
      return(private$schema[["minItems"]])
    },
    getMaxItems = function() {
      return(private$schema[["maxItems"]])
    },
    setMinItems = function(value) {
      private$schema[["minItems"]] = value
    },
    setMaxItems = function(value) {
      private$schema[["maxItems"]] = value
    },
    
    getItemSchema = function() {
      return(private$schema$items)
    },
    
    setItemSchema = function(value) {
      
      if (!"type" %in% names(value)) {
        value[["type"]]="any"
      } else if (any(value$type == "null")) {
        value$type[value$type == "null"] = NULL
        value$type = unlist(value$type)
        value$nullable = TRUE
      }
      
      if (is.null(value[["minItems"]])) value[["minItems"]] = integer()
      if (is.null(value[["maxItems"]])) value[["maxItems"]] = integer()
      
      private$schema$items = value
    },
    
    setValue = function(value) {
      process_collection = self$getProcess()$getGraph()
      
      if (length(value) > 0) {
        private$value = lapply(value, function(x, pc) {
          .checkMathConstants(x,pc)
        }, pc = process_collection)
      }
    }
  ),
  private = list(
    typeCheck = function() {
      itemType = private$schema$items$type
      if (itemType == "any") {
        # this can be anything so we shift the responsibility to the back-end
        #TODO maybe give a warning that it is unchecked
        return() 
      }
      
      if (length(private$schema$minItems) == 1 && 
          length(private$value) < private$schema$minItems) {
        stop(paste0("Minimum items are not achieved. Found ",length(private$value)," items of minimal ",private$schema$minItems," items."))
      }
      if (length(private$schema$maxItems) == 1 && 
          length(private$value) > private$schema$maxItems) {
        stop(paste0("More items than are maximal required. Found ",length(private$value)," items of maximal ",private$schema$maxItems," items."))
      }

      if (itemType == "array") {
        # just check the first layer, everything else would be nice, but is no more in our responsibility
        if ("callback-value" %in% class(private$value)) {
          if (length(private$value$getSchema()$type) > 0 && 
              private$value$getSchema()$type == "array")
            if (length(private$value$getSchema()$items$type) > 0) {
              if (private$value$getSchema()$items$type == private$schema$items$type) {
                return()
              }
            } else {
              stop("Selected callback-value is an array, but has a different item type.")
            }
            
        }
        
        allOK = all(sapply(private$value, function(item) {
          # item is an array type -> list or vector
          itemsItemType = private$schema$items$type
          itemsMinItems = private$schema$items$minItems
          itemsMaxItems = private$schema$items$maxItems
          
          # check nested item type
          typeOK = switch(itemsItemType,
                          string = is.character(item),
                          number = is.numeric(item),
                          integer = is.integer(item),
                          boolean = is.logical(item),
                          array = is.list(item) || is.vector(item))
          
          # check min/max if set
          minOK = NULL
          if (!is.null(itemsMinItems) && length(itemsMinItems) == 1){
            minOK = length(item) >= itemsMinItems
          }
          
          maxOK = NULL
          if (!is.null(itemsMaxItems) && length(itemsMaxItems) == 1){
            maxOK = length(item) >= itemsMaxItems
          }
          
          return(all(unlist(list(typeOK,minOK,maxOK))))
          
        }))
        
        if (!allOK) stop("At least one of the nested array has not the correct item type or the min/max constraint was triggered.")
        
      } else {
        if (!"callback-value" %in% class(private$value[[1]])) {
          allOK = switch(itemType,
                         string = all(sapply(private$value,function(val){
                           if ("Process" %in% class(val)) {
                             returnSchema = val$getReturns()$schema
                             
                             return(String$new()$matchesSchema(returnSchema))
                           } else {
                             return(is.character(val))
                           }
                         })),
                         number = all(sapply(private$value,function(val){
                           if ("Process" %in% class(val)) {
                             returnSchema = val$getReturns()$schema
                             
                             return(Number$new()$matchesSchema(returnSchema))
                           } else {
                             return(is.numeric(val))
                           }
                         })),
                         integer = all(sapply(private$value,function(val){
                           if ("Process" %in% class(val)) {
                             returnSchema = val$getReturns()$schema
                             
                             return(Integer$new()$matchesSchema(returnSchema))
                           } else {
                             return(is.integer(val))
                           }
                         })),
                         boolean = all(sapply(private$value,function(val){
                           if ("Process" %in% class(val)) {
                             returnSchema = val$getReturns()$schema
                             
                             return(Boolean$new()$matchesSchema(returnSchema))
                           } else {
                             return(is.logical(val))
                           }
                         }))
          )
          # if allOK == null then it was not checked
          if (!is.null(allOK) && !allOK) {
            stop(paste0("At least one element in the array is not of type: ",itemType))
          } 
        } 
        
      }
      
      
      if (!is.list(private$value)) {
        if (length(private$value) == 1) {
          private$value = list(private$value)
          return()
        }
        
        suppressWarnings({
          coerced = as.list(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a boolean."))
        # correct value if you can
        private$value = coerced
      }
    },
    typeSerialization = function() {
      if (length(self$getValue()) == 0 || is.na(self$getValue())) {
        return(NA)
      }
      
      if ("callback-value" %in% class(self$getValue()[[1]])) {
        serialized = lapply(self$getValue(),function(arg)arg$serialize())
        if (length(serialized) == 1) {
          serialized = serialized[[1]]        
        } 
        return(serialized)
      } else {
        return(
          lapply(self$getValue(), function(value) {
            
            if ("ProcessNode" %in% class(value)) return(value$serializeAsReference())
            
            if ("Argument" %in% class(value)) return(value$serialize())
            
            return(value)
          })
        )
      }
    })
)

# Kernel ====
#' Kernel
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a kernel that shall be applied on the data cube.
#' 
#' @name Kernel
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a Kernel.
NULL

Kernel = R6Class(
  "kernel",
  inherit=Array,
  public = list(
    initialize=function(name=character(),
                        description=character(),
                        type=character(),
                        format=character(),
                        items=list(),
                        required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "array"
      private$schema$format = "kernel"
      private$schema$items = items
    }
  )
)

#TemporalInterval ====
#' TemporalInterval
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a temporal interval. Open interval borders are
#' denoted with NA. Exactly two objects are in the temporal interval.
#' 
#' @name TemporalInterval
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a temporal interval.
NULL

TemporalInterval = R6Class(
  "temporal-interval",
  inherit=Array,
  public = list(
    initialize=function(name=character(),
                        description=character(),
                        type=character(),
                        format=character(),
                        items=list(),
                        required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "array"
      private$schema$format = "temporal-interval"
      private$schema$items = items
      private$schema$maxItems = 2
      private$schema$minItems = 2
    },
    setValue = function(value) {
      private$value = value
    }
  )
)

#TemporalIntervals ====
#' TemporalIntervals
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a list of \code{\link{TemporalInterval}}. 
#' 
#' @name TemporalIntervals
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a list temporal intervals.
NULL

TemporalIntervals = R6Class(
  "temporal-intervals",
  inherit=Array,
  public = list(
    initialize=function(name=character(),
                        description=character(),
                        type=character(),
                        format=character(),
                        items=list(),
                        required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "array"
      private$schema$format = "temporal-intervals"
      private$schema$items = items
    }
  )
)

# AnyOf ====
#' AnyOf
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a argument choice object. It means that multiple
#' types can be stated, but at least on data type has to be picked. In JSON schema this is oftern used to make
#' objects nullable - meaning that they allow NULL as value. The AnyOf is resolved into a simple nullable argument
#' if this applies. 
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$getChoice()}}{returns a list of \code{\link{Argument}} that are allowed}
#'   \item{\code{$isNullable}}{returns TRUE if only one element is in the choice that is not "null"}
#' }
#' 
#' @name AnyOf
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{CallbackArgument}}, 
#' \code{\link{CallbackValue}}, \code{\link{Variable}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}} and \code{\link{ProjDefinition}}
#' 
#' @return Object of \code{\link{R6Class}} which represents a list temporal intervals.
NULL

AnyOf = R6Class(
  "anyOf",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(), parameter_list,required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "anyOf"
      private$parameter_choice = parameter_list
    },
    setValue = function(value) {
      if (is.null(value)) {
        private$value =NULL
        return(self)
      }
      
      if ("function" %in% class(value)) {
        signature = formals(value)
        
        # currently we have only 1 parameter (either single value or array) or two (directly binary operation)
        number_of_params = sapply(private$parameter_choice,function(cb) {
          length(cb$getCallbackParameters())
        })
        
        choice_index = unname(which(number_of_params == length(signature)))
        
        if (length(choice_index) == 0) {
          stop("Cannot match function to any of the callback parameter.")
        }
        choice = private$parameter_choice[[choice_index]]
        
        #resolve anyof parameter
        self$getProcess()$setParameter(name = self$getName(),value = choice)
        choice$setName(self$getName())
        choice$setProcess(private$process)
        choice$setValue(value)
        
        return(self)
      }
      
      if ("Argument" %in% class(value)) {
        # This is mostly for callbacks
        arg_allowed = any(sapply(private$parameter_choice, function(argument) {
          all(length(setdiff(class(argument),class(value))) == 0,
              length(setdiff(class(value),class(argument))) == 0
          )
        }))
        
        if(!arg_allowed) stop("Cannot assign ",class(value)[[1]], " as value. Not allowed.")
        
        private$value = list(value)
      } else {
        # set to all sub parameters and run validate
        choice_copies = self$getChoice()
        validated = sapply(choice_copies, function(param) {
          
          param$setValue(value)
          
          tryCatch(
            {
              validation = param$validate()
              return(is.null(validation))
            },
            error = function(e) {
              return(FALSE)
            }
          )
          
          
        })
        
        private$value = unname(choice_copies[validated])
        
        private$value[[1]]$setValue(value)
        return(self)
      }
      
      
      
    },
    getValue = function() {

      # best case only one had survived the selection, if not throw an error?
      # or return the first result
      if (is.null(private$value)) return(private$value)
      
      if (class(private$value)=="list") {
        if (length(private$value)==1) {
          return(private$value[[1]]$getValue())
        }
        
        return(private$value[[1]]$getValue())
      } else {
        return(private$value$getValue())
      }
    },
    
    getChoice = function() {
      return(lapply(private$parameter_choice,function(choice){
        choice$clone(deep=TRUE)
      }))
    }
  ),
  active = list(
    isNullable = function(value) {
      
      if (missing(value)) {
        return(private$nullable)
      } else {
        value = as.logical(value)
        
        if (is.na(value)) {
          warning("Cannot cast value to logical. Assume FALSE.")
          value = FALSE
        }
        
        lapply(private$parameter_choice, function(param) {
          param$isNullable = value
        })
        
        private$nullable = value
      }
    }
  ),
  private = list(
    parameter_choice = list(),
    
    typeCheck = function() {
      # TODO rework
    },
    typeSerialization = function() {
      if (length(self$getValue()) == 0) {
        return(NULL)
      } else {
        val = self$getValue()
        
        if (!is.list(val)) {
          val = list(val)
        }
        
        val = lapply(val, function(v) {
          if ("Argument" %in% class(v)) {
            return(v$serialize())
          } else if ("ProcessNode" %in% class(v)){
            return(v$serializeAsReference())
          } else {
            return(v)
          }
        })
        
        if (length(val) == 1) return(val[[1]])
        else return(val)
        
      }
    },
    deep_clone = function(name, value) {
      
      # also check if it is a list of R6 objects
      if (name == "parameter_choice") {
        new_list = list()
        if (is.null(names(value))) {
          iterable = 1:length(value)
        } else {
          iterable = names(value)
        }
        
        for (list_name in iterable) {
          list_elem = value[[list_name]]
          
          if ("R6" %in% class(list_elem)) {
            list_elem = list_elem$clone(deep=TRUE)
          }
          
          entry = list(list_elem)
          names(entry) = list_name
          new_list = append(new_list,entry)
        }
        
        return(new_list)
        
      }
      
      if (is.environment(value) && !is.null(value$`.__enclos_env__`)) {
        return(value$clone(deep = TRUE))
      }
      
      value
    }
  )
)

# parse functions ----
findParameterGenerator = function(schema) {
  # TODO adapt this if I add some parameter/argument
  # variables are not listed since they are created at the graph
  parameter_constructor = list(Integer,
                               EPSGCode,
                               Number, 
                               Boolean,
                               BoundingBox,
                               GeoJson,
                               RasterCube, 
                               VectorCube,
                               String,
                               CollectionId,
                               JobId,
                               ProcessGraphId,
                               ProcessGraphVariables,
                               ProjDefinition,
                               OutputFormat,
                               OutputFormatOptions,
                               Callback,
                               Array,
                               Kernel,
                               Date,
                               DateTime,
                               TemporalInterval,
                               TemporalIntervals,
                               Time)
  
  matches = unlist(lapply(parameter_constructor, function(constructor){
    if(constructor$new()$matchesSchema(schema)) constructor
  }))
  
  if (is.null(matches) || length(matches) == 0) matches = list(Argument) # if we don't find anything simply use this, since it is not restricted
  
  return(matches)
}

parameterFromJson = function(param_def, nullable = FALSE) {
  if (is.null(param_def$schema$format)) param_def$schema$format = character()
  if (is.null(param_def$required)) param_def$required = FALSE
  
  type = param_def$schema$type
  
  if (is.null(type) && !is.null(param_def$schema$anyOf)) {
    #anyOf case
    # scan list for type: null
    types = sapply(param_def$schema$anyOf, function(schema)schema$type)
    
    # if present set nullable = TRUE and throw it out
    if ("null" %in% types) {
      nullable = TRUE
      param_def$schema$anyOf[[which(types=="null")]] = NULL
    }
    
    params = lapply(
      #create anyOf object
      
      param_def$schema$anyOf,
      function(anyOf_schema) {
        param_copy = param_def
        param_copy$schema = anyOf_schema
        return(parameterFromJson(param_copy))
      }
    )
    
    if (length(params) > 1) {
      choice = AnyOf$new(name=param_def$name, 
                         description=param_def$description,
                         required = param_def$required, 
                         parameter_list = params)
      
      choice$isNullable = nullable
    } else {
      nullableParameter = params[[1]]
      nullableParameter$isNullable = nullable
      return(nullableParameter)
    }
    
    return(choice)
  }
  
  if (is.list(type)) {
    # if is array and we have only null and one other, then only use the one prior parameter and make it nullable
    nullable = "null" %in% type
    if (nullable) {
      type[[which(type == "null")]] = NULL # remove this entry
    }
    
    if (length(type) > 1) {
      # create an anyOf
      param_template = param_def$schema
      types = param_template$type
      param_template$type = NULL
      
      schemas = lapply(types, function(type_name) {
        res = param_template
        res$type = type_name
        return(res)
      })
      
      param_def$schema = list(anyOf=schemas) # create new anyOf element with the schemas
      
      # recursive call -> in the next iteration it will be an anyOf case (nullable is passed on)
      return(parameterFromJson(param_def,nullable))
    } else {
      param_def$schema$type = unname(unlist(type))
    }
  }
  
  # this will be the normal case for simple schemas?
  gen=findParameterGenerator(param_def$schema)[[1]]
  param = gen$new(name=param_def$name, description=param_def$description,required = param_def$required)
  
  # in general also reolve null cases
  param$isNullable = nullable
  
  # TODO change in 1.0.0 to param_def$default
  param$setDefault(param_def$schema$default)
  
  if ("callback" %in% class(param)) {
    # iterate over all callback parameters and create CallbackParameters, but name = property name (what the process exports to callback)
    # value has to be assigned by user, then switch name and value during serialization
    
    cb_params = lapply(names(param_def$schema$parameters), function(param_name) {
      param_json = param_def$schema$parameters[[param_name]]
      if (is.null(param_json[["format"]])) param_json[["format"]] = character()
      
      # param_type = findParameterGenerator(param_json[c("type","format")])$new()
      
      
      
      cb = CallbackValue$new(name = param_name,
                             description = param_json$description,
                             type = param_json[["type"]],
                             format = param_json[["format"]],
                             required = TRUE)
      
      if(!is.null(param_json[["pattern"]])) cb$setPattern(param_json[["pattern"]])
      
      return(cb)
    })
    names(cb_params) = names(param_def$schema$parameters)
    
    # note: later all CBs need to be bound by the user
    
    param$setCallbackParameters(cb_params)
  }
  
  if ("array" %in% class(param)) {
    if (!"kernel" %in% class(param)) {
      param$setItemSchema(param_def$schema$items)
    } else {
      param$setItemSchema(param_def$schema$items)
    }
    
  }
  return(param)
}

processFromJson=function(json) {
  if (is.null(json$summary)) json$summary = character()
  if (is.null(json$parameter_order)) json$parameter_order = character()
  
  tryCatch({
    #map parameters!
    parameters = lapply(
      names(json$parameters), function(name) {
        pdef = json$parameters[[name]]
        if (is.null(pdef$name)) {
          pdef$name = name
        }
        
        # set param if it is contained in the schema
        param = parameterFromJson(pdef)
        pattern = pdef$schema$pattern
        if (!is.null(pattern)) {
          param$setPattern(pattern)
        }
        
        return(param)
      }
    )
  
    Process$new(id=json$id,
                description = json$description,
                summary=json$summary,
                parameters = parameters,
                returns = json$returns,
                parameter_order = json$parameter_order)
  }, error = function(e) {
    warning(paste0("Invalid process description for '",json$id,"'"))
    NULL
  })

}