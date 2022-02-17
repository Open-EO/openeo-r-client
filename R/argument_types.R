# Parameter ====
# should also be abstract

#' Parameter class
#' 
#' This class defines parameters of \code{\link{Process}}. They store information about the type, format and 
#' pattern. A parameter class is designed to not carry any value, as opposed to an 
#' \code{\link{Argument}}. 
#' 
#' The parameters are parsed from the specific description and format of the JSON
#' objects returned for the parameters in processes. Find a list of openEO-specific formats here: 
#' \url{https://github.com/Open-EO/openeo-processes/blob/master/meta/subtype-schemas.json}
#' 
#' @name Parameter
#' 
#' @return Object of \code{\link{R6Class}} which represents a parameter.
#' 
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{$new(name, description, required=FALSE)}}{}
#'  \item{\code{$getName}}{returns the name of a parameter as string}
#'  \item{\code{$setName(name)}}{sets the name of a parameter}
#'  \item{\code{$getDescription()}}{returns the description of a parameter}
#'  \item{\code{$setDescription(description)}}{sets the description of a parameter}
#'  \item{\code{$getPattern()}}{returns a string with the pattern of a parameter description}
#'  \item{\code{$setPattern(pattern)}}{sets the pattern (string) for a parameter}
#'  \item{\code{$getDefault()}}{returns the parameter's default value}
#'  \item{\code{$setDefault(default)}}{sets the default value of a parameter}
#'  \item{\code{$matchesSchema(schema)}}{returns TRUE if the given schema - a list of the parsed openEO 
#'  API schema object - matches the parameter's schema, which is used for finding the corresponding parameter}
#'  \item{\code{$getSchema()}}{returns the schema definition}
#'  \item{\code{$asParameterInfo()}}{returns a list representation of this parameter for being sent in a JSON to the openEO service}
#'  \item{\code{$isNullable()}}{returns TRUE if the parameter is allowed to be nullable, FALSE otherwise}
#'  \item{\code{$isRequired()}}{returns whether a parameter is mandatory or not}
#'  \item{\code{$isAny()}}{returns TRUE if this parameter describes a choice of parameters}
#' }
#' @section Arguments:
#' \describe{
#'   \item{\code{name}}{character - The name of a parameter}
#'   \item{\code{description}}{character - The description of a parameter}
#'   \item{\code{required}}{logical - whether it is required or not }
#'   \item{\code{pattern}}{the regexp as a string indicating how to formulate the value}
#'   \item{\code{default}}{the regexp as a string indicating how to formulate the value}   
#'   \item{\code{schema}}{the parsed schema object of a process parameter as a list}
#' }
NULL

Parameter = R6Class(
  "Parameter",
  public = list(
    initialize=function(name=character(), description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = FALSE
    },
    
    getName = function() {
      return(private$name)
    },
    
    setName = function(name) {
      private$name = name
    },
    
    getDescription = function() {
      return(private$description)
    },
    setDescription = function(description) {
      private$description = description
      invisible(self)
    },
    
    getPattern = function() {
      return(private$schema$pattern)
    },
    setPattern = function(pattern) {
      private$schema$pattern = pattern
    },
    getEnum = function() {
      return(private$schema$enum)
    },
    setEnum = function(enum){
      private$schema$enum = enum
    },
    setDefault = function(default) {
      private$default = default
      invisible(self)
    },
    getDefault = function() {
      return(private$default)
    },
    matchesSchema = function(schema) {
      sel = c("type","subtype")
      if (is.null(schema$type)) schema$type = character()
      if (is.null(schema$subtype)) schema$subtype = character()
      
      if (length(schema$type) == 0 && length(schema$subtype) == 0) return(TRUE)
      
      return(setequal(private$schema[sel], schema[sel]))
    },
    getSchema = function() {
      return(private$schema)
    },
    asParameterInfo = function() {
      # the function will serialize a parameter as in a process definition, which will be used
      # when describing a parameter as some sort of a variable
      
      info = list()
      info$name = self$getName()

      if (length(self$getDescription()) > 0) {
        if (!is.na(self$getDescription()) || nchar(self$getDescription()) > 0) {
          info$description = self$getDescription()
        }
      }
      
      if (self$isNullable) {
        info$optional = TRUE
        info$default = NA
      } 
      # if the parameter is required "optional" can be left out
      
      if (all(c("Argument","Parameter","R6") %in% class(self)) && 
          all(class(self) %in% c("Argument","Parameter","R6"))) {
        # this is an object where anything goes in (Any)
        info$schema = list(description = "Any data type")
      } else if (all(c("anyOf","Argument","Parameter","R6") %in% class(self))) {
        info$schema = lapply(self$getChoice(), function(param) {
          param$asParameterInfo()
        })
        
        if (self$isNullable) {
          info$schema = append(private$schema, list(list(type = "null")))
        }
      } else if (self$isNullable) {
        info$schema$type = list(private$schema$type,"null")
      } else {
        info$schema = private$schema
      }
      
      info$schema = .clean_empty_fields(info$schema)
      
      return(info)
      
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
    },
    isRequired = function(value) {
      if (missing(value)) {
        return(private$required)
      } else {
        value = as.logical(value)
        
        if (is.na(value)) {
          warning("Cannot cast value to logical. Assume FALSE.")
          value = FALSE
        }
        
        private$required = value
      }
    },
    isAny = function() {
      length(private$schema$type) == 0
    }
  ),
  private = list(
    name=character(),
    nullable = FALSE,
    default = character(),
    schema = list(
      type=character(),
      subtype = character(),
      pattern = character(),
      parameters = list(), # potential ProcessGraphParameter (variables)
      
      # items are relevant for arrays
      items = list(
        type=NULL # type name, e.g. "string", "array","number","any", etc.
      ),
      minItems = integer(),
      maxItems = integer(),
      enum = character()
    ),
    required = logical(),
    description = character()
  )
)

# Argument ====
#' Argument class
#' 
#' This class inherits all fields and functions from \code{\link{Parameter}} adds the functionality to
#' manage a value. This includes getter/setter, validation and serialization. Since this is the parent class
#' for the type specific argument classes, the inheriting classes implement their own version of the private
#' functions \code{$typeCheck()} and \code{$typeSerialization()}.
#' 
#' @name Argument
#' 
#' @return Object of \code{\link{R6Class}} representing an argument.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$setValue(value)}}{Assigns a value to this argument}
#'   \item{\code{$getValue()}}{Returns the value of this argument}
#'   \item{\code{$serialize()}}{returns a list representation of a openEO argument}
#'   \item{\code{$validate()}}{return TRUE if the parameter is validated positively by the type check}
#'   \item{\code{$isEmpty()}}{returns TRUE if the value is set}
#'   \item{\code{$getProcess()}}{returns the process this parameter belongs to}
#'   \item{\code{$setProcess(p)}}{sets the owning process for this parameter}
#' }
#' @section Arguments: 
#' \describe{
#'   \item{\code{value}}{The value for this argument.}
#'   \item{\code{p}}{An object of class 'Process' or inheriting like 'ProcessNode'}
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
            is.na(self$getValue()))) 
          ) {
        if (self$isRequired) {
          return(NA)
        } else {
          return(NULL)
        }
        
      }
      
      if (any(c("ProcessGraphParameter") %in% class(self$getValue()))) {
        return(self$getValue()$serialize())
      }
      
      if ("ProcessNode" %in% class(self$getValue())) {
        return(self$getValue()$serializeAsReference())
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
          
          if (!self$isRequired && 
              !is.environment(private$value) && 
              self$isEmpty()) {
            
          } else {
            # ProcessGraphParameter -> variable
            # schema$type length == 0 -> ANY
            if (any(c("ProcessGraphParameter") %in% class(self$getValue()))) return(invisible(NULL)) 
                
            if ("ProcessNode" %in% class(self$getValue()) && self$getValue()$getReturns()$isAny) return(invisible(NULL))
              
            private$typeCheck()
          }
          
          invisible(NULL)
        }, error = function(e) {
          if (length(self$getProcess()) >0 ) {
            node_id = self$getProcess()$getNodeId()
            if (!is.null(node_id)) node_id = paste0("[",node_id,"] ")
            
            message = paste0(node_id,"Parameter '",private$name,"': ",e$message)
          } else {
            message = e$message
          }
          return(message)
        }
      )
    },
    
    isEmpty = function() {
      return(!is.environment(private$value) && !is.call(private$value) && (
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
  # private =====
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
    deep_clone = function(name, value) {
      
      if (name == "process") {
        return(value)
      }
      
      # this is the anyOf case
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
      
      return(value)
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing an Integer
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
      if (!is.na(private$value) && !is.integer(private$value)) {
        suppressWarnings({
          coerced = as.integer(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into integer."))
        # correct value if you can
        private$value = coerced
        
        
      }
      return(invisible(NULL))
    },
    typeSerialization = function() {
      if (self$isEmpty() && !self$isRequired) return(NULL) 
      else return(as.integer(private$value))
    }
  )
)

# EPSG-Code ====
#' EPSGCode class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent an EPSG Code. Allowed values are single integer values like \code{4326} or a text containing 'EPSG:' like \code{EPSG:4326}.
#' 
#' @name EPSGCode
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing an EPSG code as Integer
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
      private$schema$subtype = "epsg-code"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.integer(private$value)) {
        suppressWarnings({
          coerced = as.integer(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) {
          if (is.character(private$value) && grepl(tolower(private$value),pattern = "^epsg:")) {
            coerced = as.integer(gsub(x = private$value,replacement = "",pattern = "[^0-9]"))
          } else {
            stop(paste0("Value '", private$value,"' cannot be coerced into integer."))
          }
        }
        # correct value if you can
        private$value = coerced
        
        return(invisible(NULL))
      }
    },
    typeSerialization = function() {
      if (self$isEmpty() && !self$isRequired) return(NULL) 
      else return(as.integer(private$value))
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a number
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
        return_value = private$value$getReturns()
        if (!any(c("number","integer") %in% class(return_value) && 
                 length(return_value$getSchema()$type)) != 0) {
          
          stop(paste0("Value 'ProcessNode' returns neither the ANY object nor a number."))
        }
        
        # if (!is.null(return_schema$type) && !"number" %in% unlist(return_schema$type))
          
      } else if (!is.na(private$value) && !is.numeric(private$value)) {
        suppressWarnings({
          coerced = as.numeric(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a number."))
        # correct value if you can
        private$value = coerced
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serialize())
      } else if (self$isEmpty() && !self$isRequired) {
        return(NULL)
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a string.
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
      if (!is.na(private$value) && !is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      }
      
      if (length(self$getEnum()) > 0) {
        if (!private$value %in% self$getEnum()) {
          stop(paste0("Enum was stated, but the value does not match any enum."))
        }
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      if (is.call(private$value)) {
        return(paste(deparse(private$value),collapse = "\n"))
      } else if (is.character(private$value)) {
        if (file.exists(private$value)) {
          # if valid file path open file and attach
          tryCatch({
            suppressWarnings({
              content = readChar(private$value, file.info(private$value)$size)
              return(content)
            })
            
          }, error = function(e) {
            return(private$value)
          })
          
        } else {
          return(private$value)
        } 
      } else if (self$isEmpty() && !self$isRequired) {
        return(NULL)
      } else if (!is.environment(private$value) && is.na(private$value)) {
        return(NA)
      } else {
        return(as.character(private$value))
      }
    }
  )
)

# URI ====

URI = R6Class(
  "uri",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$subtype = "uri"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      }
      
      if (!file.exists(private$value) || !grepl(private$value,pattern="\\w+:(\\/?\\/?)[^\\s]+")) stop("Value is not an URI or file.")
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      
      if (is.character(private$value)) {
        if (file.exists(private$value)) {
          # if valid file path open file and attach
          return(readChar(private$value, file.info(private$value)$size))
        } else {
          return(private$value)
        }    
      } else {
        return(as.character(private$value))
      }
    }
  )
)

# Output Format ====
#' OutputFormat class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent an output format of a back-end as a 
#' character string value.
#' 
#' @name OutputFormat
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing an output format of a back-end.
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
      private$schema$subtype = "output-format"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
        
        if ("FileFormat" %in% class(private$value)) {
          # what to do?
        } else {
          suppressWarnings({
            coerced = as.character(private$value)
          })
          
          if (is.null(coerced) || 
              is.na(coerced) ||
              length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
          # correct value if you can
          
          private$value = coerced
        }
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      if ("FileFormat" %in% class(private$value)) {
        return(private$value$name)
      } else {
        return(as.character(private$value))
      }
    }
  )
)

# CollectionId ====
#' CollectionId class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a CollectionId on an openeo back-end.
#' 
#' @name CollectionId
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a CollectionId.
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
      private$schema$subtype = "collection-id"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
        
        if (!"Collection" %in% class(private$value)) {
          suppressWarnings({
            coerced = as.character(private$value)
          })
          
          if (length(private$schema$pattern) > 0) {
            if (!grepl(pattern=private$schema$pattern,x=coerced,perl=TRUE)) stop(paste0("The provided regexpr pattern does not match the value: ",private$value))
          }
          
          
          if (is.null(coerced) || 
              is.na(coerced) ||
              length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
          # correct value if you can
          private$value = coerced
        } else {
          coerced = private$value$id
          
          if (is.null(coerced) || 
              is.na(coerced) ||
              length(coerced) == 0) stop(paste0("CollectionId obtained from service is not valid, please contact the openEO service support."))
        }
      } else {
        if (!grepl(pattern=private$schema$pattern,x=private$value,perl=TRUE)) stop(paste0("The provided value does not match the required pattern: ",private$value))
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      if (!"Collection" %in% class(private$value)) {
        return(as.character(private$value))
      } else {
        return(private$value$id)
      }
      
    }
  )
)

# JobId ====
#' JobId class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a jobId on an openeo back-end.
#' 
#' @name JobId
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing the id of a job.
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
      private$schema$subtype = "job-id"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
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
      return(invisible(NULL))
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# UdfRuntime argument ====
#' UdfRuntimeArgument class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the id of an UDF runtime object as obtainable by \code{\link{list_udf_runtimes}}.
#' 
#' @name UdfRuntimeArgument
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing the UDF runtime in a process argument.
NULL

UdfRuntimeArgument = R6Class(
  "udf-runtime",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$subtype = "udf-runtime"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a runtime id."))
        # correct value if you can
        private$value = coerced
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# UdfRuntimeVersion argument ====
#' UdfRuntimeVersionArgument class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the id of a UDF runtime object as obtainable by \code{\link{list_udf_runtimes}}.
#' 
#' @name UdfRuntimeVersionArgument
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} is an argument that expects a UDF runtime version or character as value.
NULL

UdfRuntimeVersionArgument = R6Class(
  "udf-runtime-version",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$subtype = "udf-runtime-version"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into an UDF runtime version string."))
        # correct value if you can
        private$value = coerced
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# UdfCode argument ====
#' UdfCodeArgument class
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the id of an UDF runtime object as obtainable by \code{\link{list_udf_runtimes}}.
#' 
#' @name UdfCodeArgument
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} is an argument that expects an UDF code or a file path.
NULL

UdfCodeArgument = R6Class(
  "udf-code",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
      private$schema$subtype = "udf-code"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
        
        if ("FileFormat" %in% class(private$value)) {
          # what to do?
        } else {
          suppressWarnings({
            coerced = as.character(private$value)
          })
          
          if (is.null(coerced) || 
              is.na(coerced) ||
              length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
          # correct value if you can
          
          private$value = coerced
        }
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      if (is.call(private$value)) {
        return(paste(deparse(private$value),collapse = "\n"))
      } else if (is.character(private$value)) {
        if (file.exists(private$value)) {
          # if valid file path open file and attach
          tryCatch({
            suppressWarnings({
              content = readChar(private$value, file.info(private$value)$size)
              return(content)
            })
            
          }, error = function(e) {
            return(private$value)
          })
          
        } else {
          return(private$value)
        } 
      } else if (self$isEmpty() && !self$isRequired) {
        return(NULL)
      } else if (!is.environment(private$value) && is.na(private$value)) {
        return(NA)
      } else {
        return(as.character(private$value))
      }
    }
  )
)

# ProcessGraphId ====
#' ProcessGraphId
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a process graph Id on an openeo back-end.
#' 
#' @name ProcessGraphId
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing the id of a process graph.
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
      private$schema$subtype = "process-graph-id"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
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
      
      return(invisible(NULL))
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a projection definition based on PROJ.
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
      private$schema$subtype = "proj-definition"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character proj definition."))
        # correct value if you can
        private$value = coerced
      } 
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      return(as.character(private$value))
    }
  )
)

# Bounding Box ====
#' BoundingBox
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a bounding box / extent of an area of 
#' interest. Its value is usually a named list with "west","south","east" and "north". An NA value means an open
#' interval.
#' 
#' @name BoundingBox
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a bounding box / extent.
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
      private$schema$subtype = "bounding-box"
    },
    setValue = function(value) {
      # the value will be a posixct where we just return the time component
      if (is.list(value)) {
        if ("crs" %in% names(value)) {
          crs_value = value[["crs"]]
          if (is.character(crs_value) && grepl(tolower(crs_value),pattern = "^epsg:")) {
            value[["crs"]] = as.integer(gsub(x = crs_value,replacement = "",pattern = "[^0-9]"))
          }
        }
      }
      private$value= value
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
      
      if (length(obj_names) == 0) stop("Bounding box parameter are unnamed. Cannot distinguish between values.") 
      
      # check if west, south, east, north are set and all are numeric values or coercable as numeric
      required_dir_params = c("west","south","east","north")
      if (!all(required_dir_params %in% obj_names)) stop(paste0(
        "Bounding box parameters are missing: ", paste(required_dir_params[!required_dir_params %in% obj_names],collapse = ", ")
      ))
      
      suppressWarnings({
        vals = lapply(private$value[obj_names],as.numeric)
        nas = sapply(vals, is.na)
        
        if (any(nas)) {
          stop("Not all bbox parameters are numeric or can be coerced into numeric automatically: ",paste0(obj_names[nas],collapse = ", "))
        } else {
          private$value[obj_names] = vals
        }
        
      })

      # check if crs is set (either proj string or epsg code)
      if ("crs" %in% obj_names) {
        crs_value = private$value[["crs"]]
        if (!is.integer(crs_value) && !is.numeric(crs_value)) {
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
      
      return(invisible(NULL))
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a boolean / logical.
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
      
      if (length(private$value) > 0 && "ProcessNode" %in% class(private$value)) {
        if (! "boolean" %in% class(private$value$getReturns())) {
          stop("No logical return from ProcessNode.")
        }
        return(invisible(NULL))
      }
      
      if (!is.na(private$value) && !is.logical(private$value)) {
        suppressWarnings({
          coerced = as.logical(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a boolean/logical."))
        # correct value if you can
        private$value = coerced
      }
      
      return(invisible(NULL))
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a date.
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
      private$schema$subtype = "date"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.Date(private$value)) {
        suppressWarnings({
          coerced = as_date(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a date"))
        # correct value if you can
        private$value = coerced
      }
      
      return(invisible(NULL))
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a date with time component.
#' 
#' @import lubridate
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
      private$schema$subtype = "date-time"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.na(private$value) && !is.POSIXct(private$value)) {
        suppressWarnings({
          coerced = as_datetime(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a date time object"))
        # correct value if you can
        private$value = coerced
      }
      
      return(invisible(NULL))
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing the time of a day.
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
      private$schema$subtype = "time"
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
      if (!is.na(private$value) && !is.POSIXct(private$value)) {
        suppressWarnings({
          coerced = strptime(value, format="%H:%M:%SZ")
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a time representation"))
        # correct value if you can
        private$value = coerced
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      return(as.character(format(private$value,format = "%H:%M:%SZ")))
    }
  )
)

# GeoJson ====
#' GeoJson
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a GeoJson object. This class represents geospatial features. 
#' Allowed values are either a list directly convertible into a valid GeoJson or polygon features from package 'sf'. 
#' If sf-objects are used, keep in mind that the objects are projected in  Lat/Long EPSG:4326, unless marked otherwise.
#' It is assumed that the coordinates have a projection. If the crs-object is set and does not match 
#' EPSG:4326, the polygon is transformed accordingly.
#' 
#' @name GeoJson
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing an object in GeoJson.
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
      private$schema$subtype = "geojson"
    },
    
    setValue = function(value) {
      
      if (!.is_package_installed("sf")) {
        warnings("Package sf is not installed but required for GeoJson support.")
      }
      
      if (isNamespaceLoaded("sf")) {
        if (all(c("XY","POLYGON") %in% class(value))) {
          value = sf::st_sfc(value)
        }
        
        if ("sfc_POLYGON" %in% class(value)) {
          value = sf::st_sf(value)
          sf::st_crs(value) = 4326
        } 
        
        if ("sf" %in% class(value)) {
          # since geojson only supports WGS84 we need to make sure that it is that crs
          value = sf::st_transform(value, st_crs(4326))
        }
      }
      
      private$value = value
    }
  ),
  private = list(
    typeCheck = function() {
      if (is.list(private$value)) {
        #if list we assume that the geojson object was created as list
        return(NULL)
      } 
      
      if ("sf" %in% class(private$value)) {
        return(NULL)
      } else {
        stop("Class ",class(private$value)[[1]], " not supported in GeoJson argument")
      }
        
      
    },
    typeSerialization = function() {
      if (is.list(private$value) && all(c("type","coordinates") %in% names(private$value))) {
        return(private$value)
      } else if ("sf" %in% class(private$value)){
        if (!.is_package_installed("geojsonsf")) {
          stop("Package 'geojsonsf' is required for serializing geometries into GeoJson. Please install the package.")
        } else {
          gson = geojsonsf::sf_geojson(private$value)
          return(jsonlite::fromJSON(gson, simplifyVector = FALSE))
        }
      } else {
        stop("Unsupported value type.")
      }
      
      
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing output format options.
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
      private$schema$subtype = "output-format-options"
    }
  ),
  private = list(
    typeCheck = function() {
      if (!self$isEmpty()) {
        if (!is.list(private$value)) {
          stop("Output format options are not a list")
        }
      }
      
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
#' output format of a process unless the process operates within a ProcessGraph on reduced data.  
#' The \code{\link{VectorCube}} behaves comparably, but with underlying spatial feature data.
#' 
#' @name RasterCube
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a raster cube.
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
      private$schema$subtype = "raster-cube"
    }
  ),
  private = list(
    typeCheck = function() {
      # a raster data cube can only be derived by process, e.g. get_collection so this
      # value should be a ProcessNode
      if (! "ProcessNode" %in% class(private$value)) stop("RasterCube is not retreived by process.")
      
      invisible(NULL)
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
#' Inheriting from \code{\link{Argument}} in order to represent a vector cube. This is analogous to
#' the \code{\link{RasterCube}}.
#' 
#' @name VectorCube
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a vector cube.
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
      private$schema$subtype = "vector-cube"
    }
  ),
  private = list(
    typeCheck = function() {
      # a vector data cube can only be derived by process, e.g. get_collection so this
      # value should be a ProcessNode
      if (! "ProcessNode" %in% class(private$value)) stop("VectorCube is not retreived by process.")
      
      invisible(NULL)
    },
    typeSerialization = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serializeAsReferences())
      }
      
      return(as.character(private$value))
    }
  )
)

# ProcessGraphArgument ====
#' ProcessGraphArgument
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a ProcessGraph (prior known as callback). The ProcessGraph operates on reduced data
#' of a data cube. For example reducing the time dimension results in a time series that has to be reduced into a
#' single value. The value of a ProcessGraph is usually a \code{\link{Graph}} with \code{\link{ProcessGraphParameter}} as 
#' added data. Additional information can be found in the openEO API documentation:
#' \itemize{
#'   \item \url{https://api.openeo.org/#section/Processes/Process-Graphs}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$getProcessGraphParameters()}}{returns the available list \code{\link{ProcessGraphParameter}}}
#'   \item{\code{$setProcessGraphParameters(parameters)}}{assigns a list of \code{\link{ProcessGraphParameter}} to the ProcessGraph}
#' }
#' 
#' @section Arguments:
#' \describe{
#'   \item{\code{parameters}}{the \code{\link{ProcessGraphParameter}} list}
#' }
#' 
#' @name ProcessGraphArgument
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a ProcessGraph.
NULL

ProcessGraphArgument = R6Class(
  "ProcessGraphArgument",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$subtype = "process-graph"
    },
    
    setValue = function(value) {
      if ("function" %in% class(value)) {
        # if value is a function -> then make a call with the function and a suitable ProcessGraph
        # parameter
        # create a new graph

        process_collection = private$process$getGraph()

        # probably switch temporarily the graph of the parent process
        # then all newly created process nodes go into the new graph
        private$process$setGraph(process_collection)

        process_graph_parameter = private$parameters

        # find suitable ProcessGraph parameter (mostly array or binary) -> check for length of formals
        # the issue can no longer been resolved automatically
        if (length(formals(value)) != length(process_graph_parameter)) stop("Function parameter do not match ProcessGraph parameter(s)")

        names(process_graph_parameter) = names(formals(value))

        lapply(process_graph_parameter, function(cb){cb$setProcess(private$process)})

        # make call
        final_node = do.call(value,args = process_graph_parameter)

        # then serialize it via the final node

        # assign new graph as value
        private$value = Graph$new(final_node = final_node)
      } else if ("ProcessNode" %in% class(value)) {
        private$value = Graph$new(final_node = value)
      } else if ("Graph" %in% class(value) || is.na(value)) {
        private$value = value
      } else {
        stop("Assigned value for process graph needs to be function, graph or a final process node.")
      }
      
      
      
    },
    setProcess = function(p) {
      private$process = p
      
      lapply(private$parameters, function(cbv) {
        cbv$setProcess(p)
      })
      
      return(invisible(self))
    },
    
    setProcessGraphParameters = function(parameters) {
      private$parameters = parameters
    },
    
    getProcessGraphParameters = function() {
      return(private$parameters)
    }
    
  ),
  private = list(
    parameters = list(),
    
    typeCheck = function() {
      # check the value (graph) for the same ProcessGraph parameters (ProcessGraphParameters)
      if (!"Graph" %in% class(private$value)) stop("The value of a ProcessGraph argument is usually a graph.")
      
      errors = private$value$validate()
      
      if (any(errors != TRUE)) {
        stop(paste("Errors in subgraph:",paste(errors,collapse=";")))
      }

      return(invisible(NULL))
      
    },
    
    typeSerialization = function() {
      if (is.environment(private$value) && "serialize" %in% names(private$value)) {
        return(list(
          process_graph = private$value$serialize()))
      } 
    }
  )
)

# ProcessGraphParameter ====
#' ProcessGraphParameter
#' 
#' Inheriting from \code{\link{Argument}} in order to represent the available data within a ProcessGraph graph.
#' Additional information can be found in the openEO API documentation:
#' \itemize{
#'   \item \url{https://api.openeo.org/#section/Processes/Process-Graphs}
#' }
#' 
#' @name ProcessGraphParameter
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a ProcessGraph value.
NULL

# in case the ProcessGraphParameter is an array - which it will be in most cases - we have to store
# process nodes for array subsetting in the object with its index. This should be done to 
# reuse the results of previous steps
ProcessGraphParameter = R6Class(
  "ProcessGraphParameter",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),
                        description=character(),
                        type=character(),
                        subtype=character(),
                        default=character(),
                        required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = type
      private$schema$subtype = subtype
      private$default = default
    },
    print = function() {
      cat(toJSON(self$serialize(),pretty = TRUE, auto_unbox = TRUE))
      invisible(self)
    },
    adaptType = function(fromParameter) {
      if (is.list(fromParameter) && length(fromParameter) == 1) {
        fromParameter = fromParameter[[1]]
        private$schema = fromParameter$getSchema()
        private$default = fromParameter$getDefault()
        private$required = fromParameter$isRequired
        private$nullable = fromParameter$isNullable
      } else {
        stop("Not considered yet")
      }
      
      invisible(self)
    }
  ),
  private = list(
    typeSerialization = function() {
      if (self$isEmpty()) {
        return(list(from_parameter=private$name))
      } else {
        
        if ("Argument" %in% class(private$value)) {
          value_serialization = self$getValue()$serialize()
        } else if ("FileFormat" %in% class(private$value)) {
          value_serialization = private$value$name
        } else {
          value_serialization = self$getValue()
        }
        
        return(value_serialization)
      }
    }
  )
)

setOldClass(c("ProcessGraphParameter","Argument","Parameter","R6"))

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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a single valued array.
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
      private$schema$subtype = format
      
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
      
      if (!is.environment(value) && length(value) > 0) {
        private$value = lapply(value, function(x, pc) {
          .checkMathConstants(x,pc)
        }, pc = process_collection)
      } else {
        private$value = value
      }
    }
  ),
  private = list(
    typeCheck = function() {
      itemType = private$schema$items$type
      if (itemType == "any") {
        # this can be anything so we shift the responsibility to the back-end
        return(invisible(NULL)) 
      }
      
      if (length(private$schema$minItems) == 1 && 
          length(private$value) < private$schema$minItems) {
        stop(paste0("Minimum items are not achieved. Found ",length(private$value)," items of minimal ",private$schema$minItems," items."))
      }
      if (length(private$schema$maxItems) == 1 && 
          length(private$value) > private$schema$maxItems) {
        stop(paste0("More items than maximum. Found ",length(private$value)," items of maximal ",private$schema$maxItems," items."))
      }

      if (itemType == "array") {
        # just check the first layer, everything else would be nice, but is no more in our responsibility
        if ("ProcessGraphParameter" %in% class(private$value)) {
          if (length(private$value$getSchema()$type) > 0 && 
              private$value$getSchema()$type == "array")
            if (length(private$value$getSchema()$items$type) > 0) {
              if (private$value$getSchema()$items$type == private$schema$items$type) {
                return()
              }
            } else {
              stop("Selected ProcessGraphParameter is an array, but has a different item type.")
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
        
        if (!allOK) stop("At least one of the nested arrays has an invalid item type or the min/max constraint was triggered.")
        
      } else {
        if (!"ProcessGraphParameter" %in% class(private$value[[1]])) {
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
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      if (!is.environment(self$getValue()) && (length(self$getValue()) == 0 || is.na(self$getValue()))) {
        return(NA)
      }
      
      lapply(self$getValue(), function(value) {
        
        if ("ProcessNode" %in% class(value)) return(value$serializeAsReference())
        
        if ("Argument" %in% class(value)) return(value$serialize())
        
        return(value)
      })
    })
)

# Kernel ====
#' Kernel
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a 2-dimensional array of weights applied 
#' to the x and y (spatial) dimensions of the data cube. The inner level of the nested array is aligned to the x-axis and 
#' the outer level is aligned to the y-axis. Each level of the kernel must have an uneven number of elements.
#' 
#' @name Kernel
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a Kernel.
NULL

Kernel = R6Class(
  "kernel",
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
      private$schema$subtype = "kernel"
      private$schema$items = items
    }
  ),
  private = list(
    typeSerialization = function() {
      # might not be really required
      
      private$value
    },
    typeCheck = function() {
      if (!self$isEmpty()) {
        if (!is.matrix(private$value) || !is.data.frame(private$value) || !is.array(private$value)) {
          stop("Kernel value is neither matrix nor data.frame")
        } else {
          dims = dim(kernel)
          
          if (!length(dims) == 2) {
            stop("Kernel has more or less than two dimensions")
          }
          
          if (!all(dims %% 2 == 1)) {
            stop("One or more kernel dimension have an even number of elements")
          }
          
          invisible(NULL)
        }
      }
    }
  )
)

#TemporalInterval ====
#' TemporalInterval
#' 
#' Inheriting from \code{\link{Argument}} in order to represent a temporal interval. Open interval borders are
#' denoted by NA. Exactly two objects form the temporal interval.
#' 
#' @name TemporalInterval
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a temporal interval.
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
      private$schema$subtype = "temporal-interval"
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a list of temporal intervals.
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
      private$schema$subtype = "temporal-intervals"
      private$schema$items = items
    }
  )
)

# MetadataFilter ====
#' MetadataFilter
#' 
#' Inheriting from \code{\link{ProcessGraphArgument}} in order to represent a list of functions that is internally 
#' interpreted into \code{\link{Process}} objects.
#' 
#' @examples 
#' \dontrun{
#' # define filter statement
#' filter = list(
#'    "eo:cloud_cover" = function(x) x >= 0 & x < 50, 
#'    "platform" = function(x) x == "Sentinel-2A"
#' )
#' 
#' # setting the arguments is done via the process graph building with of 'processes()'
#' }
#' 
#' @name MetadataFilter
#' 
#' @seealso \code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing a list of \code{\link{Process}} in order to filter for collections.
NULL

MetadataFilter <- R6Class(
  "metadata-filter",
  inherit=ProcessGraphArgument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "object"
      private$schema$subtype = "metadata-filter"
    },
    setValue = function(value) {
      if (length(value) == 0 || is.na(value)) {
        private$value = value
      } else if (is.list(value)) {
        
        node_names = names(value) # TODO add something if missing or stop
        # set parameter that is stored in this class...
        v = lapply(value,function(FUN, private) {
          
           if (is.function(FUN)) {
            process_collection = private$process$getGraph()
            
            # probably switch temporarily the graph of the parent process
            # then all newly created process nodes go into the new graph
            private$process$setGraph(process_collection)
            
            process_graph_parameter = private$parameters
            
            # find suitable ProcessGraph parameter (mostly array or binary) -> check for length of formals
            # the issue can no longer been resolved automatically
            if (length(formals(FUN)) != length(process_graph_parameter)) stop("Function parameter do not match ProcessGraph parameter(s)")
            
            names(process_graph_parameter) = names(formals(FUN))
            
            lapply(process_graph_parameter, function(cb){cb$setProcess(private$process)})
            
            # make call
            final_node = do.call(FUN,args = process_graph_parameter)
            
            # assign new graph as value
            as(final_node,"Process")
           } else {
             stop("Value for the metadata filter needs to be a list of functions.")
          }
          
          
          
        },private=private)
        
        names(v) = node_names
        
        private$value = v
      } else {
        stop("Value for the metadata filter needs to be a list of functions.")
      }
    }
  ),
  private = list(
    typeCheck = function() {
      if (!self$isEmpty()) {
        length_0 = length(private$value) == 0
        # named list of functions
        is_named = length(names(private$value)) > 0
        is_list = is.list(private$value)
        all_graphs = all(sapply(private$value,function(o)"Process" %in% class(o)))
        
        if (!length_0 && !is_named && !is_list && !all_graphs) {
          stop("value could not be parsed into a named list of Process")
        }
      }
      
      return(invisible(NULL))
    },
    typeSerialization = function() {
      nn = names(private$value)
      ll = lapply(private$value,function(o)o$serialize())
      names(ll) = nn
      return(ll)
    }
  )
)

# AnyOf ====
#' AnyOf
#' 
#' Inheriting from \code{\link{Argument}} in order to represent an argument choice object. Multiple
#' types can be stated, but at least one data type has to be picked. In a JSON-schema this is often used to make
#' objects nullable - meaning that they allow NULL as value. The AnyOf parameter is resolved into a simple nullable argument
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
#' \code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
#' \code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
#' \code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
#' \code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
#' \code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
#' \code{\link{UdfRuntimeVersionArgument}},\code{\link{TemporalIntervals}}, \code{\link{MetadataFilter}}
#' 
#' @return Object of \code{\link{R6Class}} representing an argument choice object.
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
    setProcess = function(p) {
      private$process = p
      
      lapply(private$parameter_choice, function(choice) {
        choice$setProcess(p)
      })
      
      return(invisible(self))

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
          length(cb$getProcessGraphParameters())
        })
        
        choice_index = unname(which(number_of_params == length(signature)))
        
        if (length(choice_index) == 0) {
          stop("Cannot match function to any of the ProcessGraph parameter.")
        }
        choice = private$parameter_choice[[choice_index]]
        
        #resolve anyof parameter
        self$getProcess()$setParameter(name = self$getName(),value = choice)
        choice$setName(self$getName())
        choice$setProcess(private$process)
        choice$setValue(value)
        
        return(self)
      }
      
      if ("ProcessGraph" %in% class(value)) {
        # This is mostly for ProcessGraphs
        arg_allowed = any(sapply(private$parameter_choice, function(argument) {
          all(length(setdiff(class(argument),class(value))) == 0,
              length(setdiff(class(value),class(argument))) == 0
          )
        }))
        
        if(!arg_allowed) stop("Cannot assign ",class(value)[[1]], " as value. Not allowed.")
        
        private$value = value
        return(self)
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
        
        tryCatch({
          if (!any(validated)) stop("Cannot match the value to any of the parameter selection (AnyOf)")
          
          private$value = unname(choice_copies[validated])[[1]] # pick the first match
          private$value$setValue(value)
        }, error = function(e) {
          message(e$message)
        })
        
        return(self)
      }
      
      
      
    },
    getValue = function() {

      # best case only one had survived the selection, if not throw an error?
      # or return the first result
      if (is.null(private$value)) return(private$value)
      
      if (is.list(private$value)) {
        if (length(private$value)==1) {
          return(private$value[[1]])
        }
        
        return(private$value[[1]])
      } else {
        return(private$value)
      }
    },
    
    getChoice = function() {
      param_copies = lapply(private$parameter_choice,function(choice){
        choice$clone(deep=TRUE)
      })
      return(param_copies)
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
    
    typeCheck = function() {},
    typeSerialization = function() {
      if (length(self$getValue()) == 0) {
        return(NULL)
      } else {
        val = self$getValue()
        
        if (!is.list(val)) {
          val = list(val)
        }
        
        val = lapply(val, function(v) {
          if (any(c("Graph","Argument") %in% class(v))) {
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
    }
  )
)

# parse functions ----
findParameterGenerator = function(schema) {
  # adapt this if I add some parameter/argument
  # ProcessGraphParameter are not listed since they are created at the graph (as "variables")
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
                               ProjDefinition,
                               OutputFormat,
                               OutputFormatOptions,
                               ProcessGraphArgument,
                               Array,
                               Kernel,
                               Date,
                               DateTime,
                               TemporalInterval,
                               TemporalIntervals,
                               Time,
                               URI,
                               UdfRuntimeArgument,
                               UdfRuntimeVersionArgument,
                               UdfCodeArgument,
                               MetadataFilter)
  
  # resolve the any parameter (no specification)
  if (length(schema$type) == 0 && length(schema$subtype) == 0) {
    return(list(Argument))
  }
  
  matches = unlist(lapply(parameter_constructor, function(constructor){
    if(constructor$new()$matchesSchema(schema)) constructor
  }))
  if (is.null(matches) || length(matches) == 0) matches = list(Argument) # if we don't find anything simply use this, since it is not restricted
  
  return(matches)
}

processFromJson=function(json) {
  if (is.null(json$summary)) json$summary = character()
  
  
  tryCatch({
    #map parameters!
    parameter_names = sapply(json$parameters, function(p)p$name)
    
    if (length(parameter_names) > 0) {
      parameters = lapply(
        json$parameters, function(pdef) {
          # set param if it is contained in the schema
          param = parameterFromJson(pdef)
          
          return(param)
        }
      )
      
      names(parameters) = parameter_names
    } else {
      parameters = list()
    }
    
    if (length(json$process_graph) > 0) {
      graph = parse_graph(json=json)  
    } else {
      graph = NULL
    }
    
    
    Process$new(id=json$id,
                description = json$description,
                summary=json$summary,
                parameters = parameters,
                returns = json$returns,
                process_graph = graph)
  }, error = function(e) {
    warning(paste0("Invalid process description for '",json$id,"'"))
    NULL
  })
  
}

parameterFromJson = function(param_def) {
  if (length(param_def$schema) == 0) {
    # an empty schema means ANY value is allowed
    arg = Argument$new()
    
    if (length(param_def$description) > 0) {
      arg$setDescription(param_def$description)
    }
    
    return(arg)
  }
    
  # if it is no unnamed object list, then box it
  if (length(names(param_def$schema)) > 0) {
    if (!is.null(param_def$schema$type) && is.list(param_def$schema$type)) {
      param_def$schema = lapply(param_def$schema$type, function(type,original_param_schema) {
        original_param_schema$type = type
        return(original_param_schema)
      },original_param_schema = param_def$schema)
    } else {
      param_def$schema = list(param_def$schema)
    }
    
  }
  
  #special case a simple type + null, which mean a type that is a list and schema not
  # then dissolve the parameter into multiple instances
  
  
  
  # now we have a list over which we can lapply
  nullable = sapply(param_def$schema, function(schema) {
    nullable_single = !is.null(schema$type) && schema$type == "null"
    
    nullable_array = !is.null(schema$type) && schema$type == "array" && 
      length(schema$items$type) == 1 && schema$items$type == "null"
    
    return(nullable_single || nullable_array)
  })
  
  param_nullable = any(nullable)
  
  # delete the null parameter
  if (param_nullable) {
    param_def$schema[[which(nullable)]] = NULL
  }
  
  is_choice = length(param_def$schema) > 1
  
  
  #create a list of parameters / find from schema
  params = lapply(param_def$schema, function(schema) {
    # this will be the normal case for simple schemas?
    gen=findParameterGenerator(schema)[[1]]
    param = gen$new()
    
    if (length(schema$pattern) != 0) {
      param$setPattern(schema$pattern)
    }
    
    if (length(schema$enum) != 0) {
      param$setEnum(schema$enum)
    }
    if ("metadata-filter" %in% class(param)) {
      if ("additionalProperties" %in% names(schema)) {
        schema = append(schema,schema[["additionalProperties"]])
        schema[["additionalProperties"]] = NULL
      }
    } 
    
    if ("ProcessGraphArgument" %in% class(param)) {
      # iterate over all ProcessGraph parameters and create ProcessGraphParameters, but name = property name (what the process exports to ProcessGraph)
      # value has to be assigned by user, then switch name and value during serialization
      pg_params = lapply(schema$parameters, function(param_json) {
        if (is.null(param_json$schema[["subtype"]])) param_json$schema[["subtype"]] = character()
        
        cb = ProcessGraphParameter$new(name = param_json$name,
                               description = param_json$description,
                               type = param_json$schema[["type"]],
                               subtype = param_json$schema[["subtype"]],
                               required = TRUE)
        
        if(!is.null(param_json$schema[["pattern"]])) cb$setPattern(param_json$schema[["pattern"]])
        
        if (is.null(param_json$optional)) param_json$optional = FALSE
        
        cb$isRequired = isFALSE(param_json$optional)
        
        return(cb)
      })
      pg_param_names = sapply(schema$parameters,function(p)p$name)
      names(pg_params) = pg_param_names
      
      param$setProcessGraphParameters(pg_params)
    }
    
    if ("array" %in% class(param)) {
      if (!"kernel" %in% class(param)) {
        param$setItemSchema(schema$items)
      } else {
        param$setItemSchema(schema$items)
      }
      
    }
    return(param)
  })
  
  # if choice then create an anyOf
  if (is_choice) {
    #build an anyOf
    param = AnyOf$new(parameter_list = params)
  } else {
    param = params[[1]]
  }
  
  
  # in general also reolve null cases
  param$isNullable = param_nullable
  param$setDefault(param_def$default)
  param$setName(param_def$name)
  param$setDescription(param_def$name)
  
  if (is.null(param_def$optional)) param_def$optional = FALSE
  
  param$isRequired = isFALSE(param_def$optional)
  
  pattern = param_def$schema$pattern
  if (!is.null(pattern)) {
    param$setPattern(pattern)
  }
  
  return(param)
  
}
