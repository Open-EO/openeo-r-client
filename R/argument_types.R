# Parameter ====
# should also be abstract
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
    matchesSchema = function(schema) {
      sel = c("type","format")
      return(setequal(private$schema[sel], schema[sel]))
    },
    getSchema = function() {
      return(private$schema)
    }
  ),
  private = list(
    name=character(),
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
    getParameterOrder = function() {
      return(private$parameter_order)
    },
    serialize = function() {
      # for format specific conversion overwrite this by children
      return(private$value)
    },
    validate = function(node_id=NULL) {
      tryCatch(
        {
          private$checkRequiredNotSet()
          
          if (!self$isRequired() && 
              !is.environment(private$value) && 
              self$isEmpty()) {
            
          } else {
            private$typeCheck()
          }
          
          invisible(NULL)
        }, error = function(e) {
          if (!is.null(node_id)) node_id = paste0("[",node_id,"] ")
          
          message = paste0(node_id,"Parameter '",private$name,"': ",e$message)
          
          return(message)
        }
      )
    },
    
    isEmpty = function() {
      return(!is.environment(private$value) && (is.null(private$value) || 
               is.na(private$value) ||
               length(private$value) == 0))
    }
  ),
  private = list(
    value=NULL,
    
    checkRequiredNotSet = function() {
      if (private$required && 
          !is.environment(private$value) &&
          self$isEmpty()) stop("Argument is required, but has not been set.")
    },
    
    typeCheck = function() {
      # implemented / overwritten by children
    },
    
    checkMultiResults = function() {
      if ("ProcessNode" %in% class(private$value)) {
        returns = private$value$getProcess()$getReturns()
        
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

# Integer ====
Integer = R6Class(
  "integer",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "integer"
    },
    serialize = function() {
      return(as.integer(private$value))
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
    }
  )
)

# EPSG-Code ====
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
    },
    serialize = function() {
      return(as.integer(private$value))
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
    }
  )
)

# Number ====
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
    serialize = function() {
      return(as.numeric(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.numeric(private$value)) {
        suppressWarnings({
          coerced = as.numeric(private$value)
        })
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a number."))
        # correct value if you can
        private$value = coerced
      }
    }
  )
)

# String ====
String = R6Class(
  "string",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "string"
    },
    serialize = function() {
      return(as.character(private$value))
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
    }
  )
)

# Null ====
Null = R6Class(
  "null",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "null"
      private$value = NULL
    },
    serialize = function() {
      return(private$value)
    },
    setValue = function(value) {
      
    }
  ),
  private = list(
    typeCheck = function() {

    }
  )
)

# Output Format ====
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
    },
    serialize = function() {
      return(as.character(private$value))
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
    }
  )
)

# CollectionId ====
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
    },
    serialize = function() {
      return(as.character(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (!grepl(pattern=private$schema$pattern,x=private$value)) stop(paste0("The provided regexpr pattern does not match the value: ",private$value))
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      } else {
        if (!grepl(pattern=private$schema$pattern,x=private$value)) stop(paste0("The provided value does not match the required pattern: ",private$value))
      }
    }
  )
)

# JobId ====
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
    },
    serialize = function() {
      return(as.character(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (!grepl(pattern=private$schema$pattern,x=private$value)) stop(paste0("The provided regexpr pattern does not match the value: ",private$value))
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      } else {
        if (!grepl(pattern=private$schema$pattern,x=private$value)) stop(paste0("The provided value does not match the required pattern: ",private$value))
      }
    }
  )
)

# ProcessGraphId ====
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
    },
    serialize = function() {
      return(as.character(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      if (!is.character(private$value)) {
        suppressWarnings({
          coerced = as.character(private$value)
        })
        
        if (!grepl(pattern=private$schema$pattern,x=private$value)) stop(paste0("The provided regexpr pattern does not match the value: ",private$value))
        
        if (is.null(coerced) || 
            is.na(coerced) ||
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a character string."))
        # correct value if you can
        private$value = coerced
      } else {
        if (!grepl(pattern=private$schema$pattern,x=private$value)) stop(paste0("The provided value does not match the required pattern: ",private$value))
      }
    }
  )
)

# Proj-Definition ====
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
    },
    serialize = function() {
      return(as.character(private$value))
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
    }
  )
)

# Bounding Box ====
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
    },
    serialize = function() {
      return(private$value)
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
        vals = lapply(private$value[required_dir_params],as.numeric)
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
    }
  )
)

# Boolean ====
Boolean = R6Class(
  "boolean",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),description=character(),required=FALSE) {
      private$name = name
      private$description = description
      private$required = required
      private$schema$type = "boolean"
    },
    serialize = function() {
      return(as.logical(private$value))
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
    }
  )
)

# Date ====
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
    },
    serialize = function() {
      return(as.character(format(private$value,format = "%Y-%m-%d")))
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
    }
  )
)

# DateTime ====
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
    },
    serialize = function() {
      return(as.character(format(private$value,format = "%Y-%m-%dT%H:%M%SZ")))
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
    }
  )
)

# Time ====
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
    serialize = function() {
      return(as.character(format(private$value,format = "%H:%M:%SZ")))
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
    }
  )
)

# GeoJson ====
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
    },
    serialize = function() {
      return(as.list(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      #TODO implement! object == list in R
    }
  )
)

# OutputFormatOptions ====
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
    },
    serialize = function() {
      return(as.list(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      #TODO implement! object == list in R
    }
  )
)

# ProcessGraphVariables ====
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
    },
    serialize = function() {
      return(as.list(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      #TODO implement! object == list in R
    }
  )
)

# RasterCube ====
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
    },
    serialize = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serialize())
      }
      
      return(as.character(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      # a raster data cube can only be derived by process, e.g. get_collection so this
      # value should be a ProcessNode
      if (! "ProcessNode" %in% class(private$value)) stop("RasterCube is not retreived by process.")
      
      private$checkMultiResults()
      
      # if (! "raster-cube" %in% class(private$value$getProcess()$getReturns())) {
      #   stop("The stated process does not return a RasterCube")
      # }
    }
  )
)

# VectorCube ====
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
    },
    serialize = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serialize())
      }
      
      return(as.character(private$value))
    }
  ),
  private = list(
    typeCheck = function() {
      # a vector data cube can only be derived by process, e.g. get_collection so this
      # value should be a ProcessNode
      if (! "ProcessNode" %in% class(private$value)) stop("VectorCube is not retreived by process.")
      
      private$checkMultiResults()
      # if (! "vector-cube" %in% class(private$value$getProcess()$getReturns())) stop("The stated process does not return a VectorCube")
    }
  )
)

# Callback ====
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
      if (! "ProcessNode" %in% class(value)) stop("Callback function is no Process / ProcessNode")
      
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
    parameters = list()
  )
)

# CallbackValues ====
# The value / binding that the process submits to a process in the callback graph, maybe also referred as a Promise:
# the process that calls a callback promises to submit this value
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
    
    serialize = function() {
      res = list(from_argument=private$value)
      return(res)
    }
  ),
  private = list()
)

# Array ====
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
    
    serialize = function() {
      return(
        lapply(private$value, function(value) {
          value
        })
      )
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
      if (!"type" %in% names(value)) value[["type"]]="any"
      
      if (is.null(value[["minItems"]])) value[["minItems"]] = integer()
      if (is.null(value[["maxItems"]])) value[["maxItems"]] = integer()
      
      private$schema$items = value
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
        allOK = switch(itemType,
                       string = all(sapply(private$value,is.character)),
                       number = all(sapply(private$value,is.numeric)),
                       integer = all(sapply(private$value,is.integer)),
                       boolean = all(sapply(private$value,is.logical))
        )
        # if allOK == null then it was not checked
        if (!is.null(allOK) && !allOK) {
          stop(paste0("At least one element in the array is not of type: ",itemType))
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
    })
)

# Kernel ====
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
    }
  )
)

#TemporalIntervals ====
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

# parse functions ----
findParameterGenerator = function(schema) {
  # TODO adapt this if I add some parameter/argument
  
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
                               Time,
                               Null)
  
  matches = unlist(lapply(parameter_constructor, function(constructor){
    if(constructor$new()$matchesSchema(schema)) constructor
  }))
  
  if (is.null(matches) || length(matches) == 0) matches = list(Argument) # if we don't find anything simply use this, since it is not restricted
  
  return(matches)
}

parameterFromJson = function(param_def) {
  if (is.null(param_def$schema$format)) param_def$schema$format = character()
  if (is.null(param_def$required)) param_def$required = FALSE
  
  if (is.null(param_def$schema$anyOf)) {
    gen=findParameterGenerator(param_def$schema)[[1]]
    param = gen$new(name=param_def$name, description=param_def$description,required = param_def$required)
  } else {
    param = lapply(
      #TODO create anyOf object
      
      param_def$schema$anyOf,
      function(anyOf_schema) {
        param_copy = param_def
        param_copy$schema = anyOf_schema
        return(parameterFromJson(param_copy))
      }
    )
  }
  
  
  if ("callback" %in% class(param)) {
    # iterate over all callback parameters and create CallbackParameters, but name = property name (what the process exports to callback)
    # value has to be assigned by user, then switch name and value during serialization
    
    cb_params = lapply(names(param_def$schema$parameters), function(param_name) {
      param_json = param_def$schema$parameters[[param_name]]
      if (is.null(param_json[["format"]])) param_json[["format"]] = character()
      
      # param_type = findParameterGenerator(param_json[c("type","format")])$new()
      
      
      
      cb = CallbackValue$new(name = param_json$name,
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
}