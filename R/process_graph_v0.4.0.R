library(R6)
library(jsonlite)
library(lubridate)
# definitions ----

# Graph ====
Graph = R6Class(
  "Graph",
  lock_objects = FALSE,
  public = list(
    initialize = function(processes) {
      if (!is.list(processes)) stop("Processes are not provided as list")
      
      for (index in 1:length(processes)) {
        
        pid = processes[[index]]$getId()
        function_formals = processes[[index]]$getFormals()
        
        f = function() {}
        formals(f) = function_formals
        
        # probably do a deep copy of the object
        # for the body we have the problem that index is addressed as variable in the parent environment. This
        # causes a problem at call time, where index is resolve and this means that usually the last element
        # of the list will be used as process all the time -> solution: serialize index, gsub on quote, make "{" as.name
        # and then as.call
        body(f) = quote({
          process = processes[[index]]$clone(deep=TRUE)
          # find new node id:
          node_id = .randomNodeId(process$getId(),sep="_")
          
          while (node_id %in% names(private$nodes)) {
            node_id = .randomNodeId(process$getId(),sep="_")
          }
          
          #map given parameter of this function to the process parameter / arguments and set value
          arguments = process$parameters
          # parameter objects should be updated directly, since there is a real object reference
          this_param_names = names(formals())
          
          # used match.call before, but it seem that it doesn't resolve the pipe - it is something like data = .
          this_arguments = lapply(this_param_names, function(param) get(param))
          names(this_arguments) = this_param_names
          
          lapply(names(this_arguments), function(param_name, arguments){
            call_arg = this_arguments[[param_name]]
            arguments[[param_name]]$setValue(call_arg)
          }, arguments = arguments)
          
          # special case: value is of type Argument
          
          node = ProcessNode$new(node_id = node_id,process=process)
          entry = list(node)
          names(entry) = node_id
          
          private$nodes = append(private$nodes,entry)
          
          return(node)
        })
        # replace index with the actual number!
        tmp = gsub(body(f),pattern="index",replacement = eval(index))
        body(f) = as.call(c(as.name(tmp[1]),parse(text=tmp[2:length(tmp)])))
        
        # register the ProcessNode creator functions on the Graph class
        self[[pid]] = f
      }
    },
    
    getNodes = function() {
      return(private$nodes)
    },
    
    clean = function() {
      # run through the nodes and clean them
      
      # start at the end -> if multiple end nodes throw an error
      suppressMessages({
        endnode = self$getFinalNode()
      })
      
      
      if (is.null(endnode)) stop("No final node defined in this graph. Please set a final node.")
      
      
      # traverse over nested process nodes and extract their node ids from the final node
      final_path = private$extractUsedNodeIds(endnode)
      
      
      unvisitedNodes = setdiff(names(private$nodes),final_path)
      map = lapply(unvisitedNodes, function(node_id) {
        return(unname(private$extractUsedNodeIds(self$getNode(node_id))))
      })
      
      # now we need to check the remaining nodes if they connect in their downward traversion to an already visited node
      # therefore list also the ids again
      # remove found ids from temporary list
      removeables = character()
      if (length(map) > 0) {
        for (index in 1:length(map)) {
          if (any(map[[index]] %in% final_path)) {
            # they are connected to the final_path -> add new elements to final_path
            final_path = union(setdiff(map[[index]],final_path),final_path)
          } else {
            # add to removeables
            removeables = union(map[[index]],removeables)
          }
        }
        
        removeables = unique(removeables)
        
        # now, the remaining nodes are not connected -> remove them from private$nodes
        private$nodes[removeables] = NULL
      }
      
      
      invisible(self)
    },
    
    serialize = function() {
      # iterate over all nodes and serialize their process (not the node it self, since this serializes it as argument)
      # before clean the nodes, remove those that are not connected
      self$clean()
      
      result = lapply(private$nodes, function(node) {
        return(node$getProcess()$serialize())
      })
      names(result) = names(private$nodes)
      
      return(result)
    },
    
    validate = function() {
      tryCatch({
        self$clean() # only test that what has to be used
        
        # for each process node call their processes parameters validate function
        return(unlist(lapply(private$nodes, function(node) {
          node$getProcess()$validate(node_id=node$getNodeId())
        })))
        
      }, error = function(e){
        message(e$message)
      })
      
      
      
      
    },
    
    getNode = function (node_id) {
      private$assertNodeExists(node_id)
      return(private$nodes[[node_id]])
    },
    
    removeNode = function(node_id) {
      private$assertNodeExists(node_id)
      private$nodes[[node_id]] = NULL
      
      return(TRUE)
    },
    
    getFinalNode = function() {
      if (length(private$final_node_id) == 0) {
        message("No final node set in this graph.")
        invisible(NULL)
      } else {
        return(private$nodes[[private$final_node_id]])
      }
      
      
    },
    setFinalNode = function(node) {
      if ("ProcessNode" %in% class(node)) {
        node = node$getNodeId()
      }
      if (is.null(node) || !is.character(node)) {
        message("Node ID is not a character / string.")
        return(FALSE)
      }
      private$assertNodeExists(node_id = node)
      
      private$final_node_id = node
      return(TRUE)
    },
    
    setArgumentValue = function(node_id, parameter, value) {
      private$assertNodeExists(node_id)
      
      node = private$nodes[[node_id]]
      params = node$getProcess()$parameters
      
      if (! parameter %in% names(params)) stop(paste0("Cannot find parameter '",parameter,"' for process '",node_id,"'"))
      
      params[[parameter]]$setValue(value)
      return(params[[parameter]]$validate()) # TODO decide if this is useful
    }
  ),
  private = list(
    nodes = list(),
    final_node_id = character(),
    
    assertNodeExists = function(node_id) {
      if (! node_id %in% names(private$nodes)) stop(paste0("Cannot find node with id '",node_id,"' in this graph."))
    },
    
    extractUsedNodeIds = function(node) {
      nodeParams = unlist(lapply(node$getProcess()$parameters, function (param) {
        if ("ProcessNode" %in% class(param$getValue())) return(param$getValue())
        
        return(NULL)
      }))
      
      if (is.null(nodeParams)) return(node$getNodeId())
      if (length(nodeParams) == 0) return(node$getNodeId())
      
      return(c(node$getNodeId(),sapply(nodeParams,private$extractUsedNodeIds)))
      
    }
  )
)

# Process ====
Process = R6Class(
  "Process",
  public=list(
    initialize = function(id,parameters,description=character(), summary = character(), parameter_order=character(),returns) {
      private$id = id
      private$description = description
      private$summary=summary
      private$parameter_order=parameter_order
      
      if (! is.list(parameters)) stop("Parameters are not provided as list")
      
      parameter_names = sapply(parameters, function(p)p$getName())
      names(parameters) = parameter_names
      
      
      self$parameters = parameters
      private$returns = returns
    },
    
    getId = function() {
      return(private$id)
    },
    
    getParameters = function() {
      return(private$parameters)
    },
    
    getReturns = function() {
      return(private$returns)
    },
    
    getFormals = function() {
      #TODO set also default values
      result = rep(NA,length(private$parameters))
      names(result) = names(private$parameters)
      
      return(result)
    },
    setParameter= function(name,value) {
      if (!name %in% names(private$parameters)) stop("Cannot find parameter")
      
      private$parameters[[name]]$setValue(value)
    },
    getParameter = function(name) {
      if (!name %in% names(private$parameters)) stop("Cannot find parameter")
      
      return(private$parameters[[name]])
    },
    serialize = function() {
      serializedArgList = lapply(private$parameters, 
                                 function(arg){
                                   if(!arg$isEmpty()) arg$serialize()
                                 })
      
      serializedArgList[sapply(serializedArgList,is.null)] = NULL
      
      return(
        list(process_id=private$id,
             arguments = serializedArgList
        )
      )
    },
    validate = function(node_id=NULL) {
      return(unname(unlist(lapply(private$parameters,function(arg, node_id){
        arg$validate(node_id = node_id)
      },node_id = node_id))))
    }
  ),
  active = list(
    parameters = function(value) {
      if (missing(value)) {
        return(private$.parameters)
      } else {
        private$.parameters = value
      }
      
    }
  ),
  
  private=list(
    id = character(),
    returns = NULL, # the object that is returend Parameter or Argument
    # parameters = list(), # named list! this is made sure of in constructor
    parameter_order = character(), # a vector of string corresponding to the parameter order
    summary = character(),
    description = character(),
    categories = character(), # probably more than 1 string -> so it is a vector of strings
    examples = list(), # a list of objects with arguments: <list>, returns: <value>
    .parameters = list(),
    
    deep_clone = function(name, value) {
      if (is.environment(value) && !is.null(value$`.__enclos_env__`)) {
        return(value$clone(deep = TRUE))
      }
      
      # also check if it is a list of R6 objects
      if (name == "parameters") {
        
        new_list = list()
        
        for (list_name in names(value)) {
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
      
      
      value
    }
  )
)

# ProcessNode ====
ProcessNode = R6Class(
  "ProcessNode",
  public = list(
    
    initialize = function(node_id=character(),process) {
      private$node_id = node_id
      
      if (!"Process" %in% class(process)) stop("Process is not of type 'Process'")
      private$process = process
    },
    
    getProcess = function() {
      return(private$process)
    },
    
    getNodeId = function() {
      return(private$node_id)
    },
    serialize = function() {
      return(list(
        from_node=private$node_id
      ))
    }
  ),
  
  private = list(
    node_id = character(),
    process = NULL
  )
)

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
      properties = list(), # potential callback parameter
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
          
          invisible(TRUE)
        }, error = function(e) {
          if (!is.null(node_id)) node_id = paste0("[",node_id,"] ")
          
          message = paste0(node_id,"Parameter '",private$name,"': ",e$message)
          
          return(message)
        }
      )
    },
    
    isEmpty = function() {
      return(is.null(private$value) || 
               is.na(private$value) ||
               length(private$value) == 0)
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
            length(coerced) == 0) stop(paste0("Value '", private$value,"' cannot be coerced into a boolean."))
        # correct value if you can
        private$value = coerced
      }
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
      
      if (! "raster-cube" %in% class(private$value$getProcess()$getReturns())) stop("The stated process does not return a RasterCube")
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
      # a raster data cube can only be derived by process, e.g. get_collection so this
      # value should be a ProcessNode
      if (! "ProcessNode" %in% class(private$value)) stop("RasterCube is not retreived by process.")
      
      if (! "vector-cube" %in% class(private$value$getProcess()$getReturns())) stop("The stated process does not return a VectorCube")
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

.randomNodeId = function(name,n = 1,...) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste(name,paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)),...)
}

# parse functions ----
findParameterGenerator = function(schema) {
  # TODO adapt this if I add some parameter/argument
  
  parameter_constructor = list(Integer, Number, Boolean,RasterCube, VectorCube,String,Callback,Array,Date)
  
  matches = unlist(lapply(parameter_constructor, function(constructor){
    if(constructor$new()$matchesSchema(schema)) constructor
  }))
  
  if (is.null(matches) || length(matches) == 0) matches = list(Argument) # if we don't find anything simply use this, since it is not restricted
  
  return(matches)
}

parameterFromJson = function(param_def) {
  if (is.null(param_def$schema$format)) param_def$schema$format = character()
  if (is.null(param_def$required)) param_def$required = FALSE
  
  gen=findParameterGenerator(param_def$schema)[[1]]
  param = gen$new(name=param_def$name, description=param_def$description,required = param_def$required)
  
  if ("callback" %in% class(param)) {
    # iterate over all callback properties and create CallbackParameters, but name = property name (what the process exports to callback)
    # value has to be assigned by user, then switch name and value during serialization
    
    cb_params = lapply(names(param_def$schema$properties), function(param_name) {
      param_json = param_def$schema$properties[[param_name]]
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
    names(cb_params) = names(param_def$schema$properties)
    
    # note: later all CBs need to be bound by the user
    
    param$setCallbackParameters(cb_params)
  }
  
  if ("array" %in% class(param)) {
    param$setItemSchema(param_def$schema$items)
  }
  return(param)
}

processFromJson=function(json) {
  if (is.null(json$summary)) json$summary = character()
  if (is.null(json$parameter_order)) json$parameter_order = character()
  
  
  #map parameters!
  parameters = lapply(
    json$parameters, parameterFromJson
  )
  
  Process$new(id=json$id,
              description = json$description,
              summary=json$summary,
              parameters = parameters,
              returns = json$returns,
              parameter_order = json$parameter_order)
}

