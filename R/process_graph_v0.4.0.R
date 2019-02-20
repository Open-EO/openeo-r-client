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
          arguments = process$getParameters()
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
      endnode = self$getFinalNode()
      
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
      
      result = lapply(private$nodes, function(node) {
        return(node$getProcess()$serialize())
      })
      names(result) = names(private$nodes)
      
      return(list(process_graph=result))
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
    }
  ),
  private = list(
    nodes = list(),
    final_node_id = character(),
    
    assertNodeExists = function(node_id) {
      if (! node_id %in% names(private$nodes)) stop(paste0("Cannot find node with id '",node_id,"' in this graph."))
    },
    
    extractUsedNodeIds = function(node) {
      nodeParams = unlist(lapply(node$getProcess()$getParameters(), function (param) {
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
    initialize = function(id,parameters,returns) {
      private$id = id
      
      if (! is.list(parameters)) stop("Parameters are not provided as list")
      
      parameter_names = sapply(parameters, function(p)p$getName())
      names(parameters) = parameter_names
      
      private$parameters = parameters
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
    serialize = function() {
      return(
        list(process_id=private$id,
             arguments = lapply(private$parameters, 
                                function(arg)arg$serialize())
        )
      )
    }
  ),
  private=list(
    id = character(),
    returns = NULL, # the object that is returend Parameter or Argument
    parameters = list(), # named list! this is made sure of in constructor
    parameter_order = character(), # a vector of string corresponding to the parameter order
    summary = character(),
    description = character(),
    categories = character(), # probably more than 1 string -> so it is a vector of strings
    examples = list(), # a list of objects with arguments: <list>, returns: <value>
    
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
    initialize=function(name,required=FALSE) {
      private$name = name
      private$required = FALSE
    },
    
    isRequired = function() {
      return(private$required)
    },
    
    getName = function() {
      return(private$name)
    }
  ),
  private = list(
    name=character(),
    schema = list(
      type=character(),
      format = character()
    ),
    required = logical()
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
    serialize = function() {
      # implemented by children
    }
  ),
  private = list(
    value=character()
  )
)

# Integer ====
Integer = R6Class(
  "integer",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),required=FALSE) {
      private$name = name
      private$required = FALSE
      private$schema$type = "integer"
    },
    serialize = function() {
      return(as.integer(private$value))
    }
  ),
  private = list()
)
# Number ====
Number = R6Class(
  "number",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),required=FALSE) {
      private$name = name
      private$required = FALSE
      private$schema$type = "number"
    },
    serialize = function() {
      return(as.numeric(private$value))
    }
  ),
  private = list()
)

# String ====
String = R6Class(
  "string",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),required=FALSE) {
      private$name = name
      private$required = FALSE
      private$schema$type = "string"
    },
    serialize = function() {
      return(as.character(private$value))
    }
  ),
  private = list()
)

# Boolean ====
Boolean = R6Class(
  "boolean",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),required=FALSE) {
      private$name = name
      private$required = FALSE
      private$schema$type = "boolean"
    },
    serialize = function() {
      return(as.logical(private$value))
    }
  ),
  private = list()
)

# Date ====
Date = R6Class(
  "date",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),required=FALSE) {
      private$name = name
      private$required = FALSE
      private$schema$type = "string"
      private$schema$format = "date"
    },
    serialize = function() {
      return(as.character(format(private$value,format = "%Y-%m-%dT%H:%M:%SZ")))
    }
  ),
  private = list()
)

# RasterCube ====
RasterCube = R6Class(
  "raster-cube",
  inherit=Argument,
  public = list(
    initialize=function(name=character(),required=FALSE) {
      private$name = name
      private$required = FALSE
      private$schema$type = "object"
      private$schema$format = "raster-cube"
    },
    serialize = function() {
      if ("ProcessNode" %in% class(private$value)) {
        return(private$value$serialize())
      }
      
      return(as.character(private$value))
    },
    setValue = function(value) {
      private$value = value
    }
  ),
  private = list()
)

.randomNodeId = function(name,n = 1,...) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste(name,paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)),...)
}

