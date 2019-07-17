library(R6)
library(jsonlite)
library(lubridate)
# definitions ----

# Graph ====
Graph = R6Class(
  "Graph",
  lock_objects = FALSE,
  public = list(
    data = list(),
    
    initialize = function(processes, data = list()) {
      if (!is.list(processes)) stop("Processes are not provided as list")
      
      self$data = data
      
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
          
          while (node_id %in% private$getNodeIds()) {
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
            
            #TODO maybe check here for is.list and then try the assignable
            arguments[[param_name]]$setValue(call_arg)
          }, arguments = arguments)
          
          # special case: value is of type Argument
          
          node = ProcessNode$new(node_id = node_id,process=process)
          
          private$nodes = append(private$nodes,node)
          
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
      final_path = unname(unlist(private$extractUsedNodeIds(endnode)))
      
      
      unvisitedNodes = setdiff(private$getNodeIds(),final_path)
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
        return(node$serialize())
      })
      names(result) = private$getNodeIds()
      
      result[[self$getFinalNode()$getNodeId()]]$result = TRUE
      
      return(result)
    },
    
    validate = function() {
      tryCatch({
        self$clean() # only test that what has to be used
        
        # for each process node call their processes parameters validate function
        results = unname(unlist(lapply(private$nodes, function(node) {
          node$validate(node_id=node$getNodeId())
        })))
        
        
        if(is.null(results)) {
          return(TRUE)
        } else {
          return(results)
        }
        
      }, error = function(e){
        message(e$message)
      })
      
      
      
      
    },
    
    getNode = function (node_id) {
      private$assertNodeExists(node_id)
      return(private$nodes[[which(private$getNodeIds() == node_id)]])
    },
    
    removeNode = function(node_id) {
      private$assertNodeExists(node_id)
      private$nodes[[which(private$getNodeIds() == node_id)]] = NULL
      
      return(TRUE)
    },
    
    getFinalNode = function() {
      if (length(private$final_node_id) == 0) {
        message("No final node set in this graph.")
        invisible(NULL)
      } else {
        return(private$nodes[[which(private$getNodeIds() == private$final_node_id)]])
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
      
      node = self$getNode(node_id)
      params = node$parameters
      
      if (! parameter %in% names(params)) stop(paste0("Cannot find parameter '",parameter,"' for process '",node_id,"'"))
      
      params[[parameter]]$setValue(value)
      return(params[[parameter]]$validate()) # TODO decide if this is useful
    },
    
    getVariables = function() {
      return(private$variables)
    },
    
    createVariable = function(id,description=NULL,type="string",default=NULL) {
      var = Variable$new(id=id, description=description,type=type,default=default)
      
      temp_list= list(var)
      names(temp_list) = var$getName()
      private$variables = append(private$variables, var)
      
      return(var)
    },
    
    removeVariable = function(variable_id) {
      private$variables[[variable_id]] = NULL
    }
  ),
  private = list(
    nodes = list(),
    variables = list(),
    final_node_id = character(),
    
    assertNodeExists = function(node_id) {
      if (! node_id %in% private$getNodeIds()) stop(paste0("Cannot find node with id '",node_id,"' in this graph."))
    },
    
    getNodeIds = function() {
      return(sapply(private$nodes, function(node)node$getNodeId()))
    },
    
    extractUsedNodeIds = function(node) {
      
      nodeParams = unlist(lapply(node$parameters, function (param) {
        #check if the argument contains a ProcessNode in a list
      
        if (!is.null(param$getValue()) && all("list" %in% class(param$getValue()))) {
          nodesInList = lapply(param$getValue(), function(listArg) {
            
            if ("ProcessNode" %in% class(listArg)) return(listArg)
            
            return(NULL)
          })
          return(nodesInList)
        }
        
        # check if the argument contains a ProcessNode in itself
        if ("ProcessNode" %in% class(param$getValue())) return(param$getValue())
        
        # if no node return NULL which will be removed with unlist()
        return(NULL)
      }))
      
      if (is.null(nodeParams)) return(node$getNodeId())
      if (length(nodeParams) == 0) return(node$getNodeId())
      
      return(c(node$getNodeId(),sapply(nodeParams,private$extractUsedNodeIds)))
      
    }
  )
)

#'@export
setOldClass(c("Graph","R6"))

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
      
      # iterate over all the parameter objects and extract their name
      parameter_names = sapply(parameters, function(p) {
        if (is.list(p)) {
          # in case there was a any of parameter
          p = p[[1]]
        }
        
        p$getName()
      })
      names(parameters) = parameter_names
      
      
      self$parameters = parameters
      
      if (any(returns$schema$type=="null")) {
        returns$schema$type[returns$schema$type=="null"] = NULL
        returns$schema$type = unlist(returns$schema$type)
        
        returns$nullable = TRUE
      }
      
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
      result = rep(NA,length(self$parameters))
      names(result) = names(self$parameters)
      
      return(result)
    },
    setDescription = function(value) {
      if (is.null(value) || is.na(value)) value = character()
      private$description = value
    },
    setParameter= function(name,value) {
      if (!name %in% names(private$.parameters)) stop("Cannot find parameter")
      
      private$.parameters[[name]]$setValue(value)
    },
    getParameter = function(name) {
      if (!name %in% names(self$parameters)) stop("Cannot find parameter")
      
      return(self$parameters[[name]])
    },
    serialize = function() {
      serializedArgList = lapply(self$parameters, 
                                 function(arg){
                                   if(!arg$isEmpty()) arg$serialize()
                                 })
      
      serializedArgList[sapply(serializedArgList,is.null)] = NULL
      
      results = list(process_id=private$id,
                     arguments = serializedArgList
      )
      
      if (length(private$description)>0) results$description = private$description
      
      return(results)
    },
    validate = function(node_id=NULL) {
      return(unname(unlist(lapply(self$parameters,function(arg, node_id){
        arg$validate(node_id = node_id)
      },node_id = node_id))))
    },
    
    getCharacteristics = function() {
      # select all non functions of the private area, to be used when copying process information into a process node
      fields = sapply(names(private),
                            function(name){
        if(!is.function(private[[name]])) {
          return(name)
        }
      })
      
      return(mget(x=unname(unlist(fields)),envir=private))
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
    parameter_order = character(), # a vector of string corresponding to the parameter order
    summary = character(),
    description = character(),
    categories = character(), # probably more than 1 string -> so it is a vector of strings
    examples = list(), # a list of objects with arguments: <list>, returns: <value>
    .parameters = list(),
    
    deep_clone = function(name, value) {
      # also check if it is a list of R6 objects
      if (name == ".parameters") {
        
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
      
      if (is.environment(value) && !is.null(value$`.__enclos_env__`)) {
        return(value$clone(deep = TRUE))
      }
      
      value
    }
    
    
  )
)

# ProcessNode ====
ProcessNode = R6Class(
  "ProcessNode",
  inherit = Process,
  public = list(
    
    # initialized in the graph$<function> call  together with is argument values
    initialize = function(node_id=character(),process) {
      private$node_id = node_id
      
      if (!"Process" %in% class(process)) stop("Process is not of type 'Process'")
      private$copyAttributes(process)
    },
    
    getNodeId = function() {
      return(private$node_id)
    },
    setNodeId = function(id) {
      private$node_id = id
    },
    serializeAsReference = function() {
      return(list(
        from_node=private$node_id
      ))
    }
  ),
  
  private = list(
    node_id = character(),
    
    copyAttributes = function(process) {
      #extract names that are no function
      tobecopied = process$getCharacteristics()
      
      tobecopied[["description"]] = character()
      
      lapply(names(tobecopied), function(attr_name) {
        assign(x = attr_name, value = tobecopied[[attr_name]],envir = private)
      })
      return()
    }
  )
)

.randomNodeId = function(name,n = 1,...) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste(name,paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)),...)
}

#' Parses a JSON openeo graph into a R graph
#' 
#' The function reads and parses the given json text and creates based on the information of the
#' text a Graph object.
#' 
#' @param con a connected openeo client
#' @param json the json graph in a textual representation or an already parsed list object
#' @param graph a already created process graph (probably empty) for callback graphs
#' @return Graph object
#' @export
parse_graph = function(con, json, graph=NULL) {
  if (is.list(json)) {
    parsed_json = json
  } else {
    if (!is.character(json)) json = as.character(json)
    
    is_valid = jsonlite::validate(json)
    
    if (!is_valid) {
      message("Invalid JSON object. With the following message:")
      stop(attr(is_valid,"err"))
    }
    if (is.null(graph)) graph = process_graph_builder(con)
    
    parsed_json = fromJSON(json)
  }
  
  
  # first create foreach process an object
  lapply(names(parsed_json), function(node_id) {
    json_process = parsed_json[[node_id]]
    
    
    
    process = graph[[json_process$process_id]]()
    process$setNodeId(node_id)
    
    
    if (!is.null(json_process$result) && json_process$result) {
      graph$setFinalNode(process)
    }
    
    return(node_id)
  })
  
  lapply(names(parsed_json), function(node_id) {
    json_process = parsed_json[[node_id]]
    args = json_process$arguments
    
    # first create foreach process an object
    process = graph$getNode(node_id)
    
    # set the arguments
    lapply(names(args), function(param_name) {
      value = args[[param_name]]
      
      # keep track of callback, variables, node references
      # identifiers:
      # node ref - list of length 1 with name "from_node"
      # callback - list of length >= 1 with name "callback"
      # callback parameters/values - list with name "from_argument"
      # variable - list with at least at least name "variable_id" (optional description and type)
      # lists might be parsed as named vector, so don't check for list
      if ("from_node" %in% names(value)) {
        process$setParameter(param_name,graph$getNode(value))
        
        return(param_name)
      }
      
      if ("variable_id" %in% names(value)) {
        description = if ("description" %in% names(value)) value[["description"]] else character()
        type = if ("type" %in% names(value)) value[["type"]] else "string"
        default = if ("default" %in% names(value)) value[["default"]] else NULL
        
        variable = Variable$new(id =value[["variable_id"]],
                                description = description,
                                type = type,
                                default = default)
        
        process$setParameter(param_name, variable)
        return(param_name)
      }
      
      if ("from_argument" %in% names(value)) {
        
        cb_val = CallbackValue$new(name=value[["from_argument"]])
        process$setParameter(param_name, cb_val)
        return(param_name)
      }
      
      if ("callback" %in% names(value)) {
        cb_graph = callback(con,process,param_name)
        parse_graph(con,value[["callback"]],cb_graph)
      
        # no extra setting because in callback function the link between the object environments have
        # already been set
        return(param_name)  
      }
      
      
      process$setParameter(param_name,value)
      
    })
    
  })
  
  
  
  
  return(graph)
}