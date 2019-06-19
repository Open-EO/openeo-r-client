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
            
            #TODO maybe check here for is.list and then try the assignable
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
      final_path = unname(unlist(private$extractUsedNodeIds(endnode)))
      
      
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
        return(node$serialize())
      })
      names(result) = names(private$nodes)
      
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
      params = node$parameters
      
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
      nodeParams = unlist(lapply(node$parameters, function (param) {
        #check if the argument contains a ProcessNode in a list
        if (all("list" %in% class(param$getValue()))) {
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

