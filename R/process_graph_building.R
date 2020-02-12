library(R6)
library(jsonlite)
library(lubridate)
# definitions ----

# Graph ====
#' Graph object
#' 
#' This class represents an openeo process graph. It consists of \code{\link{ProcessNode}}s and optional \code{\link{Variable}}s. The 
#' class as such offers also an environment where the offered processes of a back-end are made available on
#' runtime. This means besides the functions mentioned here, there are also the processes of the back-end
#' mapped dynamically after creation.
#' 
#' @name Graph
#' @return Object of \code{\link{R6Class}} with methods for building an openeo process graph
#' 
#' @field data a named list of collection ids or callback parameters depending on the context
#' @section Methods:
#' \describe{
#'    \item{\code{$new(processes, data = list())}}{The object creator created from processes and available data. 
#'    If \code{data} was omitted then it is fetched from \code{\link{list_collections}}. }
#'    \item{$getNodes()}{a function to return a list of created \code{\link{ProcessNode}}s for this graph}
#'    \item{$clean()}{function to clean the graph from unused process nodes that are not connected with the graph}
#'    \item{$serialize()}{creates a list representation of the graph by recursively calling \code{$serialize} or 
#'    \code{$serializeAsReference} on all graph elements that are connected to the graph}
#'    \item{$validate()}{runs through the nodes and checks the validity of its argument values}
#'    \item{$getNode(node_id)}{searches and returns a node from within the graph referenced by its node id}
#'    \item{$removeNode(node_id)}{removes a process node from the graph}
#'    \item{$getFinalNode()}{gets the result process node of a process graph}
#'    \item{$setFinalNode(node)}{sets the result process node by node id or a ProcessNode}
#'    \item{$setArgumentValue(node_id, parameter, value)}{sets or replaces a value on a specific ProcessNodes parameter with the given value}
#'    \item{$getVariables()}{creates a named list of the defined variables of a process graph}
#'    \item{$createVariable(id,description=NULL,type="string",default=NULL)}{creates a variable and registers 
#'    it internally as defined variable}
#'    \item{$removeVariable(variable_id)}{removes a variable}
#' }
#' @section Arguments:
#' \describe{
#'    \item{processes}{a list of Process objects}
#'    \item{data}{optional a named list of available data}
#'    \item{node_id}{the id of a process node}
#'    \item{node}{process node or  its node id}
#'    \item{parameter}{the name of a parameter in a process}
#'    \item{value}{the value to be set for a parameter of a paricular process}
#'    \item{id or variable_id}{the variable id}
#'    \item{description}{a description field for a variable}
#'    \item{type}{the type of variable, default 'string'}
#'    \item{default}{optional default value to be set for a variable}
#' }
NULL

Graph = R6Class(
  "Graph",
  lock_objects = FALSE,
  public = list(
    data = list(),
    
    initialize = function(con, data = list(),final_node=NULL) {
      con = .assure_connection(con)
      
      private$connection = con
      
      if (is.null(final_node)) {
        processes = lapply(con$processes, processFromJson)
        
        if (!is.list(processes)) stop("Processes are not provided as list")
        
        self$data = data
        
        for (index in 1:length(processes)) {
          if (is.null(processes[[index]])) {
            next
          }
          
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
            exec_process = processes[[index]]$clone(deep=TRUE)
            # find new node id:
            node_id = .randomNodeId(exec_process$getId(),sep="_")
            
            while (node_id %in% private$getNodeIds()) {
              node_id = .randomNodeId(exec_process$getId(),sep="_")
            }
            
            #map given parameter of this function to the process parameter / arguments and set value
            arguments = exec_process$parameters
            
            # parameter objects should be updated directly, since there is a real object reference
            this_param_names = names(formals())
            
            # used match.call before, but it seem that it doesn't resolve the pipe - it is something like data = .
            this_arguments = lapply(this_param_names, function(param) get(param))
            names(this_arguments) = this_param_names
            
            # special case: value is of type Argument
            
            node = ProcessNode$new(node_id = node_id,process=exec_process,graph=self)
            
            lapply(names(this_arguments), function(param_name, arguments){
              call_arg = this_arguments[[param_name]]
              
              #TODO maybe check here for is.list and then try the assignable
              arguments[[param_name]]$setValue(call_arg)
            }, arguments = arguments)
            
            private$nodes = append(private$nodes,node)
            
            return(node)
          })
          # replace index with the actual number!
          tmp = gsub(body(f),pattern="index",replacement = eval(index))
          body(f) = as.call(c(as.name(tmp[1]),parse(text=tmp[2:length(tmp)])))
          
          # register the ProcessNode creator functions on the Graph class
          self[[pid]] = f
        }
      } else if ("ProcessNode" %in% class(final_node)) {
        node_list = .final_node_serializer(final_node)
        private$nodes=node_list
        private$final_node_id = final_node$getNodeId()
      }
      
      invisible(self)
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
        private$nodes[private$getNodeIds() == removeables] = NULL
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
          node$validate()
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
    addNode = function(node) {
      if (!"ProcessNode" %in% class(node)) stop("Input object is no ProcessNode")
      private$nodes = c(private$nodes,node)
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
        return(private$nodes[[which(private$getNodeIds() == private$final_node_id)[1]]])
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
      private$variables = append(private$variables, temp_list)
      
      return(var)
    },
    
    removeVariable = function(variable_id) {
      private$variables[[variable_id]] = NULL
    },
    
    getConnection = function() {
      return(private$connection)
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

# ProcessCollection ====
ProcessCollection = R6Class(
  "ProcessCollection",
  lock_objects = FALSE,
  public = list(
    data = list(),
    initialize = function(con, data = list()) {
      con = .assure_connection(con)
      
      private$connection = con
      
      private$processes = lapply(con$processes, processFromJson)
      
      if (!is.list(private$processes)) stop("Processes are not provided as list")
      
      self$data = data
      
      for (index in 1:length(private$processes)) {
        if (is.null(private$processes[[index]])) {
          next
        }
        
        pid = private$processes[[index]]$getId()
        function_formals = private$processes[[index]]$getFormals()
        
        f = function() {}
        formals(f) = function_formals
        
        
        # probably do a deep copy of the object
        # for the body we have the problem that index is addressed as variable in the parent environment. This
        # causes a problem at call time, where index is resolve and this means that usually the last element
        # of the list will be used as process all the time -> solution: serialize index, gsub on quote, make "{" as.name
        # and then as.call
        body(f) = quote({
          exec_process = private$processes[[index]]$clone(deep=TRUE)
          # find new node id:
          node_id = .randomNodeId(exec_process$getId(),sep="_")
          
          while (node_id %in% private$getNodeIds()) {
            node_id = .randomNodeId(exec_process$getId(),sep="_")
          }
          
          private$node_ids = c(private$node_ids,node_id)
          
          #map given parameter of this function to the process parameter / arguments and set value
          arguments = exec_process$parameters
          
          # parameter objects should be updated directly, since there is a real object reference
          this_param_names = names(formals())
          
          # used match.call before, but it seem that it doesn't resolve the pipe - it is something like data = .
          this_arguments = lapply(this_param_names, function(param) get(param))
          names(this_arguments) = this_param_names
          
          # special case: value is of type Argument
          
          node = ProcessNode$new(node_id = node_id,process=exec_process,graph=self)
          
          lapply(names(this_arguments), function(param_name, arguments){
            call_arg = this_arguments[[param_name]]
            
            #TODO maybe check here for is.list and then try the assignable
            arguments[[param_name]]$setValue(call_arg)
          }, arguments = arguments)
          
          return(node)
        })
        # replace index with the actual number!
        tmp = gsub(body(f),pattern="index",replacement = eval(index))
        body(f) = as.call(c(as.name(tmp[1]),parse(text=tmp[2:length(tmp)])))
        
        # register the ProcessNode creator functions on the Graph class
        self[[pid]] = f
      }
    }
  ),
  private = list(
    conection = NULL,
    node_ids = character(),
    processes = list(),
    getNodeIds = function() {private$node_ids}
  )
)

# Process ====
#' Process object
#' 
#' This object reflects the process offered by a back-end. It will be created with the information of a received 
#' JSON object for a single process, after the arguments of the process have been translated into \code{\link{Argument}} objects.
#' 
#' @name Process
#' 
#' @return Object of \code{\link{R6Class}} with methods for storing meta data of back-end processes and user assigned data
#' 
#' @field parameters a named list of Argument objects
#' 
#' @section Methods:
#' \describe{
#'    \item{$new(id,parameters,description=character(), summary = character(), parameter_order=character(),returns)}{}
#'    \item{$getId()}{returns the id of a process which was defined on the back-end}
#'    \item{$getParameters()}{returns a named list of Arguments}
#'    \item{$getParameterOrder()}{returns the order of the parameters for this process}
#'    \item{$getReturns()}{returns the schema for the return type as list}
#'    \item{$getFormals()}{returns the function formals for this process - usually a name vector of NAs where the name 
#'    corresponds to the parameter name}
#'    \item{$validate()}{validates the processes argument values}
#'    \item{$serialize()}{serializes the process - mainly used as primary serialization for a \code{\link{ProcessNode}}}
#'    \item{$setParameter(name,value)}{sets the value of a parameter}
#'    \item{$getParameter(name)}{returns the Argument object with the provided name}
#'    \item{$setDescription(value)}{sets the description text}
#'    \item{$getCharacteristics()}{select all non functions of the private area, to be used when copying process 
#'    information into a process node}
#' }
#' @section Arguments:
#' \describe{
#'    \item{id}{process id from the back-end}
#'    \item{parameters}{a list of Argument objects}
#'    \item{description}{the process description}
#'    \item{summary}{the summary of a process}
#'    \item{parameter_order}{the order in which the parameters have to be returned to the backend}
#'    \item{returns}{the schema part of the result definition}
#'    \item{name}{a parameter name}
#'    \item{value}{the value for a parameter or the description text}
#' }
NULL

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
      return(private$.parameters)
    },
    
    getReturns = function() {
      
      return(private$returns)
    },
    
    getFormals = function() {
      if (length(self$parameters) == 0) return(list())
      
      result = as.list(rep(NA,length(self$parameters)))
      
      # set also default values
      
      for (i in 1:length(self$parameters)) {
        
        default_value = self$parameters[[i]]$getDefault()

        if (!is.null(default_value)) {
          result[[i]] = default_value
        }
        # otherwise leave it as NA
      }
      
      
      names(result) = sapply(self$parameters, function(param)param$getName())
      
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
    getParameterOrder = function() {
      return(private$parameter_order)
    },
    serialize = function() {
      if (length(self$parameters) > 0) {
        serializedArgList = lapply(self$parameters, 
                                   function(arg){
                                     arg$serialize()
                                   })
        
        serializedArgList[sapply(serializedArgList,is.null)] = NULL
        
        results = list(process_id=private$id,
                       arguments = serializedArgList
        )
      } else {
        results = list(process_id=private$id,
                       arguments = NA
        )
      }
      
      if (length(private$description)>0) results$description = private$description
      
      return(results)
    },
    validate = function() {
      return(unname(unlist(lapply(self$parameters,function(arg){
        arg$validate()
      }))))
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
    connection=NULL, # the openeo backend connection to which this graph belongs to
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
#' Process Node object
#' 
#' This class inherits all functions and fields from \code{\link{Process}} and extends it with a node id and a 
#' special serialization function. The ProcessNode is an essential building block of the \code{\link{Graph}}.
#' 
#' @name ProcessNode
#' @section Methods:
#' \describe{
#'    \item{$getNodeId()}{returns the node id}
#'    \item{$setNodeId(id)}{set the node id, which is of interest when \code{\link{parse_graph}} is executed}
#'    \item{$serializeAsReference()}{during the serialization the process node might be used as a 
#'    reference and this function serializes the process node accordingly}
#' }
#' @section Arguments:
#' \describe{
#'    \item{id}{the node id}
#' }
NULL

ProcessNode = R6Class(
  "ProcessNode",
  inherit = Process,
  public = list(
    
    # initialized in the graph$<function> call  together with is argument values
    initialize = function(node_id=character(),process,graph=NULL) {
      private$node_id = node_id
      
      if (!"Process" %in% class(process)) stop("Process is not of type 'Process'")
      
      if(length(graph) > 0) {
        private$graph = graph
      }
      
      private$copyAttributes(process)
      
      # all arguments need a reference to their parent process, this also counts for callback
      # values!
      lapply(private$.parameters,function(param)  {
        param$setProcess(self)
        if ("callback" %in% class(param)) {
          lapply(param$getCallbackParameters(), function(cbv) {
            cbv$setProcess(param$getProcess())
          })
        }
      })
      
      return(self)
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
    },
    getGraph = function() {
      return(private$graph)
    },
    setGraph = function(g) {
      private$graph = g
      invisible(self)
    }
  ),
  
  private = list(
    node_id = character(),
    graph = NULL,
    
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

setOldClass(c("ProcessNode","Process","R6"))

.randomNodeId = function(name,n = 1,...) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste(name,paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)),...)
}

# Extra functions / wrapper ----

#' Parses a JSON openeo graph into a R graph
#' 
#' The function reads and parses the given json text and creates based on the information of the
#' text a Graph object.
#' 
#' @param con a connected openeo client
#' @param json the json graph in a textual representation or an already parsed list object
#' @param graph an already created process graph (probably empty) for callback graphs
#' @return Graph object
#' @export
parse_graph = function(con, json, graph=NULL) {
  con = .assure_connection(con)
  
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

#' Creates a variable in a process graph
#' 
#' This function will create a variable to be used in the designated process graph with additional optional information.
#' 
#' @param graph a process graph object
#' @param id the id of the variable
#' @param description an optional description of the variable
#' @param type the type of the value that is replaced on runtime, default 'string'
#' @param default the default value for this variable
#' @return a \code{\link{Variable}} object
#' 
#' @export
create_variable = function(graph, id,description=NULL,type="string",default=NULL) {
  if ("ProcessNode" %in% class(graph)){
    # final node!
    graph = Graph$new(final_node = graph)
  }
  
  if (!all(c("Graph","R6") %in% class(graph))) stop("Parameter graph is no Graph object")
  
  if (length(id) == 0) stop("Variable id was not set.")
  
  return(graph$createVariable(id,description,type,default))
}

#' Lists the defined variables for a graph
#' 
#' The function creates a list of the defined (not necessarily used) variables of a process graph.
#' 
#' @param graph a process graph object
#' @return a named list of Variables
#' 
#' @export
variables = function(graph) {
  if (!all(c("Graph","R6") %in% class(graph))) stop("Parameter graph is no Graph object")
  
  return(graph$getVariables())
}

#' Removes a variable from the Graph
#' 
#' The function removes a selected variable from the graph. It only removes it from the list of defined 
#' variables that are obtainable with \code{\link{variables}}. Those that are already placed in the graph 
#' won't be deleted in the graph, only in the defined variables list.
#' 
#' @param graph a \code{\link{Graph}} object
#' @param variable a variable id or a variable object
#' @return TRUE
#' @export
remove_variable = function(graph, variable) {
  if ("ProcessNode" %in% class(graph)){
    # final node!
    graph = Graph$new(final_node = graph)
  }
  
  if (!all(c("Graph","R6") %in% class(graph))) stop("Parameter graph is no Graph object")
  
  if (length(variable) == 0) stop("Parameter 'variable' cannot be NULL or empty")
  if (all(c("variable","Argument","R6") == class(variable))) variable = variable$getName()
  
  return(graph$removeVariable(variable_id = variable))
}

.final_node_serializer = function(node,graph=list()) {
  add = list(node)
  names(add) = node$getNodeId()
  
  paramValues = unname(unlist(lapply(node$parameters,function(param)param$getValue())))
  nodeSelectors = sapply(paramValues,function(v) {
    "ProcessNode" %in% class(v)
  })
  selectedNodes = paramValues[nodeSelectors]
  
  if(length(selectedNodes)==0) {
    return(add)
  } else {
    temp = unlist(c(lapply(selectedNodes,function(node){
      .final_node_serializer(node,graph)
    }),add))
    
    unames = unique(names(temp))
    
    return(temp[unames])
  } 
}