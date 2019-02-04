#' OpenEO client class
#'
#' A R6Class that interacts with an openEO-conformant backend.
#'
#' @importFrom R6 R6Class
#' @import httr
#' @import magrittr
#' @import jsonlite
#' @importFrom lubridate as_datetime
#' @import dplyr 
#' @export
OpenEOClient <- R6Class(
  "OpenEOClient",
  # public ----
  public = list(
    # attributes ====
    disableAuth = TRUE,
    general_auth_type = "bearer",
    user_id = NULL,
    
    api.version = "0.3.1",
    api.mapping = NULL,

    # functions ====
    initialize = function() {

    },

    connect = function(url,login_type="basic",disable_auth=FALSE) {
      tryCatch({
        if (is.null(login_type) || !login_type %in% c("basic","oidc","none") ) {
          stop("Cannot find the login mechanism type. Please use 'basic', 'oidc' or 'none'")
        }
        
        if (disable_auth) {
          self$disableAuth = disable_auth
        }
        
        if (!missing(url)) {
          
            if (endsWith(url,"/")) {
              url = substr(url,1,nchar(url)-1)
            }
            private$host = url
            private$login_type=login_type
            
            self$api.mapping = endpoint_mapping(self)
            cat("Connected to host\n")
            return(invisible(self))
        } else {
          stop("Host-URL is missing")
        }
      }, 
      error = .capturedErrorToMessage
      )

    },
    capabilities = function() {
      endpoint = ""
      tryCatch({
        private$stopIfNotConnected()
        
        capabilities = private$GET(endpoint = endpoint,authorized = FALSE)
        
        return(capabilities)
      },
      error = .capturedErrorToMessage)
      
    },
    services = function() {
      tryCatch({
        private$stopIfNotConnected()
        
        tag = "ogc_services"
        endpoint = private$getBackendEndpoint(tag)
        
        services = private$GET(endpoint, authorized = FALSE)
        
        updated_services = list()
        for (key in names(services)) {
          service = services[[key]]
          service$service = key
          
          updated_services = c(updated_services,list(service))
        }
        return(lapply(updated_services, function(service) {
          class(service) = "ServiceType"
          return(service)
        }))
      },error=.capturedErrorToMessage)
    },
    output_formats = function() {
      tryCatch({
        tag = "formats"
        endpoint = private$getBackendEndpoint(tag)
        
        formats = private$GET(endpoint,authorized = FALSE)
        default = formats$default
        message(paste("Host uses '",formats$default,"' as default output format",sep=""))
        
        formats = formats$formats
        names = names(formats)
        datatypes = unname(lapply(formats, function(format){
          return(format$gis_data_types)
        }))
        
        parameters = unname(lapply(formats, function(format){
          return(format$parameters)
        }))
        
        library(tibble)
        
        table = tibble(format=names,type=datatypes,parameters = parameters)
        
        return(table)
      },
      error = .capturedErrorToMessage
      )
      
    },
    udf_runtimes = function() {
      tryCatch(
        {
          tag = "udf_runtimes"
          endpoint = private$getBackendEndpoint(tag)
          
          return(private$GET(endpoint = endpoint,
                             authorized = FALSE))
        }, 
        error = .capturedErrorToMessage
      )
    },
    
    register = function(user=NULL,password) {
      #currently this will be used for GEE only
      tryCatch({
        if (!private$isConnected()) {
          stop("No host selected")
        }
        
        tag = "registration"
        endpoint = private$getBackendEndpoint(tag)
        
        private$password = password
        
        #function(endpoint,authorized=FALSE,data,encodeType = "json",query = list(), raw=FALSE,...) {
        res = private$POST(endpoint=endpoint,
                           data = list(password=password),
                           authorized = FALSE)
        
        private$user = res$user_id
        return(private$user)
      },
      error = .capturedErrorToMessage
      )
    },
    login=function(user=NULL, password=NULL) {
      tryCatch({
        if (private$login_type %>% is.null()) {
          stop("Cannot login. Please connect to an OpenEO back-end first.")
        }
        private$stopIfNotConnected()
        
        if (private$login_type == "oidc") {
          private$loginOIDC()
        } else if (private$login_type == "basic") {
          private$loginBasic(user=user, password = password)
        } else {
          if (self$disableAuth) {
            return(self)
          } else {
            stop("Unsupported login mechanism")
          }
        }
        
        processes = self$listProcesses()
        pids = sapply(processes, function(x)x$name)
        names(processes) = pids
        private$processes = processes
        
        invisible(self)
        
      },
      error=.capturedErrorToMessage)
    },
    logout = function() {
      if (!is.null(private$oidc_client)){
        private$oidc_client$logout()
      }
    },
    user_info = function() {
      tryCatch({
        tag = "user_info"
        endpoint = private$getBackendEndpoint(tag)
        
        user_info = private$GET(endpoint=endpoint,authorized = TRUE,type="application/json")
        
        class(user_info) = "User"
        return(user_info)
        
      },
      error = .capturedErrorToMessage)
    },
    # list functions ####
    listData = function() {

      tryCatch({
        tag = "data_overview"
        endpoint = private$getBackendEndpoint(tag)
        
        listOfProducts = private$GET(endpoint=endpoint,type="application/json")
        class(listOfProducts) = "CollectionList"
        return(listOfProducts)
      },
      error=.capturedErrorToMessage)
    },
    
    listProcesses = function() {

      tryCatch({
        tag = "process_overview"
        endpoint = private$getBackendEndpoint(tag)
        
        listOfProcesses = private$GET(endpoint,type="application/json")
        listOfProcesses = listOfProcesses$processes
        
        return(lapply(listOfProcesses,function(process) {
          class(process) = "ProcessInfo"
          return(process)
        }))
      },
      error=.capturedErrorToMessage)
      
    },
    listJobs = function() {
      
      tryCatch({
        tag = "user_jobs"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id)
        
        listOfJobs = private$GET(endpoint,authorized=TRUE,type="application/json")
        listOfJobs = listOfJobs$jobs
        # list to tibble
        table = tibble(job_id=character(),
                       title = character(),
                       status=character(),
                       submitted=.POSIXct(integer(0)),
                       updated=.POSIXct(integer(0)),
                       costs=integer(0),
                       budget=integer(0),
                       plan=character()
                       )
        # desription left out on purpose... it might be to much to visualize
        
        if (length(listOfJobs) > 0) {
          for (index in 1:length(listOfJobs)) {
            job = listOfJobs[[index]]
            
            suppressWarnings({
              job_id = NA
              if (!is.null(job$job_id)) job_id = job$job_id
              title = NA
              if (!is.null(job$title)) title = job$title
              status = NA
              if (!is.null(job$status)) status = job$status
              submitted = NA
              if (!is.null(job$submitted)) submitted = as_datetime(job$submitted)
              updated = NA
              if (!is.null(job$updated)) updated = as_datetime(job$updated)
              costs = NA
              if (!is.null(job$costs)) costs = as.numeric(job$costs)
              budget = NA
              if (!is.null(job$budget)) budget = as.numeric(job$budget)
              plan = NA
              if (!is.null(job$plan)) plan = job$plan
              
              table= add_row(table,
                             job_id=job_id,
                             title = title,
                             status=status,
                             submitted=submitted,
                             updated=updated,
                             costs=costs,
                             budget=budget,
                             plan=plan)
            })
          }
        }
        
        return(table)
      },
      error=.capturedErrorToMessage)
    },
    
    listGraphs = function() {
      tryCatch({
        tag = "graph_overview"
        endpoint = private$getBackendEndpoint(tag)
        
        listOfGraphShortInfos = private$GET(endpoint, authorized = TRUE)
        listOfGraphShortInfos = listOfGraphShortInfos$process_graphs
        
        table = tibble(process_graph_id=character(),
                       title=character(),
                       description=character())
        
        if (length(listOfGraphShortInfos) > 0) {
        
          for (index in 1:length(listOfGraphShortInfos)) {
            graph_short = listOfGraphShortInfos[[index]]
            id = graph_short$process_graph_id
            title = NA
            if (!is.null(graph_short$title)) title = graph_short$title
            description = NA
            if (!is.null(graph_short$description)) description = graph_short$description
            
            table= add_row(table,
                           process_graph_id=id,
                           title = title,
                           description = description)
          }
        }
        
        return(table)
      }, error = .capturedErrorToMessage)
    },
    
    listServices = function() {
      
      tryCatch(suppressWarnings({
        tag = "user_services"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id)
        
        listOfServices = private$GET(endpoint,authorized = TRUE ,type="application/json")
        listOfServices = listOfServices$services
        
        table = tibble(service_id=character(),
                       title=character(),
                       description=character(),
                       url = character(),
                       type = character(),
                       enabled = logical(),
                       submitted = .POSIXct(character(0)),
                       plan = character(0),
                       costs = numeric(0),
                       budget = numeric(0))
        
        if (length(listOfServices) > 0) {
          for (index in 1:length(listOfServices)) {
            service = listOfServices[[index]]
            
            service_id = NA
            if (!is.null(service$service_id)) service_id = service$service_id
            
            title = NA
            if (!is.null(service$title)) title = service$title
            
            description = NA
            if (!is.null(service$description)) description = service$description
            
            url = NA
            if (!is.null(service$url)) url = service$url
            
            type = NA
            if (!is.null(service$type)) type = service$type
            
            enabled = NA
            if (!is.null(service$enabled)) enabled = service$enabled
            
            submitted = NA
            if (!is.null(service$submitted)) submitted = as_datetime(service$submitted)
            
            plan = NA
            if (!is.null(service$plan)) plan = service$plan
            
            costs = NA
            if (!is.null(service$costs)) costs = as.numeric(service$costs)
            
            budget = NA
            if (!is.null(service$budget)) budget = as.numeric(service$budget)
            
            table= add_row(table, 
                           service_id=service_id,
                           title=title,
                           description=description,
                           url = url,
                           type = type,
                           enabled=enabled,
                           submitted=submitted,
                           plan = plan,
                           costs = costs,
                           budget=budget)
          }
        }
        
        
        return(table)
      }), error = .capturedErrorToMessage)
    },
    
    listUserFiles = function() {
      
      tryCatch({
        tag = "user_files"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id)
        
        files = private$GET(endpoint,TRUE,type="application/json")
        files = files$files
        
        if (is.null(files) || length(files) == 0) {
          message("The user workspace at this host is empty.")
          return(invisible(files))
        }
        
        files = tibble(files) %>% rowwise() %>% summarise(name=files$name, size=files$size)
        
        return(files)
      },error=.capturedErrorToMessage)
    },
    
    listResults = function(job_id) {
      
      tryCatch({
        tag = "jobs_download"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        listOfResults = private$GET(endpoint,authorized=TRUE,type="application/json")
        return(listOfResults)
      },error=.capturedErrorToMessage)
    },
    
    # describe functions ####
    describeProcess = function(pid) {
      if (is.null(private$processes)) {
        stop("No processes found or loaded from the back-end")
      }
      
      if (! pid %in% names(private$processes)) {
        stop(paste("Cannot describe process '",pid,"'. Process does not exist.",sep=""))
      } else {
        return(private$processes[[pid]])
      }
    },
    
    describeProduct = function(pid) {
      
      tryCatch({
        tag = "data_details"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(pid)
        
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)

        class(info) = "CollectionInfo"
        
        if (!is.null(info$`eo:bands`)) {
          class(info$`eo:bands`) = "BandList"
        }
        
        return(info)
      },
      error = .capturedErrorToMessage)
    },
    
    describeJob = function(job_id) {
      
      tryCatch({
        tag = "jobs_details"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        info = private$GET(endpoint = endpoint,authorized = TRUE, type="application/json",auto_unbox=TRUE)
        
        class(info) = "JobInfo"
        class(info$process_graph) = "process"
        
        return(info)
      },error=.capturedErrorToMessage)
      
    },
    
    describeGraph = function(graph_id) {
      tryCatch(
        {
          if (is.null(graph_id)) {
            stop("No graph id specified. Cannot fetch unknown graph.")
          }
          
          tag = "graph_details"
          endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(graph_id)
          graph = private$GET(endpoint, authorized = TRUE, type="application/json",auto_unbox=TRUE)
          
          class(graph) = "ProcessGraphInfo"
          class(graph$process_graph) = "process"
          return(graph)
        },
        error=.capturedErrorToMessage
      )
      
    },
    
    describeService = function(service_id) {
      tryCatch({
        if (is.null(service_id)) {
          stop("No service id specified.")
        }
        
        tag = "services_details"
        #TODO replace parseable attributes
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(service_id)
        
        service = private$GET(endpoint,authorized = TRUE)
        class(service) = "ServiceInfo"
        return(service)
      }, error=.capturedErrorToMessage)
    },
    
    describeUdfType = function(language, udf_type) {
      tryCatch({
        if (is.null(language) || is.null(udf_type)) {
          stop("Missing parameter language or udf_type")
        }
        tag = "udf_functions"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(language,udf_type)
        
        msg = private$GET(endpoint = endpoint,
                          authorized = FALSE)
        return(msg)
      },error=.capturedErrorToMessage)
      
    },
    # publish functions ====
    
    storeGraph = function(graph,title = NULL, description = NULL) {
      tryCatch({
        tag = "new_graph"
        endpoint = private$getBackendEndpoint(tag)
        
        if (!is.list(graph) || is.null(graph)) {
          stop("The graph information is missing or not a list")
        }
        
        requestBody = list(
          title=title,
          description = description,
          process_graph = graph
        )
        
        response = private$POST(endpoint=endpoint,
                                authorized = TRUE,
                                data=requestBody,
                                raw=TRUE)
        
        message("Graph was sucessfully stored on the backend.")
        locationHeader = headers(response)$location
        split = unlist(strsplit(locationHeader,"/"))
        return(trimws(split[length(split)]))
      },error = .capturedErrorToMessage)
    },
    
    storeJob = function(task=NULL,graph_id=NULL,
                        title = NULL, description = NULL,
                        plan = NULL, budget = NULL,
                        format, ...) {
      tryCatch({
        tag = "jobs_define"
        endpoint = private$getBackendEndpoint(tag)
        
        create_options = list(...)
        output = list()
        output$format = format
        if (length(create_options) > 0) {
          output$parameters = create_options
        }
        
        
        if (!is.null(task)) {
          if (is.list(task)) {
            job = list(process_graph=toJSON(task,force=TRUE),output = output)
          } else {
            stop("Parameter task is not a task object. Awaiting a list.")
          }
        } else if (! is.null(graph_id)) {
          job = list(process_graph=graph_id,output = output)
        } else {
          stop("No process graph was defined. Please provide either a process graph id or a process graph description.")
        }
        
        if (!is.null(title)) job$title = title
        if (!is.null(description)) job$description = description
        if (!is.null(plan)) job$plan = plan
        if (!is.null(budget)) job$budget = budget
        
        #endpoint,authorized=FALSE,data,encodeType = "json",query = list(),...
        response = private$POST(endpoint=endpoint,
                                authorized = TRUE,
                                data=job,
                                raw=TRUE)
        
        message("Job was sucessfully registered on the backend.")
        locationHeader = headers(response)$location
        split = unlist(strsplit(locationHeader,"/"))
        return(split[length(split)])
      },error=.capturedErrorToMessage)
    },
    
    createService = function(type, 
                             process_graph,
                             title = NULL,
                             description = NULL,
                             enabled = NULL,
                             parameters = NULL,
                             plan = NULL,
                             budget = NULL
                             ) {
      tryCatch({
        if (is.null(type)) {
          stop("No type specified.")
        }
        
        tag = "service_publish"
        endpoint = private$getBackendEndpoint(tag)
        
        service_request_object = list(
          type = type,
          process_graph = process_graph,
          title = title,
          description = description,
          enabled =enabled,
          parameters = parameters,
          plan = plan,
          budget = budget
        )
        
        response = private$POST(endpoint,
                                authorized = TRUE, 
                                data = service_request_object, 
                                encodeType = "json",
                                raw = TRUE)
        
        message("Service was successfully created.")
        locationHeader = headers(response)$location
        split = unlist(strsplit(locationHeader,"/"))
        return(trimws(split[length(split)]))
      },error=.capturedErrorToMessage)
    },
    uploadUserFile = function(file.path,target,encode="raw",mime="application/octet-stream") {
      tryCatch({
        target = URLencode(target,reserved = TRUE)
        target = gsub("\\.","%2E",target)
        
        if (is.null(self$user_id)) {
          stop("User id is not set. Either login or set the id manually.")
        }
        
        tag = "user_file_upload"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id,target)
        
        message = private$PUT(endpoint= endpoint,authorized = TRUE, data=upload_file(file.path,type=mime),encodeType = encode)
        message("Upload of user data was successful.")
        return(message)
      },error=.capturedErrorToMessage)
      
    },
    
    # Update functions ====
    
    modifyGraph = function(graph_id, graph=NULL,title = NULL, description = NULL) {
      tryCatch({
        if (is.null(graph_id)) {
          stop("Cannot replace unknown graph. If you want to store the graph, use 'storeGraph' instead")
        }
        
        requestBody = list()
        
        if (!is.null(graph)) {
          if (is.na(graph)) {
            stop("Cannot remove process graph from the element. Please replace it with another process graph, or ignore it via setting NULL")
          }else if (!is.list(graph)) {
            stop("The graph information is missing or not a list")
          } else {
            requestBody[["process_graph"]] = graph
          }
        }
        
        if (!is.null(title)) {
          if (is.na(title)) {
            requestBody[["title"]] = NULL
          } else {
            requestBody[["title"]] = title
          }
        }
        if (!is.null(description)) {
          if (is.na(description)) {
            requestBody[["description"]] = NULL
          } else {
            requestBody[["description"]] = description
          }
        }
        
        tag = "graph_replace"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(graph_id)
        
        message = private$PATCH(endpoint = endpoint, 
                              authorized = TRUE, 
                              data = requestBody,
                              encodeType = "json")
        
        if (is.null(message)) {
          message(paste("Process graph '",graph_id,"' was successfully modified.",sep=""))
          invisible(TRUE)
        }
      },error=.capturedErrorToMessage)
    },
    
    modifyJob = function(job_id,
                         title=NULL, description=NULL,
                         process_graph = NULL, 
                         plan = NULL, budget= NULL,
                         format=NULL, ...) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job i was specified.")
        }
        
        tag = "jobs_update"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        patch = list()
        create_options = list(...)
        output = list()
        if (length(create_options) > 0) {
          output$parameters = create_options
        }
        if (!is.null(format)) output$format = format
        
        if (length(output) > 0) patch$output = output
        
        if (!is.null(process_graph)) {
          patch$process_graph = process_graph
        }
        
        if (!is.null(title)) {
          if (is.na(title)) patch$title = NULL
          else patch$title = title
        }
        
        if (!is.null(description)){
          if (is.na(description)) patch$description = NULL
          else patch$description = description
        } 
        
        if (!is.null(plan)) {
          if (is.na(plan)) patch$plan = NULL
          else patch$plan = plan
        }
        
        if (!is.null(budget)) {
          if (is.na(budget)) patch$budget = NULL
          else patch$budget = budget
        }
        
        res = private$PATCH(endpoint = endpoint,
                            authorized = TRUE,
                            encodeType = "json",
                            data=patch)
        message(paste("Job '",job_id,"' was successfully updated.",sep=""))
        invisible(TRUE)
      },error=.capturedErrorToMessage)
    },
    
    modifyService = function(service_id,
                             type=NULL, 
                             process_graph=NULL,
                             title = NULL,
                             description = NULL,
                             enabled = NULL,
                             parameters = NULL,
                             plan = NULL,
                             budget = NULL) {
      tag = "services_update"
      endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(service_id)
      tryCatch({
        patch = list()
        
        if (!is.null(type)) {
          patch[["type"]] = type
        }
        
        if (!is.null(process_graph)) {
          if (length(process_graph) > 0) {
            patch[["process_graph"]] = process_graph
          } else {
            stop("Process graph cannot be set to be empty.")
          }
        }
        
        if (!is.null(title)) {
          if (!is.na(title)) {
            patch[["title"]] = title
          } else {
            patch[["title"]] = NULL
          }
        }
        
        if (!is.null(description)) {
          if (!is.na(description)) {
            patch[["description"]] = description
          } else {
            patch[["description"]] = NULL
          }
        }

        if (!is.null(enabled)) {
          if (!is.na(enabled) && is.logical(enabled)) {
            patch[["enabled"]] = enabled
          } else {
            stop("No valid data for parameter 'enabled'. Use TRUE, FALSE or NULL")
          }
        }
        
        if (!is.null(parameters)) {
          if (is.na(parameters)) {
            patch[["parameters"]] = NULL
          } else if (is.list(parameters)) {
            patch[["parameters"]] = parameters
          } else {
            stop("No valid data for parameter 'parameters'. It has to be a list")
          }
        }
        
        if (!is.null(plan)) {
          if (!is.na(plan)) {
            patch[["plan"]] = plan
          } else {
            stop("No valid data for parameter 'plan'. Use a plan identifier or skip updating the parameter with NULL")
          }
        }
        
        # budget = NULL
        if (!is.null(budget)) {
          if (!is.na(budget)) {
            patch[["budget"]] = budget
          } else {
            patch[["budget"]] = NULL
          }
        }
        
        res = private$PATCH(endpoint = endpoint,
                            authorized = TRUE,
                            encodeType = "json",
                            data=patch)
        message(paste("Service '",service_id,"' was successfully updated.",sep=""))
        invisible(TRUE)
      },error=.capturedErrorToMessage)
    },
    
    # other getter / download functions ====
    downloadUserFile = function(src, dst=NULL) {
      tryCatch({
        if (!is.character(src)) {
          stop("Cannot download file with a source statement that is no character")
        } else {
          src = .urlHardEncode(src)
        }
        
        if (is.null(dst)) {
          dst = tempfile()
        }
        
        tag = "user_file_download"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id,src)
        
        file_connection = file(dst,open="wb")
        writeBin(object=private$GET(endpoint,authorized = TRUE,as = "raw"),con = file_connection)
        
        message("Successfully downloaded the requested file.")
        
        return(dst)
      },error=.capturedErrorToMessage,
      finally= {
        close(file_connection,type="wb")
      })
    },
    validateProcessGraph = function(graph) {
      tryCatch({
        tag = "process_graph_validate"
        endpoint = private$getBackendEndpoint(tag)
        
        if (!is.list(graph) || is.null(graph)) {
          stop("The graph information is missing or not a list")
        }
        
        requestBody = list(
          process_graph = graph
        )
        
        response = private$POST(endpoint=endpoint,
                                authorized = TRUE,
                                data=requestBody,
                                encodeType = "json")
        
        message("Graph was sucessfully validated.")
        invisible(response)
      },error = .capturedErrorToMessage)
    },
    
    cancel = function(job_id) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag = "jobs_cancel"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        if (success) {
          message(paste("Job '",job_id,"' has been successfully canceled.",sep=""))
        }
        
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    
    estimateCosts = function(job_id) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        tag = "jobs_cost_estimation"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$GET(endpoint = endpoint, authorized = TRUE)
        class(success) = "JobCostsEstimation"
        return(success)
      },error=.capturedErrorToMessage)
    },
    
    getProcessGraphBuilder = function() {
      tryCatch({
        if (is.null(private$graph_builder)) {
          private$graph_builder = ProcessGraphBuilder$new(con=self)
        }
        
        return(private$graph_builder)
      },error=.capturedErrorToMessage)
      
    }, 
    # processing functions ====
    execute = function (task=NULL,graph_id=NULL,output_file=NULL,format=NULL, ...) {
      tryCatch({
        # former sync evaluation
        tag = "execute_sync"
        endpoint = private$getBackendEndpoint(tag)
        
        if (is.null(format)) {
          format = self$output_formats()$default
        }
        
        output = list(...)
        output = append(output, list(format=format))
        if (!is.null(task)) {
          if (is.list(task)) {
            job = toJSON(list(process_graph=task,output = output),force=TRUE,auto_unbox = TRUE)
          } else {
            stop("Parameter task is not a task object. Awaiting a list.")
          }
        } else if (! is.null(graph_id)) {
          job = list(process_graph=graph_id,output = output)
        } else {
          stop("No process graph was defined. Please provide either a process graph id or a process graph description.")
        }
        
        header = list()
        header = private$addAuthorization(header)
        
        res = private$POST(endpoint,
                           authorized = TRUE, 
                           data=job,
                           encodeType = "json",
                           raw=TRUE)
        
        if (!is.null(output_file)) {
          tryCatch(
            {
              message("Task result was sucessfully stored.")
              writeBin(content(res,"raw"),output_file)
            },
            error = function(err) {
              stop(err)
            }
          )
          
          return(output_file)
        } else {
          return(content(res,"raw"))
        }
      },error=.capturedErrorToMessage)
      
    },
    
    orderResults = function(job_id) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag = "execute_async"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$POST(endpoint = endpoint, authorized = TRUE)
        message(paste("Job '",job_id,"' has been successfully queued for evaluation.",sep=""))
        
        invisible(self)
      },error=.capturedErrorToMessage)
      
    },
    
    #delete functions ====
    deleteUserFile = function (src) {
      tryCatch({
        if (is.character(src)) {
          src = .urlHardEncode(src)
        } else {
          stop("Cannot interprete parameter 'src' during delete request")
        }
        
        tag = "user_file_delete"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id, src)
        
        return(private$DELETE(endpoint = endpoint, authorized = TRUE))
      },error=.capturedErrorToMessage)
    }, 
    
    deleteGraph = function(graph_id) {
      tryCatch({
        tag = "graph_delete"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(graph_id)
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        if(success) {
          message(paste("Graph '",graph_id,"' was successfully deleted from the back-end",sep=""))
        }
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    deleteJob = function(job_id) {
      tryCatch({
        tag = "jobs_delete"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        if(success) {
          message(paste("Job '",job_id,"' was successfully deleted from the back-end",sep=""))
        }
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    
    deleteService = function(service_id) {
      tryCatch({
        tag = "services_delete"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(service_id)
        
        msg = private$DELETE(endpoint = endpoint,
                             authorized = TRUE)
        message("Service '",service_id,"' successfully removed.")
        invisible(msg)
      },error=.capturedErrorToMessage)
    }

  ),
  # private ----
  private = list(
    # attributes ====
    login_type = NULL, # basic or oidc
    login_token = NULL,
    oidc_client= NULL,
    user = NULL,
    password = NULL,
    host = NULL,
    graph_builder=NULL,
    processes = NULL,
    
    # functions ====
    isConnected = function() {
      return(!is.null(private$host))
    },
    loginOIDC = function() {
      if (!is.null(self$api.mapping)) {
        tag = "oidc_login"
        endpoint = private$getBackendEndpoint(tag)
      } 
      
      
      if (is.null(endpoint)) {
        stop("Cannot find endpoint for OIDC login")
      }
      suppressWarnings({
        tryCatch(
          {
            tryCatch({
              discovery_doc = private$GET(endpoint)
              private$oidc_client = OIDCClient$new(host=discovery_doc$issuer,discovery_document = discovery_doc)
              
              if (is.null(private$oidc_client)) {
                stop("OIDC client not initialized")
              }
              
              private$oidc_client$login()
              cat("Login successful.")
              
              return(invisible(self))
            },error=function(e){
              stop("Login failed.")
            })
            
          },
          error=.capturedErrorToMessage
        )
      })
      
    },
    
    loginBasic = function(user, password) {
      tryCatch({
        if (missing(user) || missing(password)) {
          stop("Username or password is missing.")
        }
        
        if (!is.null(self$api.mapping)) {
          tag = "login"
          endpoint = private$getBackendEndpoint(tag)
        } else {
          endpoint = "auth/login"
        }
        
        private$user = user
        private$password = password
        
        url = paste(private$host, endpoint, sep="/")
        res = GET(url=url,
                  config = authenticate(user=user,
                                        password = password,
                                        type = "basic")
        )
        
        if (is.debugging()) {
          print(res)
        }
        
        if (res$status_code == 200) {
          cont = content(res,type="application/json")
          
          private$login_token = cont$access_token
          self$user_id = cont$user_id
          
          if (is.null(self$api.mapping)) {
            self$api.mapping = endpoint_mapping(self)
          }
          cat("Login successful.")

          return(invisible(self))
        } else {
          stop("Login failed.")
        }
      },
      error = .capturedErrorToMessage,
      finally = {
      })
    },

    GET = function(endpoint,authorized=FALSE,query = list(), ...) {
      url = paste(private$host,endpoint, sep ="/")

      if (authorized && !self$disableAuth) {
        response = GET(url=url, config=private$addAuthorization(),query=query)
      } else {
        response = GET(url=url,query=query)
      }
  
      if (is.debugging()) {
        print(response)
      }

      if (response$status_code < 400) {
        info = content(response, ...)
        return(info)

      } else {
        private$errorHandling(response,url)
      }
    },
    DELETE = function(endpoint,authorized=FALSE,...) {
      url = paste(private$host,endpoint,sep="/")
      
      header = list()
      if (authorized && !self$disableAuth) {
        header = private$addAuthorization(header)
      }
      
      response = DELETE(url=url, config = header, ...)
      
      if (is.debugging()) {
        print(response)
      }
      
      message = content(response)
      
      if (response$status_code < 400) {
        if (response$status_code == 204) {
          message("Object was successfully deleted.")
          return(TRUE)
        }
      } else {
        private$errorHandling(response,url)
      }
      
      
    },
    POST = function(endpoint,authorized=FALSE,data=list(),encodeType = "json",query = list(), raw=FALSE,...) {
      url = paste(private$host,endpoint,sep="/")
      if (!is.list(query)) {
        stop("Query parameters are no list of key-value-pairs")
      }
      
      if (is.character(data)) {
        data = fromJSON(data,simplifyDataFrame = FALSE)
      }
      
      if (is.list(data)) {
        
        header = list()
        if (authorized && !self$disableAuth) {
          header = private$addAuthorization(header)
        }
        
        # create json and prepare to send graph as post body
        
        response=POST(
          url= url,
          config = header,
          query = query,
          body = data,
          encode = encodeType
        )
        
        if (is.debugging()) {
          print(response)
        }
        
        if (response$status_code < 400) {
          if (raw) {
            return(response)
          } else {
            okMessage = content(response,"parsed","application/json")
            
            return(okMessage)
          }
        } else {
          private$errorHandling(response,url)
        }
      } else {
        stop("Cannot interprete data - data is no list that can be transformed into json")
      }
    },
    PUT = function(endpoint, authorized=FALSE, data, encodeType = NULL, ...) {
      url = paste(private$host,endpoint,sep="/")
      
      header = list()
      if (authorized && !self$disableAuth) {
        header = private$addAuthorization(header)
      }
      
      params = list(url=url, 
                    config = header, 
                    body=data)
      if (!is.null(encodeType)) {
        params = append(params, list(encode = encodeType))
      }
      response = do.call("PUT", args = params)
      
      if (is.debugging()) {
        print(response)
      }
      
      if (response$status_code < 400) {
        if (response$status_code == 204) {
          return(TRUE)
        }
        
        # TODO might be never reached
        okMessage = content(response,"parsed","application/json")
        return(okMessage)
      } else {
        private$errorHandling(response,url)
      }
    },
    PATCH = function(endpoint, authorized=FALSE, data=NULL, encodeType = NULL, ...) {
      url = paste(private$host,endpoint,sep="/")
      
      header = list()
      if (authorized && !self$disableAuth) {
        header = private$addAuthorization(header)
      }
      
      params = list(url=url, 
                    config = header)
      
      if (!is.null(data)) {
        params = append(params, list(body = data))
      }
      
      if (!is.null(encodeType)) {
        params = append(params, list(encode = encodeType))
      }
      response = do.call("PATCH", args = params)
      
      if (is.debugging()) {
        print(response)
      }
      
      if (response$status_code < 400) {
        if (response$status_code == 204) {
          return(TRUE)
        }
        
        okMessage = content(response,"parsed","application/json")
        return(okMessage)
      } else {
        private$errorHandling(response,url)
      }
    },
    getBackendEndpoint = function(endpoint_name) {
      if (!is.null(self$api.mapping)) {
        endpoint = self$api.mapping %>% filter(tag==endpoint_name,available) %>% dplyr::select(backend_endpoint) %>% unname() %>% unlist()
        if (length(endpoint) > 0) {
          if (startsWith(endpoint,"/")) {
            return(substr(endpoint,2,nchar(endpoint)))
          } else {
            return(endpoint)
          }
        } else {
          stop("Endpoint for this function is not supported by the current back-end.")
        }
        
      } else {
        stop("Not connected / loggedin. You need to connect or login to a back-end.")
      }
    },
    
    # returns the header list and adds Authorization
    addAuthorization = function (header) {
      if (missing(header)) {
        header = list()
      }

      if (self$general_auth_type == "bearer") {
        if (private$login_type == "basic") {
          header = append(header,add_headers(
            Authorization=paste("Bearer",private$login_token, sep =" ")
          ))
        } else if (private$login_type == "oidc") {
          header = append(header,add_headers(
            Authorization=paste("Bearer",private$oidc_client$access_token, sep =" ")
          ))
        }
        
      } else { # if all the endpoints require a basic encoded authorization header
        header = append(header,authenticate(private$user,private$password,type = self$general_auth_type))
      }

      return(header)
    },
    stopIfNotConnected = function() {
      if (!private$isConnected()) {
        stop("Not connected to a back-end. Please connect to one before proceeding")
      }
    },
    errorHandling = function(response,url) {
      if (class(response) == "response") {
        errorMessage = content(response)
        if (!is.null(errorMessage[["message"]])) {
          stop(errorMessage[["message"]])
        } else {
          # if there is an uncaptured error from the server then just return it as is
          stop(paste("SERVER-ERROR:",errorMessage))
        }
      } else {
        # never happens? it is something else than response object
        stop(response)
      }
    }
  )

)

# statics -----
.urlHardEncode=function(text) {
  text = URLencode(text)
  text = gsub("\\/","%2F",text)
  text = gsub("\\.","%2E",text)
  return(text)
}

.capturedErrorToMessage = function(e) {
  message(e)
  invisible(NULL)
}
