#' OpenEO client class
#'
#' A R6Class that interacts with an openEO-conformant backend.
#'
#' @importFrom R6 R6Class
#' @import httr
#' @import magrittr
#' @import jsonlite
#' @importFrom lubridate as_datetime
#' @export
OpenEOClient <- R6Class(
  "OpenEOClient",
  # public ----
  public = list(
    # attributes ====
    disableAuth = TRUE,
    general_auth_type = "bearer",
    user_id = NULL,
    
    api.version = "0.3.0",
    api.mapping = NULL,
    
    products = list(),
    processes = list(),

    # functions ====
    initialize = function() {

    },

    connect = function(url) {
      if (!missing(url)) {
        tryCatch({
          if (endsWith(url,"/")) {
            url = substr(url,1,nchar(url)-1)
          }
          private$host = url
          # cat(paste("Registered '",url,"' as host","\n",sep=""))
          cat("Registered host\n")
        
          self$api.mapping = endpoint_mapping(self)
          invisible(self)
        }, error = function(e){
          invisible(self)
        }
        )
        
      } else {
        stop("Host-URL is missing")
      }

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
        
        return(formats)
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

    login = function(user, password, auth_type="basic") {
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

        private$stopIfNotConnected()
        
        private$user = user
        private$password = password
        
        url = paste(private$host, endpoint, sep="/")
        res = GET(url=url,
                  config = authenticate(user=user,
                                        password = password,
                                        type = auth_type)
        )
        
        if (res$status_code == 200) {
          cont = content(res,type="application/json")
          
          private$login_token = cont$access_token
          self$user_id = cont$user_id
          
          cat("Login successful." )
          
          if (is.null(self$api.mapping)) {
            self$api.mapping = endpoint_mapping(self)
          }
          
          invisible(self)
        } else {
          stop("Login failed.")
        }
      },
      error = .capturedErrorToMessage,
      finally = {
        
      }
      )
      
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
        table = tibble(product_id = character(),
                       description = character(),
                       source = character())
        for (index in 1: length(listOfProducts)) {
          product = listOfProducts[[index]]
          
          product_id = product$data_id
          if ("description" %in% names(product)) {
            description = product$description
          } else {
            description = NA
          }
          
          if ("source" %in% names(product)) {
            source = product$source
          } else {
            source = NA
          }
          
          
          table = table %>% add_row(product_id = product_id,
                                    description = description,
                                    source = source)
        }
        
        return(table)
      },
      error=.capturedErrorToMessage)
    },
    
    listProcesses = function() {

      tryCatch({
        tag = "process_overview"
        endpoint = private$getBackendEndpoint(tag)
        
        listOfProcesses = private$GET(endpoint,type="application/json")
        
        # table = tibble(process_id = character(),
        #                description = character())
        # 
        # for (index in 1:length(listOfProcesses)) {
        #   process = listOfProcesses[[index]]
        #   table = table %>% add_row(process_id = process$process_id,
        #                             description = process$description)
        # }
        
        
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
        # list to tibble
        table = tibble(job_id=character(),
                       status=character(),
                       submitted=.POSIXct(integer(0)),
                       updated=.POSIXct(integer(0)),
                       consumed_credits=integer(0))
        
        for (index in 1:length(listOfJobs)) {
          job = listOfJobs[[index]]
          table= add_row(table,
                         job_id=job$job_id,
                         status = job$status,
                         submitted = as_datetime(job$submitted),
                         updated = as_datetime(job$updated),
                         consumed_credits = job$consumed_credits)
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
        
        table = tibble(process_graph_id=character(),
                       title=character(),
                       description=character())
        
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
        
        return(table)
      }, error = .capturedErrorToMessage)
    },
    
    listServices = function() {
      
      tryCatch({
        tag = "user_services"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id)
        
        listOfServices = private$GET(endpoint,authorized = TRUE ,type="application/json")
        table = tibble(service_id=character(),
                       service_type=character(),
                       service_args=list(),
                       job_id=character(),
                       service_url=character())
        
        for (index in 1:length(listOfServices)) {
          service = listOfServices[[index]]
          
          if (!is.null(service$service_args) && length(service$service_args) == 0) {
            service$service_args <- NULL
          }
          table= do.call("add_row",append(list(.data=table),service))
        }
        
        return(table)
      }, error = .capturedErrorToMessage)
    },
    
    listUserFiles = function() {
      
      tryCatch({
        tag = "user_files"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id)
        
        files = private$GET(endpoint,TRUE,type="application/json")
        
        if (is.null(files) || length(files) == 0) {
          message("The user workspace at this host is empty.")
          return(invisible(files))
        }
        
        files = tibble(files) %>% rowwise() %>% summarise(name=files$name, size=files$size)
        
        return(files)
      },error=.capturedErrorToMessage)
    },
    
    # describe functions ####
    describeProcess = function(pid) {
      tryCatch({
        stop("This is not provided anymore in API v0.3.0. Use listProcess to get a detailed overview over all processes")
        
        tag = "processes_details"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(pid)
        
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)
        
        # info is currently a list 
        # make a class of it and define print
        class(info) <- "ProcessInfo"
        
        return(info)
      },error=.capturedErrorToMessage)
    },
    
    describeProduct = function(pid) {
      
      tryCatch({
        tag = "data_details"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(pid)
        
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)

        info = private$modifyProductList(info)
        class(info) = "openeo_product"
        return(info)
      },
      error = .capturedErrorToMessage)
    },
    
    describeJob = function(job_id) {
      
      tryCatch({
        tag = "jobs_details"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        info = private$GET(endpoint = endpoint,authorized = TRUE, type="application/json",auto_unbox=TRUE)
        table = tibble(job_id = info$job_id,
                       status = info$status,
                       process_graph = list(info$process_graph),
                       output = list(info$output),
                       submitted = as_datetime(info$submitted),
                       updated = as_datetime(info$updated),
                       user_id = info$user_id,
                       consumed_credits = info$consumed_credits)
        
        return(table)
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
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(service_id)
        
        return(private$GET(endpoint,authorized = TRUE))
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
    
    replaceGraph = function(graph_id, graph) {
      tryCatch({
        if (is.null(graph_id)) {
          stop("Cannot replace unknown graph. If you want to store the graph, use 'storeGraph' instead")
        }
        if (is.null(graph)) {
          stop("Cannot replace graph with 'NULL'")
        }
        if (!is.list(graph) || is.null(graph)) {
          stop("The graph information is missing or not a list")
        }
        
        tag = "graph_replace"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id,graph_id)
        
        message = private$PUT(endpoint = endpoint, 
                              authorized = TRUE, 
                              data = graph,
                              encodeType = "json")
        
        return(message) #in principle a void function
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
        
        message("Successfully uploaded the udf script.")
        
        return(dst)
      },error=.capturedErrorToMessage,
      finally=function() {
        close(file_connection,type="wb")
      })
    },
    
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
        return(split[length(split)])
      },error = .capturedErrorToMessage)
    },
    
    storeJob = function(task=NULL,graph_id=NULL,format, ...) {
      tryCatch({
        tag = "jobs_define"
        endpoint = private$getBackendEndpoint(tag)
        
        output = list(...)
        output = append(output, list(format=format))
        
        if (!is.null(task)) {
          if (is.list(task)) {
            job = list(process_graph=task,output = output)
          } else {
            stop("Parameter task is not a task object. Awaiting a list.")
          }
        } else if (! is.null(graph_id)) {
          job = list(process_graph=graph_id,output = output)
        } else {
          stop("No process graph was defined. Please provide either a process graph id or a process graph description.")
        }
        
        #endpoint,authorized=FALSE,data,encodeType = "json",query = list(),...
        okMessage = private$POST(endpoint=endpoint,
                                 authorized = TRUE,
                                 data=job)
        
        message("Job was sucessfully registered on the backend.")
        return(okMessage$job_id)
      },error=.capturedErrorToMessage)
    },
    
    modifyJob = function(job_id,...) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job i was specified.")
        }
        
        tag = "jobs_update"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        updateables = list(...)
        
        patch = list()
        graph = NULL
        if ("graph_id" %in% names(updateables)) {
          graph = updateables$graph_id
          updateables$graph_id = NULL
        } else if ("task" %in% names(updateables)) {
          graph = updateables$task
          updateables$task = NULL
        }
        if (!is.null(graph)) {
          patch = append(patch,list(process_graph = graph))
        }
        
        if (length(updateables) > 0) {
          patch = append(patch, list(output=updateables))
        }
        
        #endpoint, authorized=FALSE, data=NULL, encodeType = NULL, ...
        res = private$PATCH(endpoint = endpoint,
                            authorized = TRUE,
                            encodeType = "json",
                            data=patch)
        message(paste("Job '",job_id,"' was successfully updated.",sep=""))
        return(res)
      },error=.capturedErrorToMessage)
    },
    
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
            job = list(process_graph=task,output = output)
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
    
    queue = function(job_id) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag = "execute_async"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$PATCH(endpoint = endpoint, authorized = TRUE)
        message(paste("Job '",job_id,"' has been successfully queued for evaluation.",sep=""))
        
        invisible(self)
      },error=.capturedErrorToMessage)
      
    },
    
    results = function(job_id, format = NULL) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag= "jobs_download"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)

        supportedFormats = names(self$output_formats()$formats)
        if (!is.null(format) && format %in% supportedFormats) {
          return(private$GET(endpoint = endpoint,
                             authorized = TRUE,
                             query=list(
                               format=format
                             )))
        } else {
          return(private$GET(endpoint = endpoint,
                             authorized = TRUE))
        }
      },error=.capturedErrorToMessage)
    },
    
    pause = function(job_id) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag = "jobs_pause"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$PATCH(endpoint = endpoint, authorized = TRUE)
        message(paste("Job '",job_id,"' has been successfully paused.",sep=""))
        return(success)
      },error=.capturedErrorToMessage)
    },
    
    cancel = function(job_id) {
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag = "jobs_cancel"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$PATCH(endpoint = endpoint, authorized = TRUE)
        message(paste("Job '",job_id,"' has been successfully canceled.",sep=""))
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    
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
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id, graph_id)
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        message(paste("Graph '",graph_id,"' was successfully deleted from the back-end",sep=""))
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    
    deleteService = function(service_id) {
      tryCatch({
        tag = "services_delete"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(service_id)
        
        msg = private$DELETE(endpoint = endpoint,
                             authorized = TRUE)
        message("Service '",service_id,"' successfully deactivated")
        invisibile(msg)
      },error=.capturedErrorToMessage)
    },
    
    getUserCredits = function() {
      tryCatch({
        tag = "user_credits"
        endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id)
        
        return(private$GET(endpoint,authorized = TRUE))
      },error=.capturedErrorToMessage)
    },
    
    createService = function(job_id, service_type, ...) {
      tryCatch({
        if (is.null(job_id)) {
          stop("Cannot create service. job_id is missing.")
        }
        if (is.null(service_type)) {
          stop("No service_type specified.")
        }

        tag = "service_publish"
        endpoint = private$getBackendEndpoint(tag)
        
        service_args = list(...)
        
        service_request_object = list(
          job_id = job_id,
          service_type = service_type,
          service_args = service_args
        )
        
        response = private$POST(endpoint,
                                authorized = TRUE, 
                                data = service_request_object, 
                                encodeType = "json")
        
        message("Service was successfully created.")
        return(response)
      },error=.capturedErrorToMessage)
    },
    
    modifyService = function(service_id) {
      tag = "services_update"
      endpoint = private$getBackendEndpoint(tag) %>% replace_endpoint_parameter(service_id)
      
      .not_implemented_yet()
    },
    
    getProcessGraphBuilder = function() {
      tryCatch({
        if (is.null(private$graph_builder)) {
          private$graph_builder = ProcessGraphBuilder$new(self)
        }
        
        return(private$graph_builder)
      },error=.capturedErrorToMessage)
      
    }

  ),
  # private ----
  private = list(
    # attributes ====
    login_token = NULL,
    user = NULL,
    password = NULL,
    host = NULL,
    graph_builder=NULL,
    
    # functions ====
    isConnected = function() {
      return(!is.null(private$host))
    },

    GET = function(endpoint,authorized=FALSE,query = list(), ...) {
      url = paste(private$host,endpoint, sep ="/")

      if (authorized && !self$disableAuth) {
        response = GET(url=url, config=private$addAuthorization(),query=query)
      } else {
        response = GET(url=url,query=query)
      }


      if (response$status_code %in% c(200)) {
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
      
      message = content(response)
      success = response$status_code %in% c(200,202,204)
      if (success) {
        tmp = lapply(message, function(elem) {
          message(elem)
        })
      } else {
        if (!is.null(message) && is.list(message)) {
          warning(message[["message"]])
        } else {
          warning(message)
        }
      }
      
      return(success)
    },
    POST = function(endpoint,authorized=FALSE,data,encodeType = "json",query = list(), raw=FALSE,...) {
      url = paste(private$host,endpoint,sep="/")
      if (!is.list(query)) {
        stop("Query parameters are no list of key-value-pairs")
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
        success = response$status_code %in% c(200,201,202,204)
        if (success) {
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
      
      success = response$status_code %in% c(200,202,204)
      if (success) {
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
      
      success = response$status_code %in% c(200,202,204)
      if (success) {
        okMessage = content(response,"parsed","application/json")
        return(okMessage)
      } else {
        private$errorHandling(response,url)
      }
    },
    modifyProductList = function(product) {
      if (is.list(product) && any(c("collection_id","product_id","data_id") %in% names(product))) {
        if ("spatial_extent" %in% names(product)) {
          e = product$spatial_extent
  
          ext = sp::bbox(matrix(nrow=2,ncol=2,c(e$left,e$right,e$bottom,e$top)))
          product$spatial_extent = ext
          
          if ("crs" %in% names(e)) {

            if (grepl("EPSG",toupper(e$crs))) {
              product$crs = sp::CRS(paste("+init=",e$crs,sep="")) # epsg code
            } else if (startsWith(e$crs,"+")) {
              product$crs = sp::CRS(e$crs) # proj4string (potentially)
            } else {
              product$crs = e$crs
              warning("Cannot interprete SRS statement (no EPSG code or PROJ4 string).")
            }
            
            
           
          }
        }
        
        if ("temporal_extent" %in% names(product)) {
          range = unlist(strsplit(product$temporal_extent, "/"))
          
          t_range = list(from = as_datetime(range[[1]]),
                         to = as_datetime(range[[2]])
                         )
          
          product$temporal_extent = t_range
        }

        return(product)

      } else {
        stop("Object that is modified is not the list result of product.")
      }
    },
    
    getBackendEndpoint = function(endpoint_name) {
      if (!is.null(self$api.mapping)) {
        endpoint = self$api.mapping %>% filter(tag==endpoint_name,available) %>% select(backend_endpoint) %>% unname() %>% unlist()
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
        header = append(header,add_headers(
          Authorization=paste("Bearer",private$login_token, sep =" ")
        ))
      } else {
        header = append(header,authenticate(private$user,private$password,type = self$general_auth_type))
      }

      return(header)
    },
    stopIfNotConnected = function() {
      if (!private$isConnected()) {
        stop(.notConnected())
      }
    },
    errorHandling = function(response,url) {
      if (response$status_code %in% c(400,404)) {
        stop(paste("REQUEST-ERROR: Cannot find or access endpoint ","'",url,"'.",sep=""))
      } else if (response$status_code == 401) {
        stop(.notLoggedInOrExpired())
      } else if (response$status_code == 403) {
        stop(.notAuthorized())
      } else {
        errorMessage = content(response)
        if (!is.null(errorMessage) && is.list(errorMessage)) {
          stop(paste("SERVER-ERROR:",errorMessage[["message"]]))
        } else {
          stop(paste("SERVER-ERROR:",errorMessage))
        }
        
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



