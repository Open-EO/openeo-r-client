#' OpenEO client class
#'
#' A R6Class that interacts with an openEO-conformant backend.
#'
#' @importFrom R6 R6Class
#' @import httr
#' @import magrittr
#' @import jsonlite
#' @importFrom raster extent
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom lubridate as_datetime
#' @export
OpenEOClient <- R6Class(
  "OpenEOClient",
  # public ----
  public = list(
    # attributes ====
    disableAuth = FALSE,
    general_auth_type = "bearer",
    user_id = NULL,
    
    is_rserver = FALSE,
    api.version = "0.0.2",
    
    products = list(),
    processes = list(),

    # functions ====
    initialize = function() {

    },

    connect = function(url) {
      if (!missing(url)) {
        if (endsWith(url,"/")) {
          url = substr(url,1,nchar(url)-1)
        }
        private$host = url
        # cat(paste("Registered '",url,"' as host","\n",sep=""))
        cat("Registered host\n")
        invisible(self)
      } else {
        stop("Host-URL is missing")
      }

    },
    capabilities = function() {
      endpoint = "capabilities"
      tryCatch({
        private$stopIfNotConnected()
        
        capabilities = private$GET(endpoint = endpoint,authorized = FALSE)
        
        return(capabilities)
      },
      error = .capturedErrorToMessage)
      
    },
    services = function() {
      endpoint = "capabilities/services"
      
      tryCatch({
        private$stopIfNotConnected()
        
        services = private$GET(endpoint, authorized = FALSE)
        return(services)
      },error=.capturedErrorToMessage)
    },
    output_formats = function() {
      endpoint = "capabilities/output_formats"
      
      tryCatch({
        formats = private$GET(endpoint,authorized = FALSE)
        
        return(formats)
      },
      error = .capturedErrorToMessage
      )
      
    },
    udf_runtimes = function() {
      endpoint = "udf_runtimes"
      tryCatch(
        {
          return(private$GET(endpoint = endpoint,
                             authorized = FALSE))
        }, 
        error = .capturedErrorToMessage
      )
    },
    
    register = function(user=NULL,password) {
      #currently this will be used for GEE only
      endpoint = "auth/register"
      
      tryCatch({
        if (!private$isConnected()) {
          stop("No host selected")
        }
        
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
      endpoint = "auth/login"
      
      tryCatch({
        if (missing(user) || missing(password)) {
          stop("Username or password is missing.")
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
          
          private$login_token = cont$token
          self$user_id = cont$user_id
          
          cat("Login successful." )
          invisible(self)
        } else {
          stop("Login failed.")
        }
      },
      error = .capturedErrorToMessage
      )
      
    },
    # list functions ####
    listData = function() {
      if (self$is_rserver) {
        endpoint = "data/"
      } else {
        endpoint = "data"
      }
      
      tryCatch({
        listOfProducts = private$GET(endpoint=endpoint,type="application/json")
        table = tibble(product_id = character(),
                       description = character(),
                       source = character())
        for (index in 1: length(listOfProducts)) {
          product = listOfProducts[[index]]
          
          product_id = product$product_id
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
      
      if (self$is_rserver) {
        endpoint = "processes/"
      } else {
        endpoint = "processes"
      }
      
      tryCatch({
        listOfProcesses = private$GET(endpoint,type="application/json")
        
        table = tibble(process_id = character(),
                       description = character())
        
        for (index in 1:length(listOfProcesses)) {
          process = listOfProcesses[[index]]
          table = table %>% add_row(process_id = process$process_id,
                                    description = process$description)
        }
        
        return(table)
      },
      error=.capturedErrorToMessage)
      
    },
    listJobs = function() {
      endpoint = paste("users",self$user_id,"jobs",sep="/")
      
      tryCatch({
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
      endpoint = paste("users",self$user_id,"process_graphs",sep="/")
      
      tryCatch({
        listOfGraphIds = private$GET(endpoint, authorized = TRUE)
        
        return(listOfGraphIds)
      }, error = .capturedErrorToMessage)
    },
    listServices = function() {
      endpoint = paste("users",self$user_id,"services",sep="/")
      
      tryCatch({
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
      endpoint = paste("users",self$user_id,"files",sep="/")
      
      tryCatch({
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
      endpoint = paste("processes",pid,sep="/")
      
      tryCatch({
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)
        
        # info is currently a list 
        # make a class of it and define print
        class(info) <- "ProcessInfo"
        
        return(info)
      },error=.capturedErrorToMessage)
    },
    describeProduct = function(pid) {
      endpoint = paste("data",pid,sep="/")
      
      tryCatch({
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)

      
        info = private$modifyProductList(info)
        class(info) = "openeo_product"
        return(info)
      },
      error = .capturedErrorToMessage)
    },
    describeJob = function(job_id) {
      endpoint = paste("jobs",job_id,sep="/")
      
      tryCatch({
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
    describeGraph = function(graph_id, user_id = NULL) {
      tryCatch(
        {
          if (is.null(graph_id)) {
            stop("No graph id specified. Cannot fetch unknown graph.")
          }
          
          if (is.null(user_id)) {
            user_id = self$user_id #or "me"
          }
          
          endpoint = paste("users", user_id, "process_graphs", graph_id, sep="/")
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
        endpoint = paste("services",service_id,sep="/")
        
        return(private$GET(endpoint,authorized = TRUE))
      }, error=.capturedErrorToMessage)
    },
    describeUdfType = function(language, udf_type) {
      tryCatch({
        if (is.null(language) || is.null(udf_type)) {
          stop("Missing parameter language or udf_type")
        }
        
        endpoint = paste("udf_runtimes",language,udf_type,sep="/")
        
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
        endpoint = paste("users",self$user_id,"process_graphs",graph_id,sep="/")
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
        
        endpoint = paste("users",self$user_id,"files",target,sep="/")
        
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
        
        endpoint = paste("users",self$user_id,"files",src,sep="/")
        file_connection = file(dst,open="wb")
        writeBin(object=private$GET(endpoint,authorized = TRUE,as = "raw"),con = file_connection)
        
        message("Successfully uploaded the udf script.")
        
        return(dst)
      },error=.capturedErrorToMessage,
      finally=function() {
        close(file_connection,type="wb")
      })
    },
    storeGraph = function(graph) {
      tryCatch({
        endpoint = paste("users",self$user_id,"process_graphs",sep="/")
        
        if (!is.list(graph) || is.null(graph)) {
          stop("The graph information is missing or not a list")
        }
        
        okMessage = private$POST(endpoint=endpoint,
                                 authorized = TRUE,
                                 data=graph)
        
        message("Graph was sucessfully stored on the backend.")
        return(okMessage$process_graph_id)
      },error = .capturedErrorToMessage)
    },
    storeJob = function(task=NULL,graph_id=NULL,format, ...) {
      tryCatch({
        if (self$is_rserver) {
          endpoint = paste("jobs/",sep="/")
        } else {
          endpoint = paste("jobs",sep="/")
        }
        if (is.null(format)) {
          # format = self$output_formats()$default
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
        endpoint = paste("jobs",job_id,sep="/")
        
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
        if (self$is_rserver) {
          endpoint = paste("execute/",sep="/")
        } else {
          endpoint = paste("execute",sep="/")
        }
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
        endpoint = paste("jobs",job_id,"queue",sep="/")
        
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
        
        endpoint = paste("jobs",job_id,"download",sep="/")
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
        endpoint = paste("jobs",job_id,"pause",sep="/")
        
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
        endpoint = paste("jobs",job_id,"cancel",sep="/")
        
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
        endpoint = paste("users",self$user_id,"files",src,sep="/")
        
        return(private$DELETE(endpoint = endpoint, authorized = TRUE))
      },error=.capturedErrorToMessage)
      
    }, 
    deleteGraph = function(graph_id) {
      tryCatch({
        endpoint = paste("users",self$user_id,"process_graphs",graph_id,sep="/")
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        message(paste("Graph '",graph_id,"' was successfully deleted from the back-end",sep=""))
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    deleteService = function(service_id) {
      tryCatch({
        endpoint = paste("services",service_id,sep="")
        
        msg = private$DELETE(endpoint = endpoint,
                             authorized = TRUE)
        message("Service '",service_id,"' successfully deactivated")
        invisibile(msg)
      },error=.capturedErrorToMessage)
     
    },
    getUserCredits = function() {
      tryCatch({
        endpoint = paste("users",self$user_id,"credits",sep="/")
        
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
        if (self$is_rserver) {
          endpoint = "services/"
        } else {
          endpoint = "services"
        }
        
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
    modifyService = function() {
      
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
        
        success = response$status_code %in% c(200,202,204)
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
      if (is.list(product) && any(c("collection_id","product_id") %in% names(product))) {
        if ("extent" %in% names(product)) {
          e = product$extent
  
          ext = extent(e$left,e$right,e$bottom,e$top)
          product$extent = ext
          
          if ("srs" %in% names(e)) {
            product$crs = gdalsrsinfo(e$srs, as.CRS = TRUE)
          }
        }
        
        if ("time" %in% names(product)) {
          if ("from" %in% names(product$time)) {
            product$time$from = as_datetime(product$time$from)
          }
          if ("to" %in% names(product$time)) {
            product$time$to = as_datetime(product$time$to)
          }
        }

        return(product)

      } else {
        stop("Object that is modified is not the list result of product.")
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
        if (!is.null(response) && is.list(response)) {
          stop(paste("SERVER-ERROR:",response[["message"]]))
        } else {
          stop(paste("SERVER-ERROR:",response))
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

#' @export
print.openeo_product = function(x, ...) {
  return(str(x))
}

.capturedErrorToMessage = function(e) {
  message(e)
  invisible(NULL)
}



