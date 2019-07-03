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
    user_id = NULL,
    
    api.mapping = NULL,
    processes = NULL,

    # functions ====
    initialize = function() {

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
    
    request = function(operation,endpoint, authorized=FALSE, ...) {
      http_operation = private[[toupper(operation)]]
      
      return(do.call(http_operation, append(list(endpoint=endpoint,authorized=authorized),list(...))))
    },
    isConnected = function() {
      return(!is.null(private$host))
    },
    stopIfNotConnected = function() {
      if (!self$isConnected()) {
        stop("Not connected to a back-end. Please connect to one before proceeding")
      }
    },

    connect = function(url,version, login_type=NULL) {
      if (!is.null(login_type)) {
        private$disableAuth = FALSE
        login_type = tolower(login_type)
      }
      
      tryCatch({
        if (!is.null(login_type) && !login_type %in% c("basic","oidc") ) {
          stop("Cannot find the login mechanism type. Please use 'basic' or 'oidc'")
        }
        
        if (!missing(url)) {
          
            if (endsWith(url,"/")) {
              url = substr(url,1,nchar(url)-1)
            }
            private$host = url
            private$login_type=login_type
            
            if (!is.null(version)) {
              # url is not specific, then resolve /.well-known/openeo and check if the version is allowed
              hostInfo=private$backendVersions()$versions
              versionLabels = sapply(hostInfo,function(x)x$api_version)
              names(hostInfo) = versionLabels
              
              if (!version %in% versionLabels) {
                # print the available versions
                message(paste("Version",version,"is not provided by the back-end. Please choose one of the following",
                              paste(versionLabels,collapse=",")))
              } else {
                url = hostInfo[[version]]$url
                
                if (endsWith(url,"/")) {
                  url = substr(url,1,nchar(url)-1)
                }
                private$host = url
              }
            }
            
            self$api.mapping = endpoint_mapping(self)
            cat("Connected to host\n")
            
            tryCatch({
              if (is.null(self$processes)) {
                processes = list_processes(self)
                pids = sapply(processes, function(x)x$id)
                names(processes) = pids
                self$processes = processes
              }
              
            }, error = function(e){
              self$processes = NULL
            })
            return(invisible(self))
        } else {
          message("Note: Host-URL is missing")
          return(self)
        }
      }, 
      error = .capturedErrorToMessage
      )

    },
    client_version = function () {
      return(private$version)
    },
    
    register = function(user=NULL,password) {
      #currently this will be used for GEE only
      tryCatch({
        if (!self$isConnected()) {
          stop("No host selected")
        }
        
        tag = "registration"
        endpoint = self$getBackendEndpoint(tag)
        
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
        if (private$host %>% is.null()) {
          stop("Cannot login. Please connect to an OpenEO back-end first.")
        }
        self$stopIfNotConnected()
        
        if (private$login_type == "oidc") {
          private$loginOIDC()
        } else if (private$login_type == "basic") {
          private$loginBasic(user=user, password = password)
        } else {
          if (private$disableAuth) {
            return(self)
          } else {
            stop("Unsupported login mechanism")
          }
        }
        
        tryCatch({
          if (is.null(self$processes)) {
            processes = list_processes(self)
            pids = sapply(processes, function(x)x$id)
            names(processes) = pids
            self$processes = processes
          }
        }, error = function(e){
          self$processes = NULL
        })
        
        
        invisible(self)
        
      },
      error=.capturedErrorToMessage)
    },
    logout = function() {
      if (!is.null(private$oidc_client)){
        private$oidc_client$logout()
      }
    },
    
    # Update functions ====
    
    update_process_graph = function(id, graph=NULL,title = NULL, description = NULL) {
      tryCatch({
        if (is.null(id)) {
          stop("Cannot replace unknown graph. If you want to store the graph, use 'create_process_graph' instead")
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
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
        
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
    
    update_job = function(id,
                         title=NULL, description=NULL,
                         process_graph = NULL, 
                         plan = NULL, budget= NULL,
                         format=NULL, ...) {
      tryCatch({
        if (is.null(id)) {
          stop("No job i was specified.")
        }
        
        tag = "jobs_update"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
        
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
    
    update_service = function(id,
                             type=NULL, 
                             process_graph=NULL,
                             title = NULL,
                             description = NULL,
                             enabled = NULL,
                             parameters = NULL,
                             plan = NULL,
                             budget = NULL) {
      tag = "services_update"
      endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
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
    download_file = function(src, dst=NULL) {
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
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id,src)
        
        file_connection = file(dst,open="wb")
        writeBin(object=private$GET(endpoint,authorized = TRUE,as = "raw"),con = file_connection)
        
        message("Successfully downloaded the requested file.")
        
        return(dst)
      },error=.capturedErrorToMessage,
      finally= {
        close(file_connection,type="wb")
      })
    },
    validate_process_graph = function(graph) {
      tryCatch({
        tag = "process_graph_validate"
        endpoint = self$getBackendEndpoint(tag)
        
        if ("Graph" %in% class(graph)) graph = graph$serialize()
        
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
    
    stop_job = function(job) {
      if (!is.null(job) && "JobInfo" %in% class(job)) {
        job_id = job$id
      } else {
        job_id = job
      }
      
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag = "jobs_cancel"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        if (success) {
          message(paste("Job '",job_id,"' has been successfully canceled.",sep=""))
        }
        
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    
    estimate_job = function(job) {
      if (!is.null(job) && "JobInfo" %in% class(job)) {
        job_id = job$id
      } else {
        job_id = job
      }
      
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        tag = "jobs_cost_estimation"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$GET(endpoint = endpoint, authorized = TRUE)
        class(success) = "JobCostsEstimation"
        return(success)
      },error=.capturedErrorToMessage)
    },
    
    process_graph_builder = function() {
      tryCatch({
        if (is.null(self$processes)) {
          tag = "process_overview"
          endpoint = self$getBackendEndpoint(tag)
          
          listOfProcesses = private$GET(endpoint,type="application/json")
          self$processes = listOfProcesses$processes
        }
        # json processes -> process objects
        
        names(self$processes) = sapply(self$processes,function(p)p$id)
        
        collections = list_collections(self)$collections
        cids = sapply(collections,function(coll)coll$id)
        collections = as.list(cids)
        names(collections) = cids
        
        
        plist = lapply(self$processes,processFromJson)
        
        return(Graph$new(plist,collections))
      },error=.capturedErrorToMessage)
      
    }, 
    # processing functions ====
    compute_result = function (graph=NULL,graph_id=NULL,output_file=NULL,format=NULL, ...) {
      tryCatch({
        # former sync evaluation
        tag = "execute_sync"
        endpoint = self$getBackendEndpoint(tag)
        if (is.null(format)) {
          format = self$output_formats()$default
        }
        
        output = list(...)
        output = append(output, list(format=format))
        if (!is.null(graph)) {
          if (is.list(graph)) {
            job = toJSON(list(process_graph=graph,output = output),force=TRUE,auto_unbox = TRUE)
          } else {
            stop("Parameter graph is not a Graph object. Awaiting a list.")
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
    
    start_job = function(job) {
      if (!is.null(job) && "JobInfo" %in% class(job)) {
        job_id = job$id
      } else {
        job_id = job
      }
      
      tryCatch({
        if (is.null(job_id)) {
          stop("No job id specified.")
        }
        
        tag = "execute_async"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$POST(endpoint = endpoint, authorized = TRUE)
        message(paste("Job '",job_id,"' has been successfully queued for evaluation.",sep=""))
        
        invisible(self)
      },error=.capturedErrorToMessage)
      
    },
    
    #delete functions ====
    delete_file = function (src) {
      tryCatch({
        if (is.character(src)) {
          src = .urlHardEncode(src)
        } else {
          stop("Cannot interprete parameter 'src' during delete request")
        }
        
        tag = "user_file_delete"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(self$user_id, src)
        
        return(private$DELETE(endpoint = endpoint, authorized = TRUE))
      },error=.capturedErrorToMessage)
    }, 
    
    delete_process_graph = function(id) {
      tryCatch({
        tag = "graph_delete"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        if(success) {
          message(paste("Graph '",id,"' was successfully deleted from the back-end",sep=""))
        }
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    delete_job = function(job) {
      if (!is.null(job) && "JobInfo" %in% class(job)) {
        job_id = job$id
      } else {
        job_id = job
      }
      
      tryCatch({
        tag = "jobs_delete"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(job_id)
        
        success = private$DELETE(endpoint = endpoint, authorized = TRUE)
        if(success) {
          message(paste("Job '",job_id,"' was successfully deleted from the back-end",sep=""))
        }
        return(success)
      },error=.capturedErrorToMessage)
      
    },
    
    delete_service = function(id) {
      tryCatch({
        tag = "services_delete"
        endpoint = self$getBackendEndpoint(tag) %>% replace_endpoint_parameter(id)
        
        msg = private$DELETE(endpoint = endpoint,
                             authorized = TRUE)
        message("Service '",id,"' successfully removed.")
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
    version = "0.4.1",
    general_auth_type = "bearer",
    disableAuth = TRUE,
    
    # functions ====
    loginOIDC = function() {
      if (!is.null(self$api.mapping)) {
        tag = "oidc_login"
        endpoint = self$getBackendEndpoint(tag)
      } 
      
      
      if (is.null(endpoint)) {
        stop("Cannot find endpoint for OIDC login")
      }
      suppressWarnings({
        tryCatch(
          {
            tryCatch({
              discovery_doc = private$GET(endpoint)
              private$oidc_client = OIDCAuth$new(host=discovery_doc$issuer,discovery_document = discovery_doc)
              
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
          endpoint = self$getBackendEndpoint(tag)
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
    backendVersions = function(url) {
      
      tryCatch({
        endpoint = "/.well-known/openeo"
        
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)
        
        return(info)
      },error=.capturedErrorToMessage)
      
    },

    GET = function(endpoint,authorized=FALSE,query = list(), ...) {
      url = paste(private$host,endpoint, sep ="/")

      if (authorized && !private$disableAuth) {
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
      if (authorized && !private$disableAuth) {
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
        if (authorized && !private$disableAuth) {
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
      if (authorized && !private$disableAuth) {
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
      if (authorized && !private$disableAuth) {
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

    # returns the header list and adds Authorization
    addAuthorization = function (header) {
      if (missing(header)) {
        header = list()
      }

      if (private$general_auth_type == "bearer") {
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
        header = append(header,authenticate(private$user,private$password,type = private$general_auth_type))
      }

      return(header)
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
