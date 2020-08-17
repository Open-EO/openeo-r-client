#'@include utilities.R
#'@include authentication.R
NULL

#' OpenEO client class
#'
#' A R6Class that interacts with an openEO-conformant back-end.
#' 
#' @name OpenEOClient
#' 
#' @field user_id The user_id obtained after authentication
#' @field api.mapping The mapping of the API endpoints and the back-end published ones
#' @field processes a list of \code{\link{Process}} objects offered by the back-end
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new(host=NULL)}}{the constructor with an optional host URL to connect to}
#'   \item{\code{$getBackendEndpoint(endpoint_name)}}{returns the URL for the requested endpoint tag}
#'   \item{\code{$request(tag,parameters=NULL, authorized=FALSE, ...)}}{performs the desired HTTP request by endpoint tag with 
#'   path parameters and whether or not authorization (access_token) has to be used.}
#'   \item{\code{$isConnected()}}{whether or not the client has a host set}
#'   \item{\code{$isLoggedIn()}}{returns a logical describing whether the user is logged in}
#'   \item{\code{$getHost()}}{returns the host URL}
#'   \item{\code{$stopIfNotConnected()}}{throws an error if called and the client is not connected}
#'   \item{\code{$connect(url,version)}}{connects to a specific version of a backend}
#'   \item{\code{$api_version()}}{returns the openEO API version which this client complies to}
#'   \item{\code{$login(login_type = NULL,user=NULL, password=NULL)}}{creates an \code{\link{IAuth}} object based on the login_type}
#'   \item{\code{$logout()}}{invalidates the access_token and terminates the current session}
#'   \item{\code{$getAuthClient()}}{returns the Authentication client}
#'   \item{\code{$setAuthClient(value)}}{sets the authentication client if it was configured and set externally}
#'   \item{\code{$getCapabilities()}}{}
#'   \item{\code{$getDataCollection()}}{returns the list of collections as obtainable at 'list_collections()'}
#'   \item{\code{$getProcessCollection()}}{returns the evaluated process list as obtainable at 'processes()'}
#'   \item{\code{$getId()}}{returns the ID of the Connection as stated in the getCapabilities document}
#'   \item{\code{$getTitle()}}{returns the title of the connection as stated in the getCapabilities document}
#' }
#' 
#' @section Arguments:
#' \describe{
#'   \item{\code{host}}{the openeo host URL}
#'   \item{\code{endpoint_name}}{the endpoint tag the client uses for the endpoints}
#'   \item{\code{tag}}{endpoint tag}
#'   \item{\code{parameters}}{named list of values to be replaced in the endpoint}
#'   \item{\code{authorized}}{whether or not the endpoint requires authentication via access_token}
#'   \item{\code{url}}{url of an openeo backend either directly versioned or with the separate version statement}
#'   \item{\code{version}}{the openeo API version to be used, or lists available API versions if set to NULL}
#'   \item{\code{user}}{the user name}
#'   \item{\code{password}}{the users password}
#'   \item{\code{login_type}}{'basic', 'oidc' or NULL to control the authentication}
#'   \item{\code{value}}{an authentication object}
#' }
NULL

#' @importFrom R6 R6Class
#' @import httr
#' @import jsonlite
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
    initialize = function(host=NULL) {
      if (!is.null(host)) {
        private$host = host
      }
      
      active_connection(con=self)
    },
    getBackendEndpoint = function(endpoint_name) {
      if (!is.null(self$api.mapping)) {

        endpoint = self$api.mapping[self$api.mapping$tag == endpoint_name & self$api.mapping$available, "endpoint"]
        
        if (isNamespaceLoaded("tibble")) endpoint = endpoint[[1]]
        
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
    
    request = function(tag,parameters=NULL, authorized=FALSE, ...) {
      http_operation=toupper(self$api.mapping[as.vector(self$api.mapping[,"tag"] == tag),"operation"][[1]])
      
      endpoint_requires_path_params = requires_endpoint_parameter(self$getBackendEndpoint(tag))
      
      if (endpoint_requires_path_params && !is.na(parameters) && length(parameters) == 0) {
        stop(paste0("Endpoint '",self$getBackendEndpoint(tag),"' requires parameters, which are not set."))
      }
      
      if (length(parameters) >0 && !is.na(parameters) && endpoint_requires_path_params) {
        endpoint= do.call(replace_endpoint_parameter, append(list(self$getBackendEndpoint(tag)),as.list(parameters)))
      } else  {
        endpoint= self$getBackendEndpoint(tag)
      }
      
      
      return(do.call(private[[http_operation]], append(list(endpoint=endpoint,authorized=authorized),list(...))))
    },
    isConnected = function() {
      return(!is.null(private$host))
    },
    isLoggedIn = function() {
      tryCatch({
        if (is.null(private$auth_client) || length(private$auth_client$access_token) == 0) {
          return(FALSE)
        } 
        
        return(TRUE)
      }, error = function(e) {
        return(FALSE)
      })
    },
    getHost = function() {
      return(private$host)
    },
    stopIfNotConnected = function() {
      if (!self$isConnected()) {
        stop("Not connected to a back-end. Please connect to one before proceeding")
      }
    },

    connect = function(url,version,exchange_token="access_token") {
      tryCatch({
        if (missing(url)) {
          message("Note: Host-URL is missing")
          return(invisible(self))
        }
          
        if (endsWith(url,"/")) {
          url = substr(url,1,nchar(url)-1)
        }
        
        
        response = NULL
        tryCatch({
          response = httr::GET(url = url)
        }, error = function(e) {
          
        })
        
        if (length(response) == 0) return(invisible(NULL))
        
        private$host = url
        private$exchange_token = exchange_token
        
        if (!missing(version) && !is.null(version)) {
          # url is not specific, then resolve /.well-known/openeo and check if the version is allowed
          hostInfo=private$backendVersions()$versions
          versionLabels = sapply(hostInfo,function(x)x$api_version)
          names(hostInfo) = versionLabels
          
          if (!version %in% versionLabels) {
            # print the available versions
            message(paste("Version",version,"is not provided by the back-end. Please choose one of the following",
                          paste(versionLabels,collapse=",")))
            return(invisible(self))
          } else {
            url = hostInfo[[version]]$url
            
            if (endsWith(url,"/")) {
              url = substr(url,1,nchar(url)-1)
            }
            private$host = url
          }
        } else {
          
          if ("versions" %in% names(private$backendVersions())) {
            hostInfo=private$backendVersions()$versions
            hostInfo = as.data.frame(do.call(rbind,hostInfo),stringsAsFactors=FALSE)
            
            for (i in 1:ncol(hostInfo)) {
              hostInfo[,i] = unlist(hostInfo[,i])
            }
            
            # select highest API version that is production ready. if none is production
            # ready, then select highest version
            hostInfo = .version_sort(hostInfo)
            private$host = hostInfo[1,"url"]
          }
        }
        
        # connections contract for RStudio
        observer = getOption("connectionObserver")
        
        if (!is.null(observer)) {
          observer$connectionOpened(type="OpenEO Service",
                                    displayName= self$getTitle(), 
                                    host=url, 
                                    listObjectTypes = function() {
                                      list(
                                        resource = list(
                                          contains = list(
                                            collection = list(
                                              contains="data"
                                            )
                                          )
                                        )
                                      )
                                      
                                    },
                                    connectCode = paste0("openeo::connect(host=\"",url,"\")"),
                                    disconnect = function() {
                                      logout()
                                    },
                                    listObjects = function() {
                                      
                                      
                                      if (!is.null(active_connection())) {
                                          cids = self$getCollectionNames()
                                          types = rep("collection",times=length(cids))
                                          
                                          df = data.frame(name=cids,type=types,stringsAsFactors = FALSE)
                                          return(df)
                                      } else {
                                        return(list())
                                      }
                                      
                                      
                                      
                                      
                                    },
                                    listColumns = function(collection=NULL) {
                                      if (!is.null(collection)) {
                                        coll = describe_collection(collection=collection)
                                        dim_names = names(dimensions(coll))
                                        
                                        types = sapply(dimensions(coll), function(dim) {
                                          type = dim$type
                                          
                                          if ("axis" %in% names(dim)) {
                                            type = paste0(type,", axis: ",dim$axis)
                                          }
                                          
                                          if ("extent" %in% names(dim)) {
                                            type = paste0(type,", extent: [",dim$extent[1],",",dim$extent[2],"]")
                                          }
                                          
                                          if ("values" %in% names(dim)) {
                                            type = paste0(type,", enum: [",paste(dim$values,collapse=","),"]")
                                          }
                                          
                                          return(type)
                                        })
                                        
                                        df = data.frame(name=dim_names, type=types, stringsAsFactors = FALSE)
                                        
                                        df = rbind(df,list(name="value",type="numeric"))
                                        return(df)
                                      }
                                    },
                                    previewObject = function(rowLimit=1,collection=NULL) {
                                      if (!is.null(collection)) {
                                        collection_viewer(x=collection)
                                      }
                                      return(data.frame())
                                    },
                                    connectionObject = self)
        }
        
        self$api.mapping = endpoint_mapping(self)
        cat("Connected to service: ",private$host,"\n")
        cat("Please check the terms of service (terms_of_service()) and the privacy policy (privacy_policy()). By further usage of this service, you acknowledge and agree to those terms and policies.\n")
        
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
        
      }, 
      error = .capturedErrorToMessage
      )

    },
    api_version = function () {
      return(private$version)
    },
    login=function(login_type = NULL,user=NULL, password=NULL,provider=NULL,config=NULL) {
      
      if (!is.null(user) && !is.null(password) && is.null(login_type)) {
        # then it should be "basic"
        login_type = "basic"
      }
      
      if (!is.null(provider) && !is.null(config) && is.null(login_type)) {
        login_type = "oidc"
      } 
      
      self$stopIfNotConnected()
      if (is.null(login_type)) {
        message("No login type specified. Use 'basic' or 'oidc'.")
        return(invisible(self))
      }
      
      tryCatch({
        login_type = tolower(login_type)
        
        if (!is.null(login_type) && !login_type %in% c("basic","oidc") ) {
          stop("Cannot find the login mechanism type. Please use 'basic' or 'oidc'")
        }
        
        if (login_type == "oidc") {
          private$loginOIDC(provider = provider, config = config)
        } else if (login_type == "basic") {
          private$loginBasic(user=user, password = password)
        } 
        
        invisible(self)
        
      },
      error=.capturedErrorToMessage)
    },
    logout = function() {
      if (!is.null(private$auth_client)){
        private$auth_client$logout()
      }
      
      assign(x = "active_connection", value = NULL, envir = pkgEnvironment)
      
      observer = getOption("connectionObserver")
      
      if (!is.null(observer)) observer$connectionClosed(type="OpenEO Service",host=private$host)
    },
    
    getAuthClient = function() {
      return(private$auth_client)
    },
    
    setAuthClient = function(value) {
      if (is.null(value)) {
        return(invisible(self))
      }
      
      if (!"IAuth" %in% class(value)) {
        stop("Value is no authentication class")
      }
      
      private$auth_client = value
      
      return(invisible(self))
    },
    
    getCapabilities = function() {
      if (is.null(private$capabilities)) {
        endpoint = "/"
        tryCatch({
          private$capabilities = private$GET(endpoint = endpoint)
            
          class(private$capabilities) = "OpenEOCapabilities"
        }, error = .capturedErrorToMessage)
      }
      
      return(private$capabilities)
    },
    getDataCollection=function() {
      if (is.null(private$data_collection)) {
        
        tryCatch({
          tag = "data_overview"
          
          collection_list = self$request(tag = tag, authorized = self$isLoggedIn(), type = "application/json")
          collection_list = collection_list$collections
          
          collection_list = lapply(collection_list, function(coll) {
            coll$extent$spatial = unlist(coll$extent$spatial$bbox)
            coll$extent$temporal = lapply(coll$extent$temporal$interval, function(t) {
              # t is list
              return(lapply(t,function(elem) {
                if (is.null(elem)) return(NA)
                else return(elem)
              }))
              
            })
              
            class(coll) = "Collection"
            return(coll)
          })
          
          class(collection_list) = "CollectionList"
          
          collection_names = sapply(collection_list, function(coll) {
            return(coll$id)
          })
          
          names(collection_list) = collection_names
          
          private$data_collection = collection_list
        }, error = .capturedErrorToMessage)
      }
      
      return(private$data_collection)
    },
    getCollectionNames = function() {
      return(names(self$getDataCollection()))
    },
    getProcessCollection=function() {
      if (is.null(private$process_collection)) {
        private$process_collection = ProcessCollection$new(con = self)
      }
      
      return(private$process_collection)
    },
    
    getId =function() {
      cap = self$getCapabilities()
      
      return(cap$id)
    },
    
    getTitle = function() {
      cap = self$getCapabilities()
      
      return(cap$title)
    }
    
    

  ),
  # private ----
  private = list(
    # attributes ====
    auth_client = NULL,
    user = NULL,
    password = NULL,
    host = NULL,
    version = "1.0.0-rc.2", # implemented api version
    general_auth_type = "bearer",
    exchange_token="access_token",
    capabilities=NULL,
    process_collection=NULL,
    data_collection=NULL,
    
    # functions ====
    loginOIDC = function(provider=NULL, config = NULL) {
      suppressWarnings({
        tryCatch({
            private$auth_client = OIDCAuth$new(provider=provider,
                                               config = config)
            
            private$auth_client$login()
            cat("Login successful.")
            
            return(invisible(self))
        }, error=function(e){
          private$auth_client = NULL
          stop("Login failed.")
        })
      })
    },
    
    loginBasic = function(user, password) {
      tryCatch({
        if (!is.null(self$api.mapping)) {
          tag = "login"
          endpoint = paste(self$getHost(),self$getBackendEndpoint(tag),sep="/")
        } else {
          endpoint = paste(self$getHost(),"credentials/basic",sep="/")
        }
        
        #endpoint,user,password
        private$auth_client = BasicAuth$new(endpoint,user,password)

        self$user_id = private$auth_client$login()
        cat("Login successful.")
        return(invisible(self))

      },
      error = .capturedErrorToMessage,
      finally = {
      })
    },
    backendVersions = function() {
      tryCatch({
        endpoint = ".well-known/openeo"
        
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)
        
        return(info)
      },error= function(e) {
        # .capturedErrorToMessage(e) 
        return(list())
      })
      
    },

    GET = function(endpoint,authorized=FALSE,query = list(), ...) {
      url = paste(private$host,endpoint, sep ="/")

      if (authorized && !is.null(private$auth_client)) {
        response = GET(url=url, config=private$addAuthorization(),query=query)
      } else {
        response = GET(url=url,query=query)
      }
  
      if (is.debugging()) {
        print(response)
      }

      if (response$status_code < 400) {
        if (response$status_code != 202) {
          info = content(response, ...)
          return(info)
        } else {
          return(NULL)
        }
        

      } else {
        private$errorHandling(response,url)
      }
    },
    DELETE = function(endpoint,authorized=FALSE,...) {
      url = paste(private$host,endpoint,sep="/")
      
      header = list()
      if (authorized && !is.null(private$auth_client)) {
        header = private$addAuthorization(header)
      }
      
      response = DELETE(url=url, config = header, ...)
      
      if (is.debugging()) {
        print(response)
      }
      
      message = content(response)
      
      if (response$status_code < 400) {
        if (response$status_code == 204) {
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
        if (authorized && !is.null(private$auth_client)) {
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
          if (response$status_code != 202) {
            if (raw) {
              return(response)
            } else {
              okMessage = content(response,"parsed","application/json")
              
              return(okMessage)
            }
          } else {
            return(NULL)
          }
          
        } else {
          private$errorHandling(response,url)
        }
      } else {
        stop("Cannot interprete data - data is no list that can be transformed into json")
      }
    },
    PUT = function(endpoint, authorized=FALSE, data=list(),encodeType = "json",query = list(), raw=FALSE,...) {
      url = paste(private$host,endpoint,sep="/")
      
      header = list()
      if (authorized && !is.null(private$auth_client)) {
        header = private$addAuthorization(header)
      }
      

      # create json and prepare to send graph as post body

      response=PUT(
        url= url,
        config = header,
        query = query,
        body = data,
        encode = encodeType
      )

      # params = list(url=url, 
      #               config = header, 
      #               body=data)
      # if (!is.null(encodeType)) {
      #   params = append(params, list(encode = encodeType))
      # }
      # response = do.call("PUT", args = params)
      
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
      if (authorized && !is.null(private$auth_client)) {
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
        if (!is.null(private$auth_client)) {
          header = append(header,add_headers(
            Authorization=paste("Bearer",private$auth_client[[private$exchange_token]], sep =" ")
          ))
        } else {
          stop("Cannot add HTTP Authorization, because you are not logged in.")
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

# client functions ----

#' Returns the client version
#' 
#' The function returns the client version. Wraps the call 'packageVersion("openeo")', which will return this packages version.
#' 
#' @return the client version
#' 
#' @importFrom utils packageVersion
#' @export
client_version = function() {
  return(packageVersion("openeo"))
}
