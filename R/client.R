#'@include utilities.R
NULL

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

# client functions ----

#' Returns the client version
#' 
#' The function returns the client version.
#' 
#' @param con an OpenEO client
#' 
#' @return the client version
client_version = function() {
  return("0.4.1")
}
