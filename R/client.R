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
#'   \item{\code{$stopIfNotConnected()}}{throws an error if called and the client is not connected}
#'   \item{\code{$connect(url,version)}}{connects to a specific version of a backend}
#'   \item{\code{$client_version()}}{returns the client version}
#'   \item{\code{$register(user,password)}}{registers on the back-end via user and password (GEE only; legacy)}
#'   \item{\code{$login(login_type = NULL,user=NULL, password=NULL)}}{creates an \code{\link{IAuth}} object based on the login_type}
#'   \item{\code{$logout()}}{invalidates the access_token and terminates the current session}
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
      
      http_operation=toupper(self$api.mapping[self$api.mapping[,"tag"] == tag,"operation"][[1]])
      
      if (length(parameters) >0 && !is.na(parameters)) {
        endpoint= do.call(replace_endpoint_parameter, append(list(self$getBackendEndpoint(tag)),as.list(parameters)))
      } else  {
        endpoint= self$getBackendEndpoint(tag)
      }
      
      
      return(do.call(private[[http_operation]], append(list(endpoint=endpoint,authorized=authorized),list(...))))
    },
    isConnected = function() {
      return(!is.null(private$host))
    },
    getHost = function() {
      return(private$host)
    },
    stopIfNotConnected = function() {
      if (!self$isConnected()) {
        stop("Not connected to a back-end. Please connect to one before proceeding")
      }
    },

    connect = function(url,version) {
      
      tryCatch({
        
        if (missing(url)) {
          message("Note: Host-URL is missing")
          return(invisible(self))
        }
          
        if (endsWith(url,"/")) {
          url = substr(url,1,nchar(url)-1)
        }
        private$host = url
        
        if (!is.null(version)) {
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
    login=function(login_type = NULL,user=NULL, password=NULL) {
      self$stopIfNotConnected()
      if (is.null(login_type)) {
        return(invisible(self))
      }
      
      tryCatch({
        login_type = tolower(login_type)
        
        if (!is.null(login_type) && !login_type %in% c("basic","oidc") ) {
          stop("Cannot find the login mechanism type. Please use 'basic' or 'oidc'")
        }
        
        if (login_type == "oidc") {
          private$loginOIDC()
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
    }

  ),
  # private ----
  private = list(
    # attributes ====
    auth_client = NULL,
    user = NULL,
    password = NULL,
    host = NULL,
    version = "0.4.2",
    general_auth_type = "bearer",
    
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
              private$auth_client = OIDCAuth$new(host=discovery_doc$issuer,discovery_document = discovery_doc)
              
              if (is.null(private$auth_client)) {
                stop("OIDC client not initialized")
              }
              
              private$auth_client$login()
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
    backendVersions = function(url) {
      
      tryCatch({
        endpoint = "/.well-known/openeo"
        
        info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)
        
        return(info)
      },error=.capturedErrorToMessage)
      
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
    PUT = function(endpoint, authorized=FALSE, data, encodeType = NULL, ...) {
      url = paste(private$host,endpoint,sep="/")
      
      header = list()
      if (authorized && !is.null(private$auth_client)) {
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
            Authorization=paste("Bearer",private$auth_client$access_token, sep =" ")
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
#' The function returns the client version.
#' 
#' @return the client version
client_version = function() {
  return("0.4.1")
}
