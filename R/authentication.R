# Authentication Interface ----
#' IAuth
#' 
#' An interface that states the intended behavior for the authentication.
#' 
#' @field access_token The access_token to query password restricted web services of an openeo back-end
#' 
#' @name IAuth
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$login()}}{Initiates the authentication / login in order to obtain the access_token}
#'   \item{\code{$logout()}}{Terminates the access_token session and logs out the user on the openeo back-end}
#' }
#' 
#' @seealso \code{\link{BasicAuth}} or \code{\link{OIDCAuth}}
NULL 

IAuth = R6Class(
  "IAuth",
  public = list(
    login = function() {
      
    },
    logout = function() {
      
    }
  ),
  active = list(
    access_token = function() {
      
    }
  )
)

# OIDC Authentication ----
#' OIDC Authentication
#' 
#' A class that authenticates via Open ID Connect. It inherits all fields and functions from \code{\link{IAuth}}. The OIDC login
#' will open a browser window where you will be asked to enter your credentials. The website belongs to the OIDC provider of the
#' chosen openeo back-end. Meanwhile the client will start a server demon in the background that listens for the callback from
#' the OIDC provider (the R client has provided its ID and the callback URI beforehand).
#' The \code{access_token} will be returned when queried. If the lease time has run out the client will refresh the access_token 
#' automatically.
#' 
#' @name OIDCAuth
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new(host,discovery_document = NULL)}}{the constructor for the authentication}
#'   \item{\code{$getUserData()}}{queries the OIDC provider for the user data like the 'user_id'}
#' }
#' 
#' @section Arguments:
#' \describe{
#'   \item{\code{host}}{the URL pointing to the OIDC discovery document}
#'   \item{\code{discover_document}}{optional the discovery document of the OIDC endpoint}
#' }
#' 
#' @importFrom R6 R6Class
#' @import httr
#' @importFrom base64enc base64decode
#' @importFrom jsonlite fromJSON
#' @import lubridate
NULL


OIDCAuth = R6Class(
  "OIDCAuth",
  inherit=IAuth,
  # public ====
  public=list(
    # attributes ####
    
    # functions ####
    initialize = function(host,discovery_document = NULL){
      private$setHost(host)
      
      if (is.null(discovery_document)) {
        private$getEndpoints()
      } else {
        private$endpoints = discovery_document
      }
      
    },
    
    login = function() {
      openeo_endpoints = structure(list(
        authorize = private$endpoints$authorization_endpoint,
        access = private$endpoints$token_endpoint
      ),class="oauth_endpoint")
      
      app = httr::oauth_app("openeo_login",private$client_id)
      
      suppressWarnings(
        private$auth <- httr::oauth2.0_token(openeo_endpoints, app,cache=FALSE,scope="openid")
      )
      invisible(self)
    },
    
    logout = function() {
      if (is.null(private$auth)) {
        message("Not logged in.")
        return(NULL)
      }
      url = parse_url(private$endpoints$end_session_endpoint)
      response = GET(url, query = list(id_token_hint = private$auth$credentials$id_token))
      if (response$status < 400) {
        message("Successfully logged out.")
        private$auth = NULL
        invisible(TRUE)
      } else {
        return(response)
      }
    },
    #fetches the oidc user data
    getUserData = function() {
      url = parse_url(private$endpoints$userinfo_endpoint)
      response = GET(url, add_headers(Authorization = paste("Bearer",private$auth$credentials$access_token)))
      
      if (response$status < 400) {
        return(content(response,as="parsed",type="application/json"))
      } else {
        return(response)
      }
    }
  ),
  # active ====
  active = list(
    access_token = function() {
      if (!is.null(private$auth)) {
        if (private$isExpired(private$auth$credentials$access_token)) {
          if (!private$isExpired(private$auth$credentials$refresh_token)) {
            private$auth$refresh()
          } else {
            stop("Cannot provide access_token. You have to login.")
            return(invisible(NULL))
          }
        }
        return(private$auth$credentials$access_token)
      } else {
        stop("Please login first, in order to obtain an access token")
        return(invisible(NULL))
      }
    }
  ),
  # private ====
  private=list(
    # attributes ####
    host = NA, # the url of the endpoint in 
    # client_id = "openeo-r-client", # predefined in OpenID Provider configuration (should be cryptic :))
    # redirect_uri = "http://localhost:8764/auth/oidc/cb", # also registered
    client_id = "openeo-r-client_2",
    endpoints = list(),
    auth = NULL, # httr oauth2.0 token object
    
    # functions ####
    
    isExpired = function(token) {
      expiry = as_datetime(private$decodeToken(token,2)$exp, tz=Sys.timezone())
      
      return(expiry <= Sys.time())
    },
    
    decodeToken = function(access_token, token_part) {
      tokens = unlist(strsplit(access_token,"\\."))
      fromJSON(rawToChar(base64decode(tokens[token_part])))
    },
    
    getEndpoints = function() {
      endpoint = ".well-known/openid-configuration"
      url = paste(private$host,endpoint,sep="")
      
      response = GET(url)
      if (response$status < 400) {
        private$endpoints = content(response,as="parsed",type="application/json")
      } else {
        message("Cannot access openid configuration endpoint.")
      }
      
      invisible(self)
    },
    
    setHost = function(host) {
      if (!endsWith(host,"/")) {
        private$host = paste(host,"/",sep="")
      }
      invisible(self)
    }
  )
)

# Basic Authentication ----
#' Basic Authentication class
#' 
#' This class handles the authentication to an openEO back-end that supports "basic" as login type. The class handles the retrieval
#' of an access token by sending the encoded token consisting of user name and the password via HTTP header 'Authorization'. In 
#' general the authentication will be done once via \code{\link{login}} or multiple times when the lease time runs out. This class
#' is created and registered in the \code{\link{OpenEOClient}}. After the login the user_id and the access_token is obtained which
#' will be used as Bearer token for the password restricted webservices.
#' 
#' The class inherits all fields and function from \code{\link{IAuth}}
#' 
#' @name BasicAuth
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new(endpoint,user,password)}}{the constructor with the login endpoint and the credentials}
#' }
#' 
#' @section Arguments:
#' \describe{
#'   \item{\code{endpoint}}{the basic authentication endpoint as absolute URL}
#'   \item{\code{user}}{the user name}
#'   \item{\code{password}}{the user password}
#' }
#' 
#' @return an object of type \code{\link{R6Class}} representing basic authentication
#' @importFrom R6 R6Class
NULL


BasicAuth = R6Class(
  "BasicAuth",
  inherit=IAuth,
  # public ====
  public=list(
    initialize = function(endpoint,user,password) {
      private$endpoint = endpoint
      private$user=user
      private$password = password
    },
    login = function() {
      res = GET(url=private$endpoint,
                config = authenticate(user=private$user,
                                      password = private$password,
                                      type = "basic")
      )
      
      if (is.debugging()) {
        print(res)
      }
      
      if (res$status_code == 200) {
        cont = content(res,type="application/json")
        
        private$.access_token = cont$access_token
        
        return(cont$user_id)
        
      } else {
        stop("Login failed.")
      }
    },
    logout = function() {
      private$.access_token = NA
    }
  ),
  # active ====
  active = list(
    access_token = function() {
      if(length(private$.access_token)==0 || is.na(private$.access_token)) stop("No bearer token available. Please login first.")
      else return(private$.access_token)
    }
  ),
  # private ====
  private=list(
    endpoint = NA,
    user = NA,
    password = NA,
    .access_token = NA
  )
)
