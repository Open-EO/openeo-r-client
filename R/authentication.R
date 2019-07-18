# Authentication Interface ----
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

#' @importFrom R6 R6Class
#' @import httr
#' @importFrom base64enc base64decode
#' @importFrom jsonlite fromJSON
#' @import lubridate
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
      
      app = oauth_app("openeo_login",private$client_id)
      
      suppressWarnings(
        private$auth <- oauth2.0_token(openeo_endpoints, app,cache=FALSE,scope="openid")
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
#' @importFrom R6 R6Class
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