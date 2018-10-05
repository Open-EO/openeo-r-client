#' @importFrom R6 R6Class
#' @import httr
#' @importFrom base64enc base64decode
#' @importFrom jsonlite fromJSON
#' @import lubridate
OIDCClient = R6Class(
  "OIDCClient",
  # public ----
  public=list(
    # attributes ====
    
    # functions ====
    initialize = function(host=NULL){
      if (!is.null(host)) {
        self$setHost(host)
        self$getEndpoints()
      }
    },
    
    setHost = function(host) {
      if (!endsWith(host,"/")) {
        private$host = paste(host,"/",sep="")
      }
      invisible(self)
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
    
    login = function() {
      openeo_endpoints = structure(list(
        authorize = private$endpoints$authorization_endpoint,
        access = private$endpoints$token_endpoint
      ),class="oauth_endpoint")
      app = oauth_app("openeo_login",private$client_id)
      
      suppressWarnings(
        mytoken <- oauth2.0_token(openeo_endpoints, app,cache=FALSE)
      )
      
      
      # browser()
      
      # url = parse_url(private$endpoints$authorization_endpoint)
      # url$query = self$authCodeQueryConfig
      
      # open a browser to insert credentials and authorize the client, then copy the redirect link and insert
      # it into the console
      # browseURL(build_url(url))
      # redirectionURL = readline("Please insert redirection URL:\n")
      # results = private$parseRedirectURI(redirectionURL)
      
      # private$authCode = results$code
      # private$session_state = results$session_state
      
      # self$authorize()
      private$setTokenInformation(mytoken$credentials)
      invisible(self)
    },
    getAccessToken = function() {
      if (private$isExpired(private$access_token)) {
        if (!private$isExpired(private$refresh_token)) {
          self$refreshAccessToken()
        } else {
          message("Cannot provide access_token. You have to login.")
          return(invisible(NULL))
        }
      }
      return(private$access_token)
    },
    
    authorize = function() {
      url = parse_url(private$endpoints$token_endpoint)
      
      response = POST(url, body = self$accessTokenBodyConfig, encode = "form")
      
      if (response$status < 400) {
        results = content(response,as="parsed",type="application/json")
        private$setTokenInformation(results)
        
      } else {
        message("ERROR: Cannot obtain access token. Not logged in or token expired.")
      }
      invisible(self)
    },
    refreshAccessToken = function() {
      url = parse_url(private$endpoints$token_endpoint)
      
      response = POST(url, body = self$refreshTokenBodyConfig, encode = "form")
      
      if (response$status < 400) {
        results = content(response,as="parsed",type="application/json")
        private$setTokenInformation(results)
      } else {
        message("ERROR: Cannot refresh access token. Not logged in or token expired.")
      }
      invisible(self)
    },
    logout = function() {
      url = parse_url(private$endpoints$end_session_endpoint)
      response = GET(url, query = list(id_token_hint = private$id_token))
      if (response$status < 400) {
        message("Successfully logged out.")
        invisible(TRUE)
      } else {
        return(response)
      }
    },
    getUserData = function() {
      url = parse_url(private$endpoints$userinfo_endpoint)
      response = GET(url, add_headers(Authorization = paste("Bearer",private$access_token)))
      
      if (response$status < 400) {
        return(content(response,as="parsed",type="application/json"))
      } else {
        return(response)
      }
    }
  ),
  # active ----
  active = list(
    authCodeQueryConfig = function() {
      return(
        list(
          client_id = private$client_id,
          redirect_uri = private$redirect_uri,
          response_type = "code",
          scope = "openid"
        )
      ) 
    },
    
    accessTokenBodyConfig = function() {
      return(list(
        grant_type="authorization_code",
        client_id = private$client_id,
        redirect_uri = private$redirect_uri,
        code = private$authCode
      ))
    },
    
    refreshTokenBodyConfig = function() {
      return(
        list(
          grant_type = "refresh_token",
          refresh_token = private$refresh_token,
          client_id = private$client_id,
          redirect_uri = private$redirect_uri
        )
      )
    }
  ),
  # private ----
  private=list(
    # attributes ====
    host = NA,
    # client_id = "openeo-r-client", # predefined in OpenID Provider configuration (should be cryptic :))
    # redirect_uri = "http://localhost:8764/auth/oidc/cb", # also registered
    client_id = "openeo-r-client_2",
    redirect_uri = "http://localhost:1410", # also registered
    endpoints = list(),
    authCode = NA,
    session_state = NA,
    access_token = NA,
    refresh_token = NA,
    id_token = NA,
    # functions ====
    
    setTokenInformation = function(token_response) {
      private$access_token = token_response$access_token
      private$refresh_token = token_response$refresh_token
      private$id_token = token_response$id_token
      private$session_state = token_response$session_state
      
    },
    
    isExpired = function(token) {
      expiry = as_datetime(private$decodeToken(token,2)$exp, tz=Sys.timezone())
      
      return(expiry <= Sys.time())
    },
    
    # parses the redirect URI in a list of KVP based on the query parameters
    parseRedirectURI = function(redirectURL) {
      params = unlist(strsplit(unlist(strsplit(redirectURL,"\\?"))[2],"&"))
      
      # split each kvp on = and created a named list
      names = sapply(params, function(kvp) {
        split = unlist(strsplit(kvp,"="))
        return(split[1])
      })
      values = lapply(params, function(kvp) {
        split = unlist(strsplit(kvp,"="))
        return(split[2])
      })
      
      names(values) = names
      
      return(values)
    },
    
    decodeToken = function(access_token, token_part) {
      tokens = unlist(strsplit(access_token,"\\."))
      fromJSON(rawToChar(base64enc::base64decode(tokens[token_part])))
    }
  )
)




