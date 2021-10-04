# Authentication Interface ----
#' IAuth
#'
#' An interface that states the intended behavior for the authentication.
#'
#' @field access_token The access_token to query password restricted web services of an openeo back-end
#' @field id_token The id_token retrieved when exchanging the access_token at the identity provider
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

IAuth <- R6Class(
  "IAuth",
  public = list(
    login = function() {

    },
    logout = function() {

    }
  ),
  active = list(
    access_token = function() {

    },
    id_token = function() {
      
    }
  )
)

# OIDC Authentication ----
#' OIDC Authentication
#'
#' A class that handles the authentication via Open ID Connect. The \code{httr} package is used to handle OIDCs underlying
#' OAuth2.0 mechanism. To align authentication between the two supported authentication methods this class inherits and implements
#' all fields and functions from \code{\link{IAuth}}. 
#' 
#' The OIDC login interacts with the OIDC provider via the \code{Authorization Code Grant}. During the login process an internet browser window 
#' will be opened and you will be asked to enter your credentials. The website belongs to the OIDC provider of the
#' chosen openeo back-end. Meanwhile the client will start a server demon in the background that listens for the callback from
#' the OIDC provider. For this to work the user needs to get in contact with the openEO service provider and ask them for a 
#' configuration file that will contain information about the 'client_id' and 'secret'. The redirect URL requested from the 
#' provider is 'http://localhost:1410/' (\code{\link[httr]{oauth_listener}}).
#' 
#' The \code{access_token} will be returned when queried. If the lease time has run out the client will refresh the access_token
#' automatically.
#' 
#' Since the OIDC workflow is mainly based on OAuth2.0 we use httr to deal with this authentication by creating an 
#' \code{\link[httr]{oauth_app}}
#'
#' @name OIDCAuth
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new(provider, config=NULL)}}{the constructor for the authentication}
#'   \item{\code{$getUserData()}}{queries the OIDC provider for the user data like the 'user_id'}
#'   \item{\code{$getAuth()}}{returns the internal authentication client as created from package 'httr'}
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{\code{provider}}{a provider object as returned by \code{list_oidc_providers()}}
#'   \item{\code{config}}{either a file to JSON containing information about 'client_id' and 'secret' or a named list}
#' }
#'
#' @importFrom R6 R6Class
#' @import httr
#' @importFrom base64enc base64decode
#' @importFrom jsonlite fromJSON
#' @import lubridate
NULL


OIDCAuth <- R6Class(
  "OIDCAuth",
  inherit = IAuth,
  # public ====
  public = list(
    # attributes ####

    # functions ####
    initialize = function(provider, config = NULL) {
      private$setIssuer(provider$issuer)
      
      private$id = provider$id
      private$title = provider$title
      private$description = provider$description
      
      if (length(provider$scopes) == 0) {
        private$scopes = list("openid")
      } else {
        private$scopes = provider$scopes
      }
      
      private$getEndpoints()
      
      # important for authorization_code flow
      if (is.null(config) || !is.list(config)) {
        stop("Please provide any configuration details about the client_id and the secret. Either as list object or specify a valid file path.")
      }
      
      if (is.character(config)) {
        if (file.exists(config)) {
          config = jsonlite::read_json(config)
        } else {
          stop("Please provide any configuration details about the client_id and the secret. If you chose to pass the credentials as file, then please provide valid file path.")
        }
      } 
      
      if (!(is.list(config) && all(c("client_id","secret") %in% names(config)))) {
        stop("'client_id' and 'secret' are not present in the configuration.")
      }
      
      private$client_id = config$client_id
      private$secret = config$secret
      
      if (!is.null(config$grant_type)) {
        private$grant_type = config$grant_type
      }
      
      return(self)
    },

    login = function() {
      openeo_endpoints <- structure(list(
        authorize = private$endpoints$authorization_endpoint,
        access = private$endpoints$token_endpoint
      ), class = "oauth_endpoint")

      app <- httr::oauth_app(
        appname = "openeo_login",
        key = private$client_id,
        secret = private$secret
      )
      
      user_params = list()
      if (is.null(private$grant_type)) user_params = append(list(grant_type=private$grant_type))

      if (length(user_params)) user_params = NULL
      
      suppressWarnings(
        private$auth <- httr::oauth2.0_token(
          endpoint = openeo_endpoints,
          app = app,
          cache = FALSE,
          scope = unlist(private$scopes),
          user_params = user_params
        )
      )
      private$token_expiry_time = Sys.time() + private$auth$credentials$expires_in
      
      invisible(self)
    },

    logout = function() {
      if (is.null(private$auth)) {
        message("Not logged in.")
        return(NULL)
      }
      
      if (length(private$endpoints$end_session_endpoint) == 1) {
        url <- parse_url(private$endpoints$end_session_endpoint)
        response <- GET(url, query = list(id_token_hint = private$auth$credentials$id_token))
        if (response$status < 400) {
          message("Successfully logged out.")
          private$auth <- NULL
          invisible(TRUE)
        } else {
          return(content(response))
        }
      } else {
        private$auth <- NULL
        invisible(TRUE)
      }
      
    },
    # fetches the oidc user data
    getUserData = function() {
      url <- parse_url(private$endpoints$userinfo_endpoint)
      response <- GET(url, add_headers(Authorization = paste("Bearer", private$auth$credentials$access_token)))

      if (response$status < 400) {
        return(content(response, as = "parsed", type = "application/json"))
      } else {
        return(response)
      }
    },

    getAuth = function() {
      return(private$auth)
    }
  ),
  # active ====
  active = list(
    access_token = function() {
      if (!is.null(private$auth)) {
        if (private$isExpired(private$auth$credentials$access_token)) {
          if (!is.null(private$auth$credentials$refresh_token)) {
            private$auth$refresh()
            private$token_expiry_time = Sys.time() + private$auth$credentials$expiry
          } else {
            stop("Cannot provide access_token. You have to login.")
            return(invisible(NULL))
          }
        }
        return(paste("oidc",private$id,private$auth$credentials$access_token,sep="/"))
      } else {
        stop("Please login first, in order to obtain an access token")
        return(invisible(NULL))
      }
    },
    id_token = function() {
      if (!is.null(private$auth)) {
        private$auth$credentials$id_token
      } else {
        stop("Please login first, before accessing the identity token")
        return(invisible(NULL))
      }
    }
  ),
  # private ====
  private = list(
    # attributes ####
    id = NA,
    title = NA,
    description = NA,
    scopes = list(),
    issuer = NA, # the url of the endpoint in (issuer)
    client_id = NULL,
    secret = NULL,
    endpoints = list(),
    grant_type = "authorization_code",
    token_expiry_time = NULL,
    
    auth = NULL, # httr oauth2.0 token object

    # functions ####
    isValid = function() {
      # use the endpoint
    },

    isExpired = function(token) {
      return(private$token_expiry_time <= Sys.time())
    },

    decodeToken = function(access_token, token_part) {
      tokens <- unlist(strsplit(access_token, "\\."))
      fromJSON(rawToChar(base64decode(tokens[token_part])))
    },

    getEndpoints = function() {
      endpoint <- ".well-known/openid-configuration"
      url <- paste(private$issuer, endpoint, sep = "")

      response <- GET(url)
      if (response$status < 400) {
        private$endpoints <- content(response, as = "parsed", type = "application/json")
      } else {
        message("Cannot access openid configuration endpoint.")
      }

      invisible(self)
    },

    setIssuer = function(issuer) {
      if (!endsWith(issuer, "/")) {
        issuer <- paste(issuer, "/", sep = "")
      }
      
      private$issuer = issuer
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


BasicAuth <- R6Class(
  "BasicAuth",
  inherit = IAuth,
  # public ====
  public = list(
    initialize = function(endpoint, user, password) {
      private$endpoint <- endpoint
      private$user <- user
      private$password <- password
    },
    login = function() {
      res <- GET(
        url = private$endpoint,
        config = authenticate(
          user = private$user,
          password = private$password,
          type = "basic"
        )
      )

      if (is.debugging()) {
        print(res)
      }

      if (res$status_code == 200) {
        cont <- content(res, type = "application/json")

        private$.access_token <- cont$access_token

        return(cont$user_id)
      } else {
        stop("Login failed.")
      }
    },
    logout = function() {
      private$.access_token <- NA
    }
  ),
  # active ====
  active = list(
    access_token = function() {
      if (length(private$.access_token) == 0 || is.na(private$.access_token)) {
        stop("No bearer token available. Please login first.")
      } else {
        return(paste("basic","",private$.access_token,sep="/"))
      }
    },
    id_token = function() {
      stop("Not provided in basic authentication")
    }
  ),
  # private ====
  private = list(
    endpoint = NA,
    user = NA,
    password = NA,
    .access_token = NA
  )
)
