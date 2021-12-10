# Authentication Interface ----
#' IAuth
#'
#' An interface that states the intended behavior for the authentication.
#'
#' @field access_token The access_token to query password restricted  webservices of an openEO back-end
#' @field id_token The id_token retrieved when exchanging the access_token at the identity provider
#'
#' @name IAuth
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$login()}}{Initiates the authentication / login in order to obtain the access_token}
#'   \item{\code{$logout()}}{Terminates the access_token session and logs out the user on the openEO back-end}
#' }
#'
#' @seealso \code{\link{BasicAuth}} or \code{\link{OIDCAuthCodeFlow}}
NULL

# IAuth ----
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

# [OIDC Authentication] ----
#' OIDC Authentication
#'
#' A class that handles the authentication via \href{https://openid.net/connect/}{Open ID Connect (OIDC)}. The \code{httr} package is used to handle OIDCs underlying
#' OAuth2.0 mechanism. In order to align authentication between the two supported authentication methods this class inherits and implements
#' all fields and functions from \code{\link{IAuth}}. 
#' 
#' The OIDC login interacts with the OIDC provider via the \code{Authorization Code Flow}. The OIDC provider can be the 
#' back-end provider themselves, but they can also delegate the user management to other platforms like EGI, Github, Google, 
#' etc, by pointing to the respective endpoints during the service discovery of the back-end.
#' 
#' During the login process an internet browser window will be opened and you will be asked to enter your credentials.
#' The website belongs to the OIDC provider of the chosen openEO back-end. Meanwhile, the client will start a server daemon in 
#' the background that listens to the callback from the OIDC provider. For this to work the user needs to get in contact with 
#' the openEO service provider and ask them for a configuration file that will contain information about the \code{client_id} and 
#' \code{secret}. The redirect URL requested from the provider is \code{http://localhost:1410/} (\code{\link[httr]{oauth_listener}}).
#' 
#' The \code{access_token} will be returned when queried. As the \code{access_token} is only valid for a certain time period,
#' it needs to be refreshed once the lease time has run out. This refreshing will be hidden from the user and will be taken 
#' care of automatically.
#' 
#' Since the OIDC workflow is mainly based on \href{https://oauth.net/2/}{OAuth2.0} we use httr to deal with this authentication by creating an 
#' \code{\link[httr]{oauth_app}}.
#'
#' @seealso 
#' \describe{
#' \item{openEO definition on Open ID connect}{\url{https://openeo.org/documentation/1.0/authentication.html#openid-connect}}
#' \item{Open ID Connect (OIDC)}{\url{https://openid.net/connect/}}
#' }
#' 
#' @name OIDCAuthCodeFlow
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
#'   \item{\code{provider}}{the name of an OIDC provider registered on the back-end or a provider object as returned by \code{list_oidc_providers()}}
#'   \item{\code{config}}{either a JSON file containing information about 'client_id' and 
#'   'secret' or a named list. Experienced user and developer can also add 'scopes' to 
#'   overwrite the default settings of the OIDC provider}
#' }
#'
#' @importFrom R6 R6Class
#' @import httr
#' @importFrom base64enc base64decode
#' @importFrom jsonlite fromJSON
#' @import lubridate
NULL



# [Basic Authentication] ----
#' Basic Authentication class
#'
#' This class handles the authentication to an openEO back-end that supports "basic" as login type. The class handles the retrieval
#' of an access token by sending the encoded token consisting of user name and the password via HTTP header 'Authorization'. 
#' The authentication will be done once via \code{\link{login}} or multiple times when the lease time runs out. This class
#' is created and registered in the \code{\link{OpenEOClient}}. After the login the user_id and the access_token are obtained and 
#' used as "bearer token" for the password restricted  web services.
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

# [AbstractOIDCAuthentication] ----
#' @import httr2
AbstractOIDCAuthentication <- R6Class(
  "AbstractOIDCAuthentication",
  inherit = IAuth,
  # public ====
  public = list(
    # attributes ####
    
    # functions ####
    initialize = function(provider, config = list()) {
      # comfort function select provider by name if one is provided
      provider = .get_oidc_provider(provider)
      
      private$setIssuer(provider$issuer)
      
      private$id = provider$id
      private$title = provider$title
      private$description = provider$description
      
      if (is.character(config)) {
        if (file.exists(config)) {
          config = jsonlite::read_json(config)
        } else {
          stop("Please provide any configuration details about the client_id and the secret. If you add the credentials as file please provide a valid file path.")
        }
      } 
      
      if (length(config) > 0 && !is.list(config)) {
        stop("Please provide any configuration details about the client_id and the secret. Either as list object or specify a valid file path.")
      }
      
      # user knows best, allow custom scopes...
      if (length(config$scopes) > 0 && is.character(config$scopes)) {
        private$scopes = config$scopes
      } else if (length(provider$scopes) == 0) {
        private$scopes = list("openid")
      } else {
        private$scopes = provider$scopes
        
        #TODO remove later
        if (!"offline_access" %in% private$scopes) {
          private$scopes = c(private$scopes, "offline_access")
        }
      }
      
      private$getEndpoints()
      
      if ("default_client" %in% names(provider)) {
        default_client = provider[["default_client"]]
        # id, redirect_urls, grant_types
        config$client_id = default_client[["id"]]
        private$isGrantTypeSupported(default_client$grant_types)
      }
      
      
      
      if (!(is.list(config) && all(c("client_id") %in% names(config)))) {
        stop("'client_id' is not present in the configuration.")
      }
      
      private$client_id = config$client_id
      
      if (private$grant_type == "authorization_code") {
        # in this case we need a client_id and secrect, which is basically the old OIDC Auth Code implementation
        if (!all(c("client_id","secret") %in% names(config))) {
          stop("'client_id' and 'secret' are not present in the configuration.")
        }
        
        private$oauth_client = oauth_client(
          id = private$client_id,
          token_url = private$endpoints$token_endpoint,
          name = "openeo-r-oidc-auth",
          secret = config$secret
        )
      } else {
      
        private$oauth_client = oauth_client(
          id = private$client_id,
          token_url = private$endpoints$token_endpoint,
          name = "openeo-r-oidc-auth"
        )
      }
      
      return(self)
    },
    
    login = function() {
      # the initial login / getting access_token must be implemented by inheriting classes
    },
    
    logout = function() {
      if (is.null(private$auth)) {
        message("Not logged in.")
        return(NULL)
      }
      
      if (length(private$endpoints$end_session_endpoint) == 1) {
        url = url_parse(private$endpoints$end_session_endpoint)
        response = req_perform(req_url_query(
          req=request(url),
          id_token_hint = private$auth$id_token))
        
        if (response$status_code < 400) {
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
      url = url_parse(private$endpoints$userinfo_endpoint)
      response <- req_perform(
        req_auth_bearer_token(
          req=req_headers(
            request(url),
            Accept="application/json"),
          private$auth$access_token))
      
      
      if (response$status_code < 400) {
        return(resp_body_json(response))
      } else {
        message("User endpoint not supported at the authentication provider")
        invisible(NULL)
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
        if (private$isExpired(private$auth)) {
          if (!is.null(private$auth$refresh_token)) {
            private$auth = oauth_flow_refresh(client=private$oauth_client,refresh_token = private$auth$refresh_token)
          } else {
            stop("Cannot refresh access_token. Reason: no refresh token provided by the authentication service. You have to log in again.")
            return(invisible(NULL))
          }
        }
        return(paste("oidc",private$id,private$auth$access_token,sep="/"))
      } else {
        stop("Please login first, in order to obtain an access token")
        return(invisible(NULL))
      }
    },
    id_token = function() {
      if (!is.null(private$auth)) {
        private$auth$id_token
      } else {
        stop("Please login before accessing the identity token")
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
    grant_type = "", # not used internally by httr2, but maybe useful in openeo
    oauth_client = NULL,
    
    auth = NULL, # httr oauth2.0 token object
    
    isGrantTypeSupported = function(grant_types) {
      # to be implemented in inheriting class
      if (!any(c("authorization_code+pkce","authorization_code") %in% grant_types)) {
        stop("Authorization code flow with pkce is not supported by the authentication provider")
      }
      invisible(TRUE)
    },
    
    # functions ####
    isValid = function() {
      # use the endpoint
    },
    
    isExpired = function(token) {
      return(token$expires_at <= Sys.time())
    },
    
    decodeToken = function(access_token, token_part) {
      tokens <- unlist(strsplit(access_token, "\\."))
      fromJSON(rawToChar(base64decode(tokens[token_part])))
    },
    
    getEndpoints = function() {
      endpoint <- ".well-known/openid-configuration"
      url <- paste(private$issuer, endpoint, sep = "")
      
      response <- req_perform(request(url))
      if (response$status_code < 400) {
        private$endpoints <- resp_body_json(response)
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


# [OIDCDeviceCodeFlowPkce] ----
#' @export
OIDCDeviceCodeFlowPkce <- R6Class(
  "OIDCDeviceCodeFlowPkce",
  inherit = AbstractOIDCAuthentication,
  # public ====
  public = list(
    # functions ####
    login = function() {
      
      client <- oauth_client(
        id = private$client_id,
        token_url = private$endpoints$token_endpoint,
        name = "openeo-r-oidc-auth"
      )
      
      private$auth = oauth_flow_device(client = client,
                                       auth_url = private$endpoints$device_authorization_endpoint,
                                       scope=paste0(private$scopes,collapse=" "),pkce = TRUE)
      
      
      invisible(self)
    }
  ),
  # private ====
  private = list(
    # attributes ####
    grant_type = "urn:ietf:params:oauth:grant-type:device_code+pkce", # not used internally by httr2, but maybe useful in openeo
    
    # functions ####
    isGrantTypeSupported = function(grant_types) {
      # to be implemented in inheriting class
      if (!any(c("urn:ietf:params:oauth:grant-type:device_code+pkce","urn:ietf:params:oauth:grant-type:device_code") %in% grant_types)) {
        stop("Device code flow with pkce is not supported by the authentication provider")
      }
      invisible(TRUE)
    }
  )
)

# [OIDCAuthCodeFlowPKCE] ----
#' @export
OIDCAuthCodeFlowPKCE <- R6Class(
  "OIDCAuthCodeFlowPKCE",
  inherit = AbstractOIDCAuthentication,
  # public ====
  public = list(
    # attributes ####
    # functions ####
    login = function() {

      private$auth = oauth_flow_auth_code(client = client,
                                       auth_url = private$endpoints$authorization_endpoint,
                                       scope=paste0(private$scopes,collapse=" "),
                                       pkce = TRUE,
                                       port=1410
                                       )
      
      
      invisible(self)
    }
  ),
  # private ====
  private = list(
    # attributes ####
    grant_type = "authorization_code+pkce", # not used internally by httr2, but maybe useful in openeo

    isGrantTypeSupported = function(grant_types) {
      # to be implemented in inheriting class
      if (!any(c("authorization_code+pkce","authorization_code") %in% grant_types)) {
        stop("Authorization code flow with pkce is not supported by the authentication provider")
      }
      invisible(TRUE)
    }
  )
)


# [OIDCAuthCodeFlow] ----
OIDCAuthCodeFlow <- R6Class(
  "OIDCAuthCodeFlow",
  inherit = AbstractOIDCAuthentication,
  # public ====
  public = list(
    # attributes ####
    
    # functions ####
    login = function() {
      cat("fyi this is authentication code without PKCE.\n")
      private$auth = oauth_flow_auth_code(client = client,
                                          auth_url = private$endpoints$authorization_endpoint,
                                          scope=paste0(private$scopes,collapse=" "),
                                          pkce = FALSE,
                                          port=1410
      )
    }
  ),
  # private ====
  private = list(
    grant_type = "authorization_code",

    
    # functions ####
    isGrantTypeSupported = function(grant_types) {
      # to be implemented in inheriting class
      if (!any(c("authorization_code") %in% grant_types)) {
        stop("Authorization code flow with pkce is not supported by the authentication provider")
      }
      invisible(TRUE)
    }
  )
)

.get_oidc_provider = function(provider) {
  if (is.character(provider)) {
    oidc_providers = list_oidc_providers()
    if (provider %in% names(oidc_providers)) {
      return(oidc_providers[[provider]])
    } else {
      stop(paste0("The selected provider '",provider,"' is not supported. Check with list_oidc_providers() the available providers."))
    }
  }
  
  return(provider)
}