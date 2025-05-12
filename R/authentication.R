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
#'   \item{`$login()`}{Initiates the authentication / login in order to obtain the access_token}
#'   \item{`$logout()`}{Terminates the access_token session and logs out the user on the openEO back-end}
#' }
#'
#' @seealso [BasicAuth()], [OIDCAuth()]
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

# [Basic Authentication] ----
#' Basic Authentication class
#'
#' This class handles the authentication to an openEO back-end that supports "basic" as login type. The class handles the retrieval
#' of an access token by sending the encoded token consisting of user name and the password via HTTP header 'Authorization'. 
#' The authentication will be done once via [login()] or multiple times when the lease time runs out. This class
#' is created and registered in the [OpenEOClient()]. After the login the user_id and the access_token are obtained and 
#' used as "bearer token" for the password restricted  web services.
#'
#' The class inherits all fields and function from [IAuth()]
#'
#' @name BasicAuth
#'
#' @section Methods:
#' \describe{
#'   \item{`$new(endpoint,user,password)`}{the constructor with the login endpoint and the credentials}
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{`endpoint`}{the basic authentication endpoint as absolute URL}
#'   \item{`user`}{the user name}
#'   \item{`password`}{the user password}
#' }
#'
#' @return an object of type [R6::R6Class()] representing basic authentication
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
      req = req_auth_basic(
        req=req_method(
          request(private$endpoint),
          method = "GET"),
        username = private$user,
        password = private$password)
      
      res = req_perform(req)

      if (is.debugging()) {
        print_response(res)
      }

      if (res$status_code == 200) {
        cont = resp_body_json(res)

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

#' OIDC Authentication
#' 
#' defines classes for different OpenID connect interaction mechanisms. The classes are modeled in generalized
#' fashion by inheriting functions from `IAuth` and `AbstractOIDCAuthentication`.
#' 
#' @field access_token The access_token to query password restricted  webservices of an openEO back-end
#' @field id_token The id_token retrieved when exchanging the access_token at the identity provider
#' 
#' @section Methods:
#' \describe{
#'   \item{`$new(provider, config=NULL, ...)`}{the constructor for the authentication}
#'   \item{`$login()`}{Initiates the authentication / login in order to obtain the access_token}
#'   \item{`$logout()`}{Terminates the access_token session and logs out the user on the openEO back-end}
#'   \item{`$getUserData()`}{queries the OIDC provider for the user data like the 'user_id'}
#'   \item{`$getAuth()`}{returns the internal authentication client as created from package 'httr2'}
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{`provider`}{the name of an OIDC provider registered on the back-end or a provider object as returned by `list_oidc_providers()`}
#'   \item{`config`}{either a JSON file containing information about 'client_id' and 
#'   'secret' or a named list. Experienced user and developer can also add 'scopes' to 
#'   overwrite the default settings of the OIDC provider}
#'   \item{`...`}{additional parameter might contain `force=TRUE` specifying to force the use 
#'   of a specific authentication flow}
#' }
#' 
#' @details 
#' The openEO conformant back-ends shall offer either a basic authentication and / or an OpenID 
#' Connect (OIDC) authentication. The first is covered at [BasicAuth]. And since OIDC is based
#' on the OAuth2.0 protocol there are several mechanisms defined to interact with an OIDC provider. The OIDC provider can be the 
#' back-end provider themselves, but they can also delegate the user management to other platforms like EGI, Github, Google, 
#' etc, by pointing to the respective endpoints during the service discovery of the back-end. Normally
#' users would not create those classes manually, but state the general login type (oidc or basic) and some 
#' additional information (see [login]).
#' 
#' This client supports the following interaction mechanisms (grant types):
#' \itemize{
#'   \item{authorization_code}
#'   \item{authorization_code+pkce}
#'   \item{client_credentials}
#'   \item{urn:ietf:params:oauth:grant-type:device_code}
#'   \item{urn:ietf:params:oauth:grant-type:device_code+pkce}
#' }
#' 
#' \subsection{authorization_code}{
#' During the login process an internet browser window will be opened and you will be asked to enter your credentials.
#' The website belongs to the OIDC provider of the chosen openEO back-end. Meanwhile, the client will start a server daemon in 
#' the background that listens to the callback from the OIDC provider. For this to work the user needs to get in contact with 
#' the openEO service provider and ask them for a configuration file that will contain information about the `client_id` and 
#' `secret`. The redirect URL requested from the provider is `http://localhost:1410/`
#' }
#' 
#' \subsection{authorization_code+pkce}{
#' This procedure also spawns a temporary web server to capture the redirect URL from the OIDC provider. The benefit of this 
#' mechanism is that it does not require a client secret issued from the OIDC provider anymore. However, it will still open 
#' the internet browser and asks the user for credentials and authorization.
#' }
#' 
#' \subsection{device_code+pkce}{
#' This mechanism does not need to spawn a web server anymore. It will poll the endpoint of the OIDC provider until the user
#' enters a specific device code that will be printed onto the R console. To enter the code either the URL is printed also to
#' the console or if R runs in the interactive mode the internet browser will be opened automatically.
#' }
#' 
#' \subsection{device_code}{
#' This mechanism uses a designated device code for human confirmation. It is closely related to the device_code+pkce code flow, 
#' but without the additional PKCE negotiation.
#' }
#' 
#' @seealso 
#' \describe{
#' \item{openEO definition on Open ID connect}{<https://openeo.org/documentation/1.0/authentication.html#openid-connect>}
#' \item{Open ID Connect (OIDC)}{<https://openid.net/connect/>}
#' \item{OAuth 2.0 Device Authorization Grant}{<https://datatracker.ietf.org/doc/html/rfc8628>}
#' \item{Proof Key for Code Exchange by OAuth Public Clients}{<https://datatracker.ietf.org/doc/html/rfc7636>}
#' }
#' 
#' @name OIDCAuth
#' @import httr2
#' @importFrom R6 R6Class
#' @importFrom base64enc base64decode
#' @importFrom jsonlite fromJSON
#' @importFrom rlang is_interactive
NULL

# [AbstractOIDCAuthentication] ----
AbstractOIDCAuthentication <- R6Class(
  "AbstractOIDCAuthentication",
  inherit = IAuth,
  # public ====
  public = list(
    # attributes ####
    
    # functions ####
    initialize = function(provider, config = list(),...) {
      args = list(...)
      if ("force" %in% names(args)) private$force_use = args[["force"]]
      # comfort function select provider by name if one is provided
      if (!inherits(provider, "Provider")) {
        provider = .get_oidc_provider(provider)
      }
      
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
      if (length(config$scopes) > 0) {
        if (is.character(config$scopes)) {
          private$scopes = list(config$scopes)
        } else {
          private$scopes = config$scopes
        }
      } else if (length(provider$scopes) == 0) {
        private$scopes = list("openid")
      } else {
        private$scopes = provider$scopes

        #TODO remove later, this is used for automatic reconnect
        if (private$grant_type != "client_credentials" && !"offline_access" %in% private$scopes) {
          private$scopes = c(private$scopes, "offline_access")
        }
      }
      
      private$getEndpoints()
      
      if (!(is.list(config) && all(c("client_id") %in% names(config)))) {
        stop("'client_id' is not present in the configuration.")
      }
      
      private$client_id = config$client_id
      
      if (private$grant_type == "authorization_code" || private$grant_type == "client_credentials") {
        # in this case we need a client_id and secrect
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
          .req=request(url),
          id_token_hint = private$auth$id_token))
        
        if (response$status_code < 400) {
          message("Successfully logged out.")
          private$auth <- NULL
          invisible(TRUE)
        } else {
          return(resp_body_json(response))
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
    force_use=NULL,
    
    auth = NULL, # httr2 oauth2.0 token object
    
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
    
    isInteractive = function() {
      return(if (is_jupyter()) FALSE else rlang::is_interactive())
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

# [OIDCDeviceCodeFlow] ----
OIDCDeviceCodeFlow <- R6Class(
  "OIDCDeviceCodeFlow",
  inherit = AbstractOIDCAuthentication,
  # public ====
  public = list(
    # functions ####
    login = function() {
      private$auth = rlang::with_interactive(
                      oauth_flow_device(
                        client = private$oauth_client,
                        auth_url = private$endpoints$device_authorization_endpoint,
                        scope = paste0(private$scopes, collapse = " ")
                      ),
                      value = private$isInteractive()
                    )

      invisible(self)
    }
  ),
  # private ====
  private = list(
    # attributes ####
    grant_type = "urn:ietf:params:oauth:grant-type:device_code", # not used internally by httr2, but maybe useful in openeo
    
    # functions ####
    isGrantTypeSupported = function(grant_types) {
      if (!"urn:ietf:params:oauth:grant-type:device_code" %in% grant_types) {
        stop("Device code flow is not supported by the authentication provider")
      }
      invisible(TRUE)
    }
  )
)

# [OIDCDeviceCodeFlowPkce] ----
OIDCDeviceCodeFlowPkce <- R6Class(
  "OIDCDeviceCodeFlowPkce",
  inherit = AbstractOIDCAuthentication,
  # public ====
  public = list(
    # functions ####
    login = function() {
      private$auth = rlang::with_interactive(
                      oauth_flow_device(
                        client = private$oauth_client,
                        auth_url = private$endpoints$device_authorization_endpoint,
                        scope = paste0(private$scopes, collapse = " "),
                        pkce = TRUE
                      ),
                      value = private$isInteractive()
                    )

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
      if (!"urn:ietf:params:oauth:grant-type:device_code+pkce" %in% grant_types) {
        stop("Device code flow with pkce is not supported by the authentication provider")
      }
      invisible(TRUE)
    }
  )
)

# [OIDCAuthCodeFlowPKCE] ----
OIDCAuthCodeFlowPKCE <- R6Class(
  "OIDCAuthCodeFlowPKCE",
  inherit = AbstractOIDCAuthentication,
  # public ====
  public = list(
    # attributes ####
    # functions ####
    login = function() {
      private$auth = rlang::with_interactive(
                      oauth_flow_auth_code(
                        client = private$oauth_client,
                        auth_url = private$endpoints$authorization_endpoint,
                        scope = paste0(private$scopes, collapse = " "),
                        pkce = TRUE,
                        port = 1410
                      ),
                      value = private$isInteractive()
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
      private$auth = rlang::with_interactive(
                      oauth_flow_auth_code(
                        client = private$oauth_client,
                        auth_url = private$endpoints$authorization_endpoint,
                        scope = paste0(private$scopes, collapse = " "),
                        pkce = FALSE,
                        port = 1410
                      ),
                      value = private$isInteractive()
                    )
    }
  ),
  # private ====
  private = list(
    grant_type = "authorization_code",

    
    # functions ####
    isGrantTypeSupported = function(grant_types) {
      # to be implemented in inheriting class
      if (isTRUE(private$force_use)) return(invisible(TRUE))
      
      if (!any(c("authorization_code") %in% grant_types)) {
        stop("Authorization code flow is not supported by the authentication provider")
      }
      invisible(TRUE)
    }
  )
)

# [OIDCClientCredentialsFlow] ----
OIDCClientCredentialsFlow <- R6Class(
  "OIDCClientCredentialsFlow",
  inherit = AbstractOIDCAuthentication,
  # public ====
  public = list(
    # functions ####
    login = function() {
      private$auth = oauth_flow_client_credentials(
                        client = private$oauth_client,
                        scope = paste0(private$scopes, collapse = " ")
                      )

      invisible(self)
    }
  ),
  # private ====
  private = list(
    # attributes ####
    grant_type = "client_credentials", # not used internally by httr2, but maybe useful in openeo
    
    # functions ####
    isGrantTypeSupported = function(grant_types) {
      if (!"client_credentials" %in% grant_types) {
        stop("Client Credentials flow is not supported by the authentication provider")
      }
      invisible(TRUE)
    }
  )
)

# utility functions ----
.get_oidc_provider = function(provider, oidc_providers=NULL) {
  if (is.debugging()) {
    if (inherits(provider,"Provider")) return(provider)
  }
  
  if (is.null(oidc_providers)) {
    oidc_providers = list_oidc_providers()
  }
  
  if (length(oidc_providers) == 0) {
    stop("OIDC provider list from 'list_oidc_providers()' is empty. Either OIDC authentication is not supported by the back-end or the meta data is incomplete. Please contact the back-end provider.")
  }
  
  if (!is.null(provider) && !any(c("character","Provider") %in% class(provider))){
    stop("Unsupported type for 'provider'.")
  }
  
  # no provider is stated then the first provider in the list is taken
  if (is.null(provider)) {
    
    # take the first one with default_clients
    default_clients_candidates = which(sapply(oidc_providers,function(provider) {
      "default_clients" %in% names(provider)
    }))
    
    if (length(default_clients_candidates) == 0) {
      stop("All OIDC provider require additional configuration. Please contact the openEO back-end provider for login configuration.")
    }
    
    default_provider = min(default_clients_candidates)
    
    provider = oidc_providers[[default_provider]]
  } else {
    if (length(provider) > 0 && is.character(provider)) {
      if (provider %in% names(oidc_providers)) {
        return(oidc_providers[[provider]])
      } else {
        stop(paste0("The selected provider '",provider,"' is not supported. Check with list_oidc_providers() the available providers."))
      }
    } else {
      # this can only be the Provider object. Test if exists
      if (! provider$id %in% names(oidc_providers)) stop("Provider '",provider$id,"' is not supported by the back-end")
    }
  }
  
  return(provider)
}

.get_client = function(clients, grant, config) {
  if (length(clients) == 0) return(NULL)
  
  supported = which(sapply(clients, function(p) grant %in% p$grant_types))
  
  if (length(supported) == 0) return(NULL)
  
  if (length(supported) > 1) {
    # screen the ids for containment of something with openeo before selecting the first
    client_ids = sapply(clients[supported], function(c)c$id)
    openeo_named = which(grepl(x = tolower(client_ids),pattern = "openeo"))
    
    if (length(openeo_named) == 1) {
      supported = supported[openeo_named]
    }
  }
  
  if (length(supported) > 1) {
    message("Multiple default clients detected for this authentication provider. You might need to login with a custom configuration for `client_id`.")
  }
  
  config$client_id = clients[[supported[[1]]]]$id
  config$grant_type = grant
  return(config)
}

.get_default_client_ids = function(provider) {
  client_ids = sapply(provider$default_clients, function(x) x$id)
}
