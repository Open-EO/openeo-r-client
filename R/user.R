# user endpoint ----

#' List workspace files
#' 
#' Lists all files in the workspaces of the authenticated user.
#' 
#' @param con authorized connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return a \code{data.frame} or \code{tibble} with file names and storage sizes
#' 
#' @export
list_files = function(con=NULL) {
    tryCatch({
        tag = "user_files"
        
        con = .assure_connection(con)
        
        files = con$request(tag = tag, authorized = TRUE, type = "application/json")
        files = files$files
        if (is.null(files) || length(files) == 0) {
            message("The user workspace at this host is empty.")
            return(invisible(files))
        }
        
        files = .listObjectsToDataFrame(files)
        
        if (isNamespaceLoaded("tibble")) {
            files = tibble::as_tibble(files)
        }
        
        
        return(files)
    }, error = .capturedErrorToMessage)
}


#' Upload data into the users workspace
#'
#' This function sends the file retrieved by the 'content' parameter to the specified target location (relative file path in the
#' user workspace) on the back-end.
#'
#' @param con authorized Connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param content the file path of the file to be uploaded
#' @param target the relative server path location for the file, e.g. where to find the file in the users workspace
#' @param encode the encoding type used to upload the data, e.g. 'multipart','form','json','raw' ('raw' by default)
#' @param mime mime type used in upload_file ('application/octet-stream' as a default)
#' 
#' @return the relative file path on the server
#' @importFrom utils URLencode
#' @export
upload_file = function(content, target, encode = "raw", mime = "application/octet-stream", con=NULL) {
    
    if (missing(content)) {
        stop("Content data is missing")
    }
    if (is.character(content)) {
        content = file.path(content)
    }
    if (!file.exists(content)) {
        stop(paste("Cannot find file at ", content))
    }
    
    tryCatch({
        con = .assure_connection(con)
        
        if (is.null(con$isLoggedIn())) {
            stop("User is not logged in.")
        }
        
        tag = "user_file_upload"
        
        # m = con$request(tag = tag, parameters = list(target), authorized = TRUE, data = httr::upload_file(content, type = mime), encodeType = encode)
        m = con$request(tag = tag, parameters = list(target), authorized = TRUE, data = content, raw=TRUE)
        message("Upload of user data was successful.")
        return(m)
    }, error = .capturedErrorToMessage)
    
    
}

#' Download a file from the user workspace
#' 
#' Sends a request to an openEO back-end to access the users files and downloads them to a given location.
#' 
#' @param con authorized connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param src the relative file path of the source file on the openEO back-end
#' @param dst the destination file path on the local file system
#' 
#' @return The file path of the stored file on your machine
#' @export
download_file = function(src, dst = NULL, con=NULL) {
    tryCatch({
        if (!is.character(src)) {
            stop("Cannot download file with a source statement that is no character")
        }
        
        if (is.null(dst)) {
            dst = tempfile()
        }
        
        tag = "user_file_download"
        
        con = .assure_connection(con)
        
        file_connection = file(dst, open = "wb")
        writeBin(object = resp_body_raw(con$request(tag = tag, parameters = list(src), authorized = TRUE, parsed=FALSE)), con = file_connection)
        
        message("Successfully downloaded the requested file.")
        
        return(dst)
    }, error = .capturedErrorToMessage, finally = {
        close(file_connection, type = "wb")
    })
}

#' Delete a file from the user workspace
#'
#' Sends a request to an openEO back-end in order to remove a specific file from the users workspaces.
#' 
#' @param src the relative file path of the source file on the openEO back-end that shall be deleted
#' @param con authorized connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return logical
#' @export
delete_file = function(src, con=NULL) {
    tryCatch({
        if (!is.character(src)) {
            stop("Cannot interprete parameter 'src' during delete request")
        }
        
        tag = "user_file_delete"
        
        con = .assure_connection(con)
        
        return(con$request(tag = tag, parameters = list(src), authorized = TRUE))
    }, error = .capturedErrorToMessage)
}


#' Get the current user account information
#' 
#' Calls endpoint \code{/me} to fetch the user account information of the user currently logged in.
#' 
#' @param con authenticated client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @return object of type user
#' @export
describe_account = function(con=NULL) {
    tryCatch({
        tag = "user_info"
        
        con = .assure_connection(con)
        
        user_info = con$request(tag = tag, authorized = TRUE, type = "application/json")
        
        class(user_info) = "User"
        return(user_info)
        
    }, error = .capturedErrorToMessage)
}

# authentication ----
#' Connect to a openEO service
#'
#' Connects to openEO service. If the back-end provides a well-known endpoint that allows redirecting to
#' specific versions you should provide the version parameter.
#' 
#' @details 
#' You can explore several already available openEO web services by using the openEO hub (\url{https://hub.openeo.org/}). There you 
#' have an overview about their status and connection details like the URL and supported features. You can explore the
#' service for free through the access to publicly available metadata of data collections as well as the offered
#' processing functions. For any computation and the creation of web services, you need to register the openEO partner of
#' your choice. There you will get further information on credentials and the log in procedure.
#' 
#' The \code{...} parameter allows you to pass on arguments directly for \code{\link{login}}. If they are omitted the 
#' client will only connect to the back-end, but does not do authentication. The user must do that manually afterwards. 
#' Based on the provided login parameters user / password or OIDC provider the appropriate login procedure for basic authentication
#' or OIDC authentication will be chosen.
#' 
#' The parameter \code{version} is not required. If the service offers a well-known document of the
#' service the client will choose an appropriate version (default the most recent production ready version).
#' 
#' When calling this function the \code{\link{OpenEOClient}} is also stored in a variable in the package
#' which marks the latest service that was connected to.
#' 
#' @param host URL pointing to the openEO server service host
#' @param version the openEO API version number as string (optional), see also \code{\link{api_versions}}
#' @param exchange_token 'access_token' or 'id_token' defines in the OIDC case the bearer token use
#' @param ... parameters that are passed on to \code{\link{login}}
#'
#' @examples 
#' \dontrun{
#' # The following examples show different configuration settings and point 
#' # to imaginary URLs. Please obtain a valid URL via the openEO hub and 
#' # register with one of the provider if required.
#' 
#' # connect to a host of the latest version and without authentication
#' con = connect(host='http://example.openeo.org')
#' 
#' # connect to a host by direct URL and basic log in
#' con = connect(host='http://example.openeo.org/v1.0',
#'               user='user',
#'              password='password')
#' 
#' # connect to a host with open id connect authentication
#' con = connect(host='http://example.openeo.org')
#'
#' # connect and login with a named and valid oidc provider
#' con = connect(host='http://example.openeo.org',
#'               provider='your_named_provider')
#' }
#'
#' @seealso \code{\link{active_connection}}
#' @export
connect = function(host, version = NULL, exchange_token="access_token", ...) {
    con = OpenEOClient$new()
    
    con = con$connect(url = host, version = version,exchange_token=exchange_token)
    
    if (length(con) == 0) {
        message("Invalid openEO host stated. Please use an URL pointing to a valid openEO web service implementation.")
        return(invisible(NULL))
    }
    
    args = list(...)
    if(length(args) > 0) {
      do.call(login,args)
    }
    
    return(invisible(con))
}

#' Log in on a specific back-end
#' 
#' Retrieves the bearer-token from the back-end by sending user name and password to the back-end. This step
#' is usually performed during the 'connect' step. If you are only connected to a back-end in order to 
#' explore the capabilities and want to compute something, then you need to log in afterwards.
#' 
#' @details 
#' Based on the general login type (\link{BasicAuth} or \link{OIDCAuth}) there need to be different configurations. The basic
#' authentication (if supported) is the simplest login mechanism for which user need to enter their credentials directly as
#' \code{user} and \code{password}.
#' 
#' For the Open ID connect authentication the user needs to select one of the accepted OIDC providers of 
#' \code{\link{list_oidc_providers}} as \code{provider}. Alternatively the name of the provider suffices.
#' For further configuration, you can pass a named list of values as \code{config} or
#' a file path to a JSON file.
#' 
#' There are many different authentication mechanisms for OIDC and OAuth2.0, which OIDC is based on. The 'openeo' package supports
#' currently the authorization_code, authorization_code+pkce and device_code+pkce (see \link{OIDCAuth}). For authorization_code
#' you need to state the \code{client_id} and \code{secret}. In general the most comfortable available login mechanism is chosen
#' automatically (1. device_code+pkce, 2. authorization_code+pkce, 3. authorization_code). For example, with the device_code 
#' flow you normally don't even need to specify any additional configuration. 
#' 
#' If you really want to choose the authorization flow mechanism manually, you can add \code{grant_type} in the configuration
#' list. You can then use the following values:
#' 
#' \itemize{
#'   \item authorization_code
#'   \item authorization_code+pkce
#'   \item urn:ietf:params:oauth:grant-type:device_code+pkce
#' }
#' 
#' @param con connected back-end connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param user the user name
#' @param password the password
#' @param provider provider object as obtained by 'list_oidc_providers()' or the name of the provider in the provider list. If NULL
#' and \code{provider_type="oidc"} then the first available provider is chosen from the list.
#' @param config named list containing 'client_id' and 'secret' or a path to the configuration file (type JSON). If NULL and 
#' \code{provider_type="oidc"} the configuration parameters are taken from the default authentication client of the OIDC provider.
#' @return a connected and authenticated back-end connection
#' 
#' @examples 
#' \dontrun{
#' # simple connection without login to maybe explore the capabilities of a back-end first
#' # the URL won't work and is just to demonstrate how to write the code
#' con = connect(host='http://example.openeo.org',version='1.0.0')
#' 
#' # some back-ends support logging in throug OIDC without any parameters
#' login()
#' 
#' # basic authentication, credentials are dummy values
#' login(user='user',password='password')
#' 
#' # or alternatively the OIDC login
#' login(provider=provider, config=config)
#' 
#' # with device_code+pkce enabled at the OIDC provider you can even use this
#' login(provider="your_named_provider")
#' 
#' }
#' @export
login = function(user = NULL, password = NULL, provider=NULL, config=NULL, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        return(con$login(user = user, password = password, provider = provider, config=config))
    }, error = .capturedErrorToMessage)
}

#' Log out
#' 
#' Logs out or closes the active connection to an openEO service.
#' 
#' @param con a connected openEO client object (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @export
logout = function(con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        con$logout()
        return(TRUE)
    }, error = .capturedErrorToMessage)
}

#' Active Connection
#' 
#' The function gets or sets the currently active connection to an openEO service. Usually, the
#' active connection is set when calling the \code{\link{connect}} function. Just the last 
#' connection is set as active.
#' An application for the active connection is the optional connection within all the functions
#' that interact with the openEO service and require a connection. If the connection is omitted 
#' in the function, this function is called in order to try to fetch a connection. If you 
#' want to operate on multiple services at once, you should use an explicit connection.
#' 
#' @param con optional \code{\link{OpenEOClient}} to set, if omitted or NULL the currently active 
#' connection is returned
#' @return \code{\link{OpenEOClient}}
#' 
#' @seealso \code{\link{connect}}
#' 
#' @examples \dontrun{
#' # Note: all URLs and credentials are arbitrary
#' con1 = connect("https://first.openeo-backend.com")
#' con2 = connect("https://second.openeo-backend.com")
#' 
#' active_connection() # this will be con2, the last connected backend
#' 
#' active_connection(con = con1) # sets the first connection as active, so it does not have to 
#' # be passed to all functions
#' 
#' active_connection() # this will now return the previous set connection con1
#' }
#' 
#' @export
active_connection = function(con=NULL) {
    if (is.null(con)) {
        return(get(x = "active_connection", envir = pkgEnvironment))
    } else if ("OpenEOClient" %in% class(con)) {
        assign(x = "active_connection", value = con, envir = pkgEnvironment)
        invisible(con)
    } else {
        stop(paste0("Cannot set active connection with object of class '",utils::head(class(con),1),"'"))
    }
}


#' Available OIDC provider
#' 
#' In case the openEO service provider supports OpenID connect authentication, this function will return a list
#' of supported provider that can be used by this specific service.
#' 
#' @param con active openEO service connection (\code{\link{OpenEOClient}})
#' 
#' @return a \code{ProviderList} object which is a named list of \code{Provider} objects.
#' 
#' @export
list_oidc_providers = function(con = NULL) {
    tryCatch({
        con = .assure_connection(con)
        tag = "oidc_login"
        providers = con$request(tag = tag, authorized = FALSE, type = "application/json")
        
        providers = providers$providers
        
        provider_ids = sapply(providers,function(p)p$id)
        
        providers = lapply(providers, function(p) {
            class(p) = "Provider"
            return(p)
        })
        
        names(providers) = provider_ids
        class(providers) = "ProviderList"
        
        return(providers)
    }, error = .capturedErrorToMessage)
}
