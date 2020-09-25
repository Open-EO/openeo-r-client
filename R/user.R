# user endpoint ----

#' Lists workspace files
#' 
#' Lists all files in the workspaces of the authenticated user.
#' 
#' @param con authorized connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' 
#' @return a tibble of for filenames and their sizes
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


#' Uploads data into the users workspace
#'
#' This function sends the file given by 'content' to the specified target location (relative file path in the
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
        m = con$request(tag = tag, parameters = list(target), authorized = TRUE, data = httr::upload_file(content, type = mime), encodeType = encode)
        message("Upload of user data was successful.")
        return(m)
    }, error = .capturedErrorToMessage)
    
    
}

#' Downloads a file from the users workspace
#' 
#' Sends a request to an openeo back-end to access the users files and downloads them to a given location
#' 
#' @param con authorized connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param src the relative filepath of the source file on the openeo back-end
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
        writeBin(object = con$request(tag = tag, parameters = list(src), authorized = TRUE, as = "raw"), con = file_connection)
        
        message("Successfully downloaded the requested file.")
        
        return(dst)
    }, error = .capturedErrorToMessage, finally = {
        close(file_connection, type = "wb")
    })
}

#' Deletes a file from the users workspace
#'
#' Sends a request to an openeo back-end in order to remove a specific file from the users workspaces
#' 
#' @param src the relative filepath of the source file on the openeo back-end that shall be deleted
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


#' Retrieves the current users account information
#' 
#' Calls endpoint \code{/me} to fetch the user account information of the user that is currently logged in to the back-end
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
#' Connect to a openeEO service
#'
#' Connects to openEO service. If the backend provides a well-known endpoint that allows for redirecting to
#' specific versions, then you should provide the versions parameter.
#' 
#' @details 
#' You can explore several already available openEO web services by using the openEO hub (\url{https://hub.openeo.org/}). There you 
#' have an overview about their status, connection details like the URL and supported features. You can always explore the
#' service for free, meaning you have access to publicly available metadata of data collections as well as the offered
#' processing functions. For any computations and the creation of webservices you need to register the openEO partner of
#' your choice. Then you will get further information on credentials and the login procedure.
#' 
#' Especially the \code{login_type} and the \code{authType} suggested by the client development guidelines are confusing. Here the login_type deals
#' just with considered login. Meaning 'basic' allows you to use username and password directly in the call, whereas 'oidc' will
#' open up a browser window, where you enter you credentials. The authentication against all protected endpoints will later
#' use the bearer token that the client has obtained after the login, unless the authentication was dropped with NULL anyways.
#' 
#' The parameter \code{version} is not required. If the service offers a well-known document of the
#' service the client will choose an appropriate version (default the most recent production ready version).
#' 
#' When calling this function the \code{\link{OpenEOClient}} is also stored in a variable in the package
#' which marks the latest service that was connected to.
#' 
#' @param host URL pointing to the openEO server service host
#' @param version the version number as string (optional)
#' @param user the user name (optional)
#' @param password the password (optional)
#' @param login_type either NULL, 'basic' or 'oidc'. This refers to the login mechanism that shall be used. NULL disables authentication.
#' @param exchange_token 'access_token' or 'id_token' defines in the OIDC case the bearer token use
#' @param provider provider object as obtained by 'list_oidc_providers()'
#' @param config named list containing 'client_id' and 'sercret' or a path to the configuration file (type JSON)
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
#' # connect to a host by direct url and basic login
#' con = connect(host='http://example.openeo.org/v1.0',
#'               user='user',
#'              password='password',
#'              login_type='basic')
#' 
#' # connect to a host with open id connect authentication
#' con = connect(host='http://example.openeo.org',
#'               version='1.0.0-rc.2',
#'               login_type='oidc')
#' }
#'
#' @seealso \code{\link{active_connection}}
#' @export
connect = function(host, version = NULL, user = NULL, password = NULL, login_type = NULL, exchange_token="access_token", provider=NULL, config = NULL) {
    con = OpenEOClient$new()
    
    if (!is.null(user) && !is.null(password) && is.null(login_type)) login_type = "basic"
    
    con = con$connect(url = host, version = version,exchange_token=exchange_token)
    
    if (length(con) == 0) {
        message("Invalid openEO host stated. Please use an URL pointing to a valid openEO webservice implementation.")
        return(invisible(NULL))
    }
    
    if (length(login_type) > 0) {
        if (login_type == "basic") {
            if (!is.null(user) && !is.null(password)) {
                con = con$login(user = user, password = password, login_type = login_type)
            }
        } else if (login_type == "oidc") {
            con = con$login(login_type = login_type, provider=provider, config = config)
        } else {
            message("Incomplete credentials. Either username or password is missing")
            return(invisible(NULL))
        }
    }
    
    
    return(invisible(con))
}

#' Function to login to a specific backend
#' 
#' Retrieves the bearer-token from the backend by sending user name and password to the backend. This step
#' is usually also performed in the 'connect' step. But if you only connected to a back-end in order to 
#' explore the functionalities and want to compute something, then you need to log in afterwards.
#' 
#' @param con connected back-end connection (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param user the user name
#' @param password the password
#' @param login_type either NULL, 'basic' or 'oidc'. This refers to the login mechanism that shall be used. NULL disables authentication.
#' @param provider provider object as obtained by 'list_oidc_providers()'
#' @param config named list containing 'client_id' and 'sercret' or a path to the configuration file (type JSON)
#' @return a connected and authenticated back-end connection
#' 
#' @examples 
#' \dontrun{
#' # simple connection without login to maybe explore the capabilities of a back-end first
#' con = connect(host='http://example.openeo.org',version='1.0.0-rc.2')
#' 
#' login(user='user',password='password',login_type='basic', con=con)
#' 
#' # or alternatively the oidc login
#' login(login_type='oidc', provider=provider, config=config)
#' }
#' @export
login = function(user = NULL, password = NULL, login_type = NULL, provider=NULL, config=NULL, con=NULL) {
    tryCatch({
        con = .assure_connection(con)
        
        return(con$login(user = user, password = password, login_type = login_type, provider = provider, config=config))
    }, error = .capturedErrorToMessage)
}

#' Logout
#' 
#' Logs out or closes the active connection to an openEO service.
#' 
#' @param con a connected openeo client object (optional) otherwise \code{\link{active_connection}}
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
#' The function gets or sets the currently active connection to an openEO service. Usually the
#' active connection is set when calling the \code{\link{connect}} function. Only the last 
#' connection is set to active.
#' An application for the active connection is the optional connection within all the functions
#' that interact with the openEO service and require a connection. If the connection is omitted 
#' in those function this function will be called in order to try to fetch a connection. If you 
#' want to operate on multiple services at once, you should use an explicit connection.
#' 
#' @param con optional \code{\link{OpenEOClient}} to set, if omitted or NULL the currently active 
#' connection is returned
#' @return \code{\link{OpenEOClient}}
#' 
#' @seealso \code{\link{connect}}
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
#' of supported provider that can be used on this specific service.
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