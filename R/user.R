# user endpoint ----

#' Lists workspace files
#' 
#' Lists all files in the workspaces of the authenticated user.
#' 
#' @param con authorized connection
#' 
#' @return a tibble of for filenames and their sizes
#' 
#' @export
list_files = function(con) {
    tryCatch({
        tag = "user_files"
        files = con$request(tag = tag, parameters = list(con$user_id), TRUE, type = "application/json")
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
#' @param con authorized Connection
#' @param content the file path of the file to be uploaded
#' @param target the relative server path location for the file
#' @param encode the encoding type used to upload the data, e.g. 'multipart','form','json','raw' ('raw' by default)
#' @param mime mime type used in upload_file ('application/octet-stream' as a default)
#' 
#' @return the relative file path on the server
#' @importFrom utils URLencode
#' @export
upload_file = function(con, content, target, encode = "raw", mime = "application/octet-stream") {
    
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
        target = URLencode(target, reserved = TRUE)
        target = gsub("\\.", "%2E", target)
        
        if (is.null(con$user_id)) {
            stop("User id is not set. Either login or set the id manually.")
        }
        
        tag = "user_file_upload"
        m = con$request(tag = tag, parameters = list(con$user_id, target), authorized = TRUE, data = httr::upload_file(content, type = mime), encodeType = encode)
        message("Upload of user data was successful.")
        return(m)
    }, error = .capturedErrorToMessage)
    
    
}

#' Downloads a file from the users workspace
#' 
#' Sends a request to an openeo back-end to access the users files and downloads them to a given location
#' 
#' @param con authorized connection
#' @param src the relative filepath of the source file on the openeo back-end
#' @param dst the destination file path on the local file system
#' 
#' @return The file path of the stored file
#' @export
download_file = function(con, src, dst = NULL) {
    tryCatch({
        if (!is.character(src)) {
            stop("Cannot download file with a source statement that is no character")
        } else {
            src = .urlHardEncode(src)
        }
        
        if (is.null(dst)) {
            dst = tempfile()
        }
        
        tag = "user_file_download"
        file_connection = file(dst, open = "wb")
        writeBin(object = con$request(tag = tag, parameters = list(con$user_id, src), authorized = TRUE, as = "raw"), con = file_connection)
        
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
#' @param con authorized connection
#' @param src the relative filepath of the source file on the openeo back-end that shall be deleted
#' 
#' @return logical
#' @export
delete_file = function(con, src) {
    tryCatch({
        if (is.character(src)) {
            src = .urlHardEncode(src)
        } else {
            stop("Cannot interprete parameter 'src' during delete request")
        }
        
        tag = "user_file_delete"
        return(con$request(tag = tag, parameters = list(con$user_id, src), authorized = TRUE))
    }, error = .capturedErrorToMessage)
}


#' Retrieves the current users account information
#' 
#' Calls endpoint /me to fetch the user account information of the user that is currently logged in to the back-end
#' 
#' @param con authenticated client object
#' @return object of type user
#' @export
describe_account = function(con) {
    tryCatch({
        tag = "user_info"
        user_info = con$request(tag = tag, authorized = TRUE, type = "application/json")
        
        class(user_info) = "User"
        return(user_info)
        
    }, error = .capturedErrorToMessage)
}

# authentication ----
#' Connect to a openeEO back-end
#'
#' Connects to openEO back-end. If the backend provides a well-known endpoint that allows for redirecting to
#' specific versions, then you should provide the versions parameter.
#' 
#' @details Especially the login_type and the authType suggested by the client development guidelines are confusing. Here the login_type deals
#' just with considered login. Meaning 'basic' allows you to use username and password directly in the call, whereas 'oidc' will
#' open up a browser window, where you enter you credentials. The authentication against all protected endpoints will later
#' use the bearer token that the client has obtained after the login, unless the authentication was dropped with NULL anyways.
#' 
#' @param host URL pointing to the openEO server back-end host
#' @param version the version number as string
#' @param user the user name (optional)
#' @param password the password (optional)
#' @param login_type either NULL, 'basic' or 'oidc'. This refers to the login mechanism that shall be used. NULL disables authentication.
#' @param exchange_token 'access_token' or 'id_token' defines in the OIDC case the bearer token use
#' @param external which external oidc provider shall be used (currently 'google' as allowed value)
#'
#' @examples 
#' \dontrun{
#' # connect to a host with specific version and without authentication
#' con = connect(host='http://example.openeo.org',version='0.4.2')
#' 
#' # connect to a host by direct url and basic login
#' con = connect(host='http://example.openeo.org/v/0.4.2',
#'               user='user',
#'              password='password',
#'              login_type='basic')
#' 
#' # connect to a host with open id connect authentication
#' con = connect(host='http://example.openeo.org',
#'               version='0.4.2',
#'               login_type='oidc')
#' }
#'
#' @export
connect = function(host, version = NULL, user = NULL, password = NULL, login_type = NULL, exchange_token="access_token", external=NULL) {
    con = OpenEOClient$new()
    
    if (is.null(user) && is.null(password) && is.null(login_type)) {
        con = con$connect(url = host, version = version,exchange_token=exchange_token)
    } else if (login_type == "basic") {
        if (!is.null(user) && !is.null(password)) {
            con = con$connect(url = host, version = version,exchange_token=exchange_token)$login(user = user, password = password, login_type = login_type)
        } else {
            con = con$connect(url = host, version = version,exchange_token=exchange_token)
        }
    } else if (login_type == "oidc") {
        con = con$connect(url = host, version = version,exchange_token=exchange_token)$login(login_type = login_type, external=external)
    } else {
        message("Incomplete credentials. Either username or password is missing")
        return()
    }
    
    return(con)
}

#' Function to login to a specific backend
#' 
#' Retrieves the bearer-token from the backend by sending user name and password to the backend. This step
#' is usually also performed in the 'connect' step. But if you only connected to a back-end in order to 
#' register, then you need to log in afterwards.
#' 
#' @param con connected back-end connection
#' @param user the user name
#' @param password the password
#' @param login_type either NULL, 'basic' or 'oidc'. This refers to the login mechanism that shall be used. NULL disables authentication.
#' @param external character - 'google' whether Google is used as a Identity Provider for OIDC
#' @return a connected and authenticated back-end connection
#' 
#' @examples 
#' \dontrun{
#' # simple connection without login to maybe explore the capabilities of a back-end first
#' con = connect(host='http://example.openeo.org',version='0.4.2')
#' 
#' login(con=con, user='user',password='password',login_type='basic')
#' 
#' # or alternatively the oidc login
#' login(con=con,login_type='oidc')
#' }
#' @export
login = function(con, user = NULL, password = NULL, login_type = NULL, external=NULL) {
    return(con$login(user = user, password = password, login_type = login_type, external = external))
}
