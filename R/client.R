#' OpenEO client class
#'
#' A R6Class that interacts with an openEO-conformant backend.
#'
#' @include ClientListProduct-class.R
#' @importFrom R6 R6Class
#' @import httr
#' @import magrittr
#' @import jsonlite
#' @importFrom raster extent
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom lubridate as_datetime
#' @export
OpenEOClient <- R6Class(
  "OpenEOClient",
  # public ----
  public = list(
    # attributes ====
    disableAuth = FALSE,
    general_auth_type = "bearer",
    user_id = NULL,
    
    is_rserver = FALSE,
    api.version = "0.0.1",
    
    products = list(),
    processes = list(),

    # functions ====
    initialize = function() {

    },

    connect = function(url) {
      if (!missing(url)) {
        if (endsWith(url,"/")) {
          url = substr(url,1,nchar(url)-1)
        }
        private$host = url
        cat(paste("Registered '",url,"' as host","\n",sep=""))
        
        invisible(self)
      } else {
        stop("Host-URL is missing")
      }

    },
    capabilities = function() {
      endpoint = "capabilities"
      
      if (!private$isConnected()) {
        stop("No host selected")
      }
      
      capabilities = private$GET(endpoint = endpoint,authorized = FALSE)
      
      return(capabilities)
    },

    login = function(user, password, auth_type="basic") {
      endpoint = "auth/login"

      if (missing(user) || missing(password)) {
        stop("Username or password is missing.")
      }
      if (!private$isConnected()) {
        stop("No host selected")
      }
      private$user = user
      private$password = password

      url = paste(private$host, endpoint, sep="/")
      res = POST(url=url,
                 config = authenticate(user=user,
                                       password = password,
                                       type = auth_type)
      )

      if (res$status_code == 200) {
        cont = content(res,type="application/json")

        private$login_token = cont$token
        self$user_id = cont$user_id

        cat("Login successful." )
        invisible(self)
      } else {
        stop("Login failed.")
      }
    },
    listData = function() {
      
      
      if (self$is_rserver) {
        endpoint = "data/"
      } else {
        endpoint = "data"
      }
      
      listOfProducts = private$GET(endpoint=endpoint,type="application/json")
      return(listOfProducts)

      # lapply(listOfProducts, function(product) {
      #   product$print()
      #   # self$register(product)
      # })
      # invisible(self)

    },
    listProcesses = function() {
      
      if (self$is_rserver) {
        endpoint = "processes/"
      } else {
        endpoint = "processes"
      }
      
      listOfProcesses = private$GET(endpoint,type="application/json")
      return(listOfProcesses)
      # lapply(listOfProcesses, function(process) {
      #   process$print()
      #   # self$register(process)
      # })
      # invisible(self)
    },
    listJobs = function() {
      endpoint = paste("users",self$user_id,"jobs",sep="/")
      listOfJobs = private$GET(endpoint,authorized=TRUE,type="application/json")
      return(listOfJobs)
    },
    
    listUserFiles = function() {
      endpoint = paste("users",self$user_id,"files",sep="/")
      files = private$GET(endpoint,TRUE,type="application/json")
      return(files)
    },

    register = function(obj) {
      # TODO don't add already existing list stuff
      listName = NULL
      newObj = NULL

      if (isProcess(obj) || isClientListProcess(obj)) {
        if (is.null(self$processes)) {
          self$processes = list()
        }
        listName = "processes"

        newObj = list(obj)
        names(newObj) = obj$process_id

      } else if (isProduct(obj) || isClientListProduct(obj)) {
        if (is.null(self$products)) {
          self$products = list()
        }
        listName = "products"

        newObj = list(obj)
        names(newObj) = c(obj$product_id)

      } else {
        warning("Cannot register object. It is neither Process nor Product")
        return()
      }

      #add if not exists
      lapply(
        self[[listName]],
        function(obj) {
          if (listName == "products") {
            id = obj$process_id
          } else if (listName == "processes") {
            id = obj$product_id
          } else {
            stop("Trying to add something else than products or processes")
          }

          #TODO skip already existing objects
          # if it is incomplete (listElement) and obj is more complete
        }
      )
      self[[listName]] = append(self[[listName]],newObj)

    },
    describeProcess = function(pid) {
      endpoint = paste("processes",pid,sep="/")
      
      info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)

      return(info)
    },
    describeProduct = function(pid) {
      endpoint = paste("data",pid,sep="/")
      
      info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)

      return(private$modifyProductList(info))
    },
    describeJob = function(job_id) {
      endpoint = paste("jobs",job_id,sep="/")
      
      info = private$GET(endpoint = endpoint,authorized = FALSE, type="application/json",auto_unbox=TRUE)
      
      return(info)
    },

    uploadUserFile = function(file.path,target) {
      target = URLencode(target,reserved = TRUE)
      target = gsub("\\.","%2E",target)

      if (is.null(self$user_id)) {
        stop("User id is not set. Either login or set the id manually.")
      }

      endpoint = paste(private$host, "users",self$user_id,"files",target,sep="/")
      header = list()
      header = private$addAuthorization(header)

      post = PUT(url=endpoint, config = header, body=upload_file(file.path))

      return(post)
    },
    downloadUserFile = function(src, dst=NULL) {
      if (!is.character(src)) {
        stop("Cannot download file with a source statement that is no character")
      } else {
        src = .urlHardEncode(src)
      }
      
      if (is.null(dst)) {
        dst = tempfile()
      }
      
      endpoint = paste("users",self$user_id,"files",src,sep="/")
      file_connection = file(dst,open="wb")
      writeBin(object=private$GET(endpoint,authorized = TRUE,as = "raw"),con = file_connection)
      close(file_connection,type="wb")
      
      return(dst)
    },
    uploadJob = function(task,evaluate) {
      #store the job on the backend either as batch or lazy
      if (! evaluate %in% c("batch","lazy")) {
        stop("Cannot store job on the backend. Evaluate parameter is neither 'batch' nor 'lazy'")
      }
      
      if (self$is_rserver) {
        endpoint = paste(private$host,"jobs/",sep="/")
      } else {
        endpoint = paste(private$host,"jobs",sep="/")
      }
      
      if (is.list(task)) {
        
        header = list()
        header = private$addAuthorization(header)
        
        # create json and prepare to send graph as post body
        # jsonTask = taskToJSON(task)
        res=POST(
          url= endpoint,
          config = header,
          query = list(
            evaluate = evaluate # for API v0.0.2 to be removed
          ),
          body = task,
          encode = "json"
        )
        
        if (res$status_code == 200) {
          okMessage = content(res,"parsed","application/json")
          message("Task was sucessfully registered on the backend.")
          return(okMessage$job_id)
        } else {
          error = content(res,"text","application/json")
          stop(error)
        }
      } else {
        stop("Cannot interprete 'task' - task is no list")
      }
      
    },
    execute = function (task,format, output=NULL,evaluate="sync") {
      # endpoint = paste(private$host,"execute/",sep="/")
      if (self$is_rserver) {
        endpoint = paste(private$host,"jobs/",sep="/")
      } else {
        endpoint = paste(private$host,"jobs",sep="/")
      }
      
      header = list()
      header = private$addAuthorization(header)
      
      if (is.list(task)) {
        # create json and prepare to send graph as post body
        # jsonTask = taskToJSON(task)
        res=POST(
          url= endpoint,
          config = header,
          query = list(
            format = format,
            evaluate = evaluate # for API v0.0.2 to be removed
          ),
          body = task,
          encode = "json"
        )
      } else {
        # API v0.0.2
        # send task as id or url as query parameter
        # res = POST(
        #   url= endpoint,
        #   config = header,
        #   query = list(
        #     format = format,
        #     graph = task
        #   )
        # )
        stop("Not supported task object in client request creation")
      }
      
      if (res$status_code == 200) {
        if (!is.null(output)) {
          tryCatch(
            {
              message("Task result was sucessfully stored.")
              writeBin(content(res,"raw"),output)
              return(raster(output))
            },
            error = function(err) {
              stop(err)
            }
          )
          
        } else {
          return(content(res,"raw"))
        }
      } else {
        error = content(res,"text","application/json")
        stop(error)
      }
      
    },
    
    deleteUserFile = function (src) {
      
      if (is.character(src)) {
        src = .urlHardEncode(src)
      } else {
        stop("Cannot interprete parameter 'src' during delete request")
      }
      endpoint = paste("users",self$user_id,"files",src,sep="/")
      
      return(private$DELETE(endpoint = endpoint, authorized = TRUE))
    }, 
    deleteJob = function(job_id) {
      endpoint = paste("jobs",job_id,sep="/")
      
      return(private$DELETE(endpoint = endpoint, authorized = TRUE))
    }


  ),
  # private ----
  private = list(
    # attributes ====
    login_token = NULL,
    user = NULL,
    password = NULL,
    host = NULL,
    
    # functions ====
    isConnected = function() {
      return(!is.null(private$host))
    },

    isLoggedIn = function() {

      hasToken = !is.null(private$login_token)
      return(hasToken || self$disableAuth)
    },
    checkLogin = function() {
      if (!private$isLoggedIn()) {
        stop("You are not logged in.")
      }
    },
    GET = function(endpoint,authorized=FALSE, ...) {
      url = paste(private$host,endpoint, sep ="/")

      if (authorized) {
        response = GET(url=url, config=private$addAuthorization())
      } else {
        response = GET(url=url)
      }


      if (response$status_code %in% c(200)) {
        info = content(response, ...)
        return(info)

      } else {
        stop(paste("Cannot find or access endpoint ","'",endpoint,"'",sep=""))
      }
    },
    DELETE = function(endpoint,authorized=FALSE,...) {
      url = paste(private$host,endpoint,sep="/")
      
      header = list()
      if (authorized) {
        header = private$addAuthorization(header)
      }
      
      response = DELETE(url=url, config = header, ...)
      
      message = content(response)
      success = response$status_code %in% c(200,202,204)
      if (success) {
        tmp = lapply(message, function(elem) {
          message(elem)
        })
      } else {
        tmp = lapply(message, function(elem) {
          warning(elem)
        })
      }
      
      return(success)
    },
    modifyProductList = function(product) {
      if (is.list(product) && any(c("collection_id","product_id") %in% names(product))) {
        e = product$extent

        ext = extent(e$left,e$right,e$bottom,e$top)
        product$extent = ext
        product$crs = gdalsrsinfo(e$srs, as.CRS = TRUE)

        product$time$from = as_datetime(product$time$from)
        product$time$to = as_datetime(product$time$to)

        return(product)

      } else {
        stop("Object that is modified is not the list result of product.")
      }
    },
    # returns the header list and adds Authorization
    addAuthorization = function (header) {
      if (missing(header)) {
        header = list()
      }

      if (self$general_auth_type == "bearer") {
        header = append(header,add_headers(
          Authorization=paste("Bearer",private$login_token, sep =" ")
        ))
      } else {
        header = append(header,authenticate(private$user,private$password,type = self$general_auth_type))
      }

      return(header)
    }
    


  )

)

# statics -----
.urlHardEncode=function(text) {
  text = URLencode(text)
  text = gsub("\\/","%2F",text)
  text = gsub("\\.","%2E",text)
  return(text)
}

