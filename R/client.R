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
  public = list(
    disableAuth = FALSE,
    general_auth_type = "bearer",
    user_id = NULL,
    products = list(),
    processes = list(),

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
      endpoint = "data/"
      # private$checkLogin()
      listOfProducts = private$GET(endpoint=endpoint)
      return(listOfProducts)

      # lapply(listOfProducts, function(product) {
      #   product$print()
      #   # self$register(product)
      # })
      # invisible(self)

    },
    listProcesses = function() {
      endpoint = "processes/"
      listOfProcesses = private$GET(endpoint)
      return(listOfProcesses)
      # lapply(listOfProcesses, function(process) {
      #   process$print()
      #   # self$register(process)
      # })
      # invisible(self)
    },
    listJobs = function() {
      endpoint = paste("users",self$user_id,"jobs",sep="/")
      listOfJobs = private$GET(endpoint,authorized=TRUE)
      return(listOfJobs)
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
      endpoint = paste(private$host ,"processes",pid,sep="/")
      response = GET(url=endpoint)

      info = content(response,type="application/json", auto_unbox = TRUE)

      return(info)
    },
    describeProduct = function(pid) {
      endpoint = paste(private$host ,"data",pid,sep="/")
      response = GET(url=endpoint)

      info = content(response,type="application/json", auto_unbox = TRUE)

      return(private$modifyProductList(info))
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
    listUserFiles = function() {
      endpoint = paste("users",self$user_id,"files",sep="/")
      files = private$GET(endpoint,TRUE)
      return(files)
    },

    execute = function (task,format, output=NULL) {
      endpoint = paste(private$host,"execute/",sep="/")
      
      header = list()
      header = private$addAuthorization(header)
      
      if (is.list(task)) {
        # create json and prepare to send graph as post body
        # jsonTask = taskToJSON(task)
        res=POST(
          url= endpoint,
          config = header,
          query = list(
            format = format
          ),
          body = task,
          encode = "json"
        )
      } else {
        # send task as id or url as query parameter
        res = POST(
          url= endpoint,
          config = header,
          query = list(
            format = format,
            graph = task
          )
        )
      }
      
      if (res$status_code == 200) {
        if (!is.null(output)) {
          tryCatch(
            {
              writeBin(content(res,"raw"),output)
              message("Task result was sucessfully stored.")
              return(file(output))
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
      
    }


  ),
  private = list(
    login_token = NULL,
    user = NULL,
    password = NULL,
    host = NULL,

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
    GET = function(endpoint,authorized=FALSE) {
      url = paste(private$host,endpoint, sep ="/")

      if (authorized) {
        response = GET(url=url, config=private$addAuthorization())
      } else {
        response = GET(url=url)
      }


      if (response$status_code == 200) {
        info = content(response,type="application/json")
        return(info)

      } else {
        stop("Cannot access data endpoint")
      }
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


# openeo <- OpenEOClient$new()
