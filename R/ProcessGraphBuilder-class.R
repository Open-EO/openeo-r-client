#' Process graph builder
#' 
#' This class shall support the proper definition of process graphs by registering all supported processes of the backend
#' to which you are connected on this class. On initialization the class will be extended with processes that are offered. As
#' basic function the function 'process' provided by this package will be wrapped and the ... parameter will be replaced by
#' NULL initialized parameter of the arguments in the backends detailed process description.
#' 
#' @examples
#' \dontrun{
#' con = connect(host = "http://localhost:8000/api/",user = "test",password = "test",rbackend=TRUE)
#' pg = ProcessGraphBuilder$new(con)
#' collection(collection_id = "Sentinel_2A",id_name = "product_id") %>% pg$filter_bands(bands = "B04")
#' }
#' @export
ProcessGraphBuilder = R6Class(
  "ProcessGraphBuilder",
  lock_objects = FALSE,
  public=list(
    initialize = function(con) {
      processes = con %>% listProcesses()
      ids = processes$name
      for (i in 1:length(processes)) {
        # self$processes = append(self$processes,(con %>% describeProcess(id)))
        process = processes[[i]]
        id = process$name
        arguments =  sapply(names(process$parameters), function(arg){return(arg = NULL)})
        
        f = openeo::process
        forms = formals(f)
        forms$... = NULL
        forms = append(forms,arguments)
        forms$process_id = id
        
        # the paramter that contains the openeo data set -> change to interprete format
        if ("imagery" %in% names(arguments)) {
          forms$prior.name = "imagery"
        } else if ("collection" %in% names(arguments)) {
          forms$prior.name = "collection"
        }
        formals(f) <- forms
        
        body = quote({
          res = list()
          arguments = list()
          if (!missing(process) && !is.null(process)) {
            if (is.list(process)) {
              if (attr(process, "type") %in% c("process", "udf", 
                                               "collection")) {
                arguments[[prior.name]] = process
              }
              else {
                stop("Chain corrupted. Prior element is neither a process or a collection")
              }
            }
          }
          call = as.list(match.call())
          
          call[[1]] <- NULL #delete the function name
          #also replace all those arguments we know are the static ones of "process"
          call[["process"]] <- NULL
          call[["process_id"]] <- NULL
          call[["prior.name"]] <- NULL
          
          if (length(call) > 0) {
            for (index in 1:length(call)) {
              call[[index]] = eval(call[[index]])
            }
          }
          
          additionalParameter = call
          res$process_id = process_id
          res = append(res, arguments)
          res = append(res, additionalParameter)
          attr(res, "type") <- "process"
          return(res)
        })
        body(f) <- body
        
        self[[id]] = f
      }
    }
  )
)