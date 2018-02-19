#' @importFrom openEO.R.Backend Process
#' @importFrom jsonlite fromJSON
#' @export
ClientListProcess <- R6Class(
  "ClientListProcess",
  inherit = Process,
  public = list(
    initialize = function() {

    },
    print = function() {
      message = paste(
        paste("Process-ID:",self$process_id),"\n",
        paste("Description:", self$description),"\n\n",
        sep="")
      cat(message)
    },
    fromJSON = function(json) {
      if (class(json) == "list") {
        #json already parsed into list
      } else {
        # json should be a string
        # parse with jsonlite
        json = fromJSON(json)
      }
      obj = ClientListProcess$new()
      obj$process_id = json$process_id
      obj$description = json$description

      return(obj)
    }
  )
)

#' @export
isClientListProcess = function(obj) {
  return("ClientListProcess" %in% class(obj))
}
