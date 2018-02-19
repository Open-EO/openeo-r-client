#' @importFrom openEO.R.Backend Process
#' @importFrom jsonlite fromJSON
#' @export
ClientProcess <- R6Class(
  "ClientProcess",
  inherit = Process,
  public = list(
    fromJSON = function(json) {
      if (! is.list(json)) {
        json = fromJSON(json)
      }

      #json should be a list
      # lapply(json, function(attribute) {
      #   key = names(attribute)
      # })

    },
    print = function() {
      message = paste(
        paste("Product-ID:",self$process_id),"\n",
        paste("Description:", self$description),"\n",
        sep="")
      cat(message)
    }
  )
)
