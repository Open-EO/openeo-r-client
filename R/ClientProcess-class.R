#' @importFrom openEO.R.Backend Process
#' @importFrom jsonlite fromJSON
#' @export
ClientListProcess <- R6Class(
  "ClientListProcess",
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

    }
  )
)
