#' @importFrom openEO.R.Backend Product
#' @importFrom jsonlite fromJSON
#' @export
ClientListProduct <- R6Class(
  "ClientListProduct",
  inherit = Product,
  public = list(
    initialize = function() {

    },
    print = function() {
        message = paste(
          paste("Product-ID:",self$product_id),"\n",
          paste("Description:", self$description),"\n",
          paste("Source:", self$source),"\n\n",
            sep="")
        cat(message)
    },
    fromJSON = function(json) {
      if (class(json) == "list") {
        #json already parsed into list
      } else {
        # json should be a string
        # parse with jsonlite
        json = fromJSON(jsond)
      }
      obj = ClientListProduct$new()
      obj$product_id = json$product_id
      obj$description = json$description
      obj$source = json$source

      return(obj)
    }
  )
)

#' @export
isClientListProduct = function(obj) {
  return("ClientListProduct" %in% class(obj))
}
