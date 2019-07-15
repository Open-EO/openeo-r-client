#' @importFrom R6 R6Class
BasicAuth = R6Class(
  "BasicAuth",
  # public ----
  public=list(
    initialize = function(host) {
      private$host = host
      
    }
  ),
  # private ----
  private=list(
    host = NA
  )
)