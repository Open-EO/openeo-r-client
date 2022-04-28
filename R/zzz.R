pkgEnvironment = new.env()

.onLoad = function(libname, pkgname) {
    assign(x = "DEBUG_MODE", value = FALSE, envir = pkgEnvironment)
    assign(x = "active_connection", value = NULL, envir = pkgEnvironment)
}

#' openeo-deprecated
#' 
#' Lists all currently deprecated functions that will be removed in the future.
#' 
#' @name openeo-deprecated
#' @rdname openeo-deprecated
NULL