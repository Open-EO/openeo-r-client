pkgEnvironment = new.env()

.onLoad = function(libname, pkgname) {
    assign(x = "DEBUG_MODE", value = FALSE, envir = pkgEnvironment)
    assign(x = "active_connection", value = NULL, envir = pkgEnvironment)
}
