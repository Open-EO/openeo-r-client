pkgEnvironment = new.env()

.onLoad = function(libname, pkgname) {
    assign(x = "DEBUG_MODE", value = FALSE, envir = pkgEnvironment)
    assign(x = "client_id", value = "2cb21b6b-6345-507c-9c21b-6bd3fae3b2bfa", envir = pkgEnvironment)
    assign(x = "active_connection", value = NULL, envir = pkgEnvironment)
}
