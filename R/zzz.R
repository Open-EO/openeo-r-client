pkgEnvironment = new.env()

.onLoad = function(libname, pkgname) {
    assign(x = "DEBUG_MODE", value = FALSE, envir = pkgEnvironment)
}
