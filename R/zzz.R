.onLoad <- function(libname, pkgname) {
  assignInNamespace("as.Date.numeric", function (x, origin, ...) {
    if (missing(origin)) origin <- "1970-01-01"
    as.Date(origin, ...) + x
  }, ns = "base")
}

.onUnload <- function(libpath) {
  assignInNamespace("as.Date.numeric", function (x, origin, ...) {
    if (missing(origin)) stop("'origin' must be supplied")
    as.Date(origin, ...) + x
  }, ns = "base")
}
