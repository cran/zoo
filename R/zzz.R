.onLoad <- function(libname, pkgname) {
  assignInNamespace("as.Date.numeric", function (x, origin, ...) {
    if (missing(origin)) origin <- "1970-01-01"
    # MATLAB origin is day before 0000-01-01
    if (origin == "0000-00-00") as.Date("0000-01-01", ...) + x - 1 else as.Date(origin, ...) + x
  }, ns = "base")
}

.onUnload <- function(libpath) {
  assignInNamespace("as.Date.numeric", function (x, origin, ...) {
    if (missing(origin)) stop("'origin' must be supplied")
    as.Date(origin, ...) + x
  }, ns = "base")
}
