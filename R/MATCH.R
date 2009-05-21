MATCH <- function(x, table, nomatch = NA, ...)
  UseMethod("MATCH")
  
MATCH.default <- function(x, table, nomatch = NA, ...)
  match(x, table, nomatch = nomatch, ...)

MATCH.timeDate <- function(x, table, nomatch = NA, ...) {
  stopifnot(require("timeDate"))
  match(as.POSIXct(x), as.POSIXct(table), nomatch = nomatch, ...)
}
