MATCH <- function(x, table, nomatch = NA, ...)
  UseMethod("MATCH")
  
MATCH.default <- function(x, table, nomatch = NA, ...)
  match(x, table, nomatch = nomatch, ...)

MATCH.timeDate <- function(x, table, nomatch = NA, ...) {
  match(as.POSIXct(x), as.POSIXct(table), nomatch = nomatch, ...)
}

MATCH.times <- function(x, table, nomatch = NA, units = "sec", eps = 1e-10, ...) {
 match(trunc(x, units, eps), trunc(table, units, eps), nomatch = nomatch, ...)
}

MATCH.Date <- function(x, table, nomatch = NA, ...) {
  if(is.character(table)) table <- as.Date(table)
  match(unclass(x), unclass(table), nomatch = nomatch, ...)
}

MATCH.POSIXct <- function(x, table, nomatch = NA, ...) {
  if(is.character(table)) table <- as.POSIXct(table)
  match(unclass(x), unclass(table), nomatch = nomatch, ...)
}

MATCH.POSIXlt <- function(x, table, nomatch = NA, ...) {
  x <- as.POSIXct(x)
  if(inherits(table, "POSIXlt") || is.character(table)) table <- as.POSIXct(table)
  match(unclass(x), unclass(table), nomatch = nomatch, ...)
}
