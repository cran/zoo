MATCH <- function(x, table, nomatch = NA, ...)
  UseMethod("MATCH")
  
MATCH.default <- function(x, table, nomatch = NA, ...)
  match(x, table, nomatch = nomatch, ...)

