coredata <- function(x, ...)
  UseMethod("coredata")

coredata.zoo <- function(x, ...)
{
  attr(x, "class") <- attr(x, "oclass")
  attr(x, "index") <- attr(x, "oclass") <- NULL
  return(x)
}

coredata.ts <- function(x, ...)
{
  x <- unclass(x)
  attr(x, "tsp") <- NULL
  return(x)
}

coredata.irts <- function(x, ...)
{
  return(x$value)
}

coredata.its <- function(x, ...)
{
  return(x@.Data)
}


"coredata<-" <- function(x, value)
{
  UseMethod("coredata<-")
}

"coredata<-.zoo" <- function(x, value)
{
  stopifnot(length(x) == length(value))
  x[] <- value
  attr(x, "oclass") <- attr(value, "class")
  return(x)
}

"coredata<-.ts" <- function(x, value)
{
  stopifnot(length(x) == length(value))
  dim(value) <- dim(x)
  x[] <- value
  return(x)
}

"coredata<-.irts" <- function(x, value)
{
  stopifnot(length(x$value) == length(value))
  dim(value) <- dim(x$value)
  x$value[] <- value
  return(x)
}

"coredata<-.its" <- function(x, value)
{
  stopifnot(length(x@.Data) == length(value))
  dim(value) <- dim(x@.Data)
  x@.Data[] <- as.matrix(value)
  return(x)
}
