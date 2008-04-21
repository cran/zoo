as.zoo <- function(x, ...)
{
  UseMethod("as.zoo")
}

as.zoo.default <- function(x, ...)
{
  zoo(structure(x, dim = dim(x)), index(x), ...)
}

as.zoo.factor <- function(x, ...) 
{
  zoo(x, ...)
}

as.zoo.irts <- function(x, ...)
{
  zoo(x$value, x$time, ...)
}

as.zoo.its <- function(x, ...) 
{
	index <- attr(x, "dates")
	class(x) <- attr(x, "dates") <- NULL
	zoo(x, index, ...)
}

as.zoo.timeSeries <- function(x, ...) {
  stopifnot(require("fSeries"))
  zoo(as.matrix(x), time(x), ...)  
}

as.zoo.zoo <- function(x, ...) x

## This should be in its now.
## as.its.zoo <- function(x) {
## 	stopifnot(require(its))
## 	index <- index(x)
## 	stopifnot(inherits(index, "POSIXct"))
## 	its(coredata(x), index)
## }

as.vector.zoo <- function(x, mode = "any")
	as.vector(as.matrix(x), mode = mode)

as.matrix.zoo <- function(x, ...) 
{
    y <- as.matrix(coredata(x), ...)
    if (length(y) > 0) 
	    colnames(y) <- if (length(colnames(x)) > 0) 
		colnames(x)
	    else {
		lab <- deparse(substitute(x))
		if (NCOL(x) == 1) 
		    lab
		else paste(lab, 1:NCOL(x), sep = ".")
	    }
    if(is.null(row.names(y))) row.names(y) <- index2char(index(x), frequency = attr(x, "frequency"))
    return(y)
}

as.data.frame.zoo <- function(x, row.names = NULL, optional = FALSE, ...)
{
	y <- as.data.frame(coredata(x), optional = optional)
        if(NCOL(x) > 0 && !optional) {
		colnames(y) <- if (length(colnames(x)) > 0) 
			colnames(x)
		else {
			lab <- deparse(substitute(x))
			if (NCOL(x) == 1) lab
	                  else paste(lab, 1:NCOL(x), sep = ".")
		}
	}
	if (!is.null(row.names)) row.names(y) <- row.names 
	  else {
	    tmp <- index2char(index(x), frequency = attr(x, "frequency"))
	    if (!any(duplicated(tmp))) row.names(y) <- tmp
        }
	return(y)
}

as.list.zoo <- function(x, ...) {
	if (length(dim(x)) == 0) list(x)
  		else lapply(as.data.frame(x), zoo, index(x),  attr(x, "frequency"))
}

as.list.ts <- function(x, ...) {
	if (is.matrix(x))
		lapply(as.data.frame(x), ts, 
			start = start(x), end = end(x), freq = frequency(x))
	else
		list(x)
}


## regular series coercions

as.zooreg <- function(x, ...)
{
  UseMethod("as.zooreg")
}

as.zooreg.default <- function(x, ...)
{
  as.zooreg(as.zoo(x, ...))
}

as.zooreg.ts <- as.zoo.ts <- function(x, ...)
{
  xtsp <- tsp(x)
  zooreg(coredata(x), start = xtsp[1], end = xtsp[2], frequency = xtsp[3])
} 

as.ts.zooreg <- function(x, ...)
{
  freq <- frequency(x)
  deltat <- 1/freq
  # round. <- function(x) deltat * round(x/deltat)
  round. <- function(x) deltat * floor(x/deltat+0.5)
  tt <- round.(as.numeric(time(x)))
  tt2 <- round.(seq(head(tt,1), tail(tt,1), deltat))
  xx <- merge(zoo(coredata(x), tt), zoo(, tt2))
  ts(coredata(xx), start = tt[1], frequency = freq)
}

as.ts.zoo <- function(x, ...)
{
  if(is.regular(x)) {
    attr(x, "frequency") <- frequency(x)
    return(as.ts.zooreg(x))
  } else {
    warning(paste(sQuote("x"), "does not have an underlying regularity"))
    return(ts(coredata(x)))
  }
}

as.zoo.zooreg <- function(x, ...) {
  attr(x, "frequency") <- NULL
  class(x) <- "zoo"
  return(x)
}

as.zooreg.zoo <- function(x, ...)
{
  freq <- frequency(x)
  if(!is.null(freq)) {
    attr(x, "frequency") <- freq
    class(x) <- c("zooreg", "zoo")
  } else {
    warning(paste(sQuote("x"), "does not have an underlying regularity"))
    x <- zooreg(coredata(x))
  }
  return(x)
}
