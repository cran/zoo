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

as.zoo.matrix <- function(x, ...) 
{
  zoo(x, ...)
}

as.zoo.data.frame <- function(x, ...) 
{
  zoo(as.matrix(x), ...)
}

as.zoo.fts <- function(x, ...) 
{
	stopifnot(require("fts"))
	zoo(as.matrix(x), attr(x, "dates"))
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

# as.mcmc.default can handle other direction
as.zoo.mcmc <- function(x, ...)
{
	stopifnot(require("coda"))
	as.zoo(as.ts(x, ...))
}

as.zoo.timeSeries <- function(x, ...) {
  stopifnot(require("timeSeries"))
  zoo(as.matrix(x), timeSeries::time(x), ...)  
}

as.zoo.xts <- function(x, ...) {
  stopifnot(require("xts"))
  zoo(coredata(x), order.by = index(x), ...)
}

as.zooreg.xts <- function(x, frequency = NULL, ...) {
  stopifnot(require("xts"))
  as.zooreg(as.zoo(x, ...), frequency = frequency)
}

as.zoo.zoo <- function(x, ...) x

## This should be in its now.
## as.its.zoo <- function(x) {
## 	stopifnot(require("its"))
## 	index <- index(x)
## 	stopifnot(inherits(index, "POSIXct"))
## 	its(coredata(x), index)
## }

as.vector.zoo <- function(x, mode = "any")
	as.vector(as.matrix(x), mode = mode)

as.matrix.zoo <- function(x, ...) 
{
    y <- as.matrix(coredata(x), ...)
    if (identical(coredata(x), numeric(0))) dim (y) <- c(0, 0)
    if (length(y) > 0) {
	    colnames(y) <- if (length(colnames(x)) > 0) 
		colnames(x)
	    else {
		lab <- deparse(substitute(x))
		if (NCOL(x) == 1) 
		    lab
		else paste(lab, 1:NCOL(x), sep = ".")
	    }
	} else if (nrow(y) != length(index(x))) {
		dim(y) <- c(length(index(x)), 0)
	}
    if (!is.null(y) && nrow(y) > 0 && is.null(row.names(y))) 
		row.names(y) <- index2char(index(x), frequency = attr(x, "frequency"))
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
	    if (NROW(y) > 0 && !any(duplicated(tmp))) row.names(y) <- tmp
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

as.zooreg <- function(x, frequency = NULL, ...)
{
  UseMethod("as.zooreg")
}

as.zooreg.default <- function(x, frequency = NULL, ...)
{
  as.zooreg(as.zoo(x, ...), frequency = frequency)
}

as.zooreg.ts <- as.zoo.ts <- function(x, frequency = NULL, ...)
{
  xtsp <- tsp(x)
  if(is.null(frequency)) frequency <- xtsp[3]
  zooreg(coredata(x), start = xtsp[1], end = xtsp[2], frequency = frequency)
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

as.zooreg.zoo <- function(x, frequency = NULL, ...)
{
  if(!is.null(frequency)) {
    frequency(x) <- frequency
  } else {
    freq <- frequency(x)
    if(!is.null(freq)) {
      attr(x, "frequency") <- freq
      class(x) <- c("zooreg", "zoo")
    } else {
      warning(paste(sQuote("x"), "does not have an underlying regularity"))
      x <- zooreg(coredata(x))
    }
  }
  return(x)
}
