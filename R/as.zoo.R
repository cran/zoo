as.zoo <- function(x, ...)
{
  UseMethod("as.zoo")
}

as.zoo.default <- function(x, ...)
{
  if(is.zoo(x)) x
    else zoo(structure(x, dim = dim(x)), index(x))
}

as.zoo.factor <- function(x, ...) 
{
  L <- list(...)
  stopifnot(length(L) < 2)
  if (length(L))
	zoo(x, list(...)[[1]])
  else
	zoo(x)
}
  
as.zoo.ts <- function(x, ...)
{
  zoo(coredata(x), unclass(time(x)))
}  

as.zoo.irts <- function(x, ...)
{
  zoo(x$value, x$time)
}

as.zoo.its <- function(x, ...) 
{
	index <- attr(x, "dates")
	class(x) <- attr(x, "dates") <- NULL
	zoo(x, index)
}

as.its.zoo <- function(x) {
	stopifnot(require(its))
	index <- attr(x, "index")
	stopifnot(inherits(index, "POSIXct"))
	attr(x, "index") <- NULL
	its(unclass(x), index)
}


as.vector.zoo <- function(x, mode = "any")
	as.vector(as.matrix(x), mode = mode)

as.matrix.zoo <- function (x) 
{
    y <- as.matrix(unclass(x))
    attr(y, "index") <- NULL
    if (length(y) > 0) 
	    colnames(y) <- if (length(colnames(x)) > 0) 
		colnames(x)
	    else {
		lab <- deparse(substitute(x))
		if (NCOL(x) == 1) 
		    lab
		else paste(lab, 1:NCOL(x), sep = ".")
	    }
    return(y)
}

as.data.frame.zoo <- function(x, row.names = NULL, optional = FALSE)
{
	y <- as.data.frame(unclass(x))
        if(NCOL(x) > 0) {
		colnames(y) <- if (length(colnames(x)) > 0) 
			colnames(x)
		else {
			lab <- deparse(substitute(x))
			if (NCOL(x) == 1) lab
	                  else paste(lab, 1:NCOL(x), sep = ".")
		}
	}
	if (!is.null(row.names)) row.names(y) <- row.names
	return(y)
}

as.list.zoo <- function(x, ...) {
	if (length(dim(x)) == 0) list(x)
  		else lapply(as.data.frame(x), zoo, index(x))
}

as.list.ts <- function(x, ...) {
	if (is.matrix(x))
		lapply(as.data.frame(x), ts, 
			start = start(x), end = end(x), freq = frequency(x))
	else
		list(x)
}
