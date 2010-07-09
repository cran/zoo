Ops.zoo <- function (e1, e2) 
{
    e <- if (missing(e2)) {
        NextMethod(.Generic)
    }
    else if (any(nchar(.Method) == 0)) {
        NextMethod(.Generic)
    }
    else {
	merge(e1, e2, all = FALSE, retclass = NULL)
        NextMethod(.Generic)
    }
    out <- if (is.null(attr(e, "index"))) 
	zoo(e, index(e1), attr(e1, "frequency"))
    else
	e
    # the next statement is a workaround for a bug in R
	structure(out, class = class(out))
}

t.zoo <- function(x)
	t(as.matrix.zoo(x))
 
cumsum.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cumsum(coredata(x))
	  else x[] <- apply(coredata(x), 2, cumsum)
	return(x)
}


cumprod.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cumprod(coredata(x))
	  else x[] <- apply(coredata(x), 2, cumprod)
	return(x)
}


cummin.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cummin(coredata(x))
	  else x[] <- apply(coredata(x), 2, cummin)
	return(x)
}


cummax.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cummax(coredata(x))
	  else x[] <- apply(coredata(x), 2, cummax)
	return(x)
}
