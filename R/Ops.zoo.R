
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
    if (is.null(attr(e, "index"))) 
	zoo(e, index(e1))
    else
	e
}

t.zoo <- function(x)
	t(as.matrix.zoo(x))
 
cumsum.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cumsum(x)
	  else x[] <- apply(x, 2, cumsum)
	return(x)
}


cumprod.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cumprod(x)
	  else x[] <- apply(x, 2, cumprod)
	return(x)
}


cummin.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cummin(x)
	  else x[] <- apply(x, 2, cummin)
	return(x)
}


cummax.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cummax(x)
	  else x[] <- apply(x, 2, cummax)
	return(x)
}
