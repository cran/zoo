index <- function(x, ...)
{
  UseMethod("index")
}

index.default <- function(x, ...)
{
  seq(length = NROW(x))
}

index.zoo <- function(x, ...)
{
  attr(x, "index")
}

time.zoo <- function(x, ...)
{
  index(x)
}

"index<-" <- function(x, value) 
{
	UseMethod("index<-")
}

"time<-" <- function(x, value) 
{
	UseMethod("time<-")
}

"index<-.zoo" <- function(x, value) 
{
	if(length(index(x)) != length(value)) 
	  stop("length of index vectors does not match")
	attr(x, "index") <- value
	return(x)
}

"time<-.zoo" <- function(x, value) 
{
	if(length(index(x)) != length(value)) 
	  stop("length of time vectors does not match")
	attr(x, "index") <- value
	return(x)
}

start.zoo <- function(x, ...) 
{
	if (length(index(x)) > 0) index(x)[1]
	  else NULL
}

end.zoo <- function(x, ...) 
{
	lx <- length(index(x))
	if (lx > 0) index(x)[lx]
	  else NULL
}
