window.zoo <- function(x, index = index.zoo(x), start = NULL, end = NULL, ...)
{
  all.indexes <- index.zoo(x)
  in.index <- all.indexes %in% index

  if(length(start) == 2 && !is.null(attr(x, "frequency")) && is.numeric(all.indexes)) {
    freq <- attr(x, "frequency")
    start <- floor(start[1]*freq + (start[2] - 1) + .0001)/freq
  }
  if(length(end) == 2 && !is.null(attr(x, "frequency")) && is.numeric(all.indexes)) {
    freq <- attr(x, "frequency")
    end <- floor(end[1]*freq + (end[2] - 1) + .0001)/freq
  }

  if(is.null(start)) {
    if(is.null(end)) {
      wi <- which(all.indexes %in% index)
      return(x[wi,,])
    } else {
      wi <- which(in.index & all.indexes <= end)
      return(x[wi,,])
    }
  } else {
    if(is.null(end)) {
      wi <- which(in.index & all.indexes >= start)
    } else {
      wi <- which(in.index & all.indexes >= start & all.indexes <= end)
    }
    return(x[wi,,])
  }
}

"window<-.zoo" <- function(x, index = index.zoo(x), start = NULL, end = NULL, ..., value)
{
  ix <- index.zoo(x)
  stopifnot(all(index %in% ix))
  
  if(length(start) == 2 && !is.null(attr(x, "frequency")) && is.numeric(ix)) {
    freq <- attr(x, "frequency")
    start <- floor(start[1]*freq + (start[2] - 1) + .0001)/freq
  }
  if(length(end) == 2 && !is.null(attr(x, "frequency")) && is.numeric(ix)) {
    freq <- attr(x, "frequency")
    end <- floor(end[1]*freq + (end[2] - 1) + .0001)/freq
  }
  
  if (!is.null(start)) index <- index[index >= start]
  if (!is.null(end)) index <- index[index <= end]

  wi <- which(ix %in% index)
  if (length(dim(x)) == 0)
  	  x[wi] <- value
  else
  	  x[wi,] <- value
  return(x)
}
 
lag.zoo <- function(x, k = 1, ...)
{
   if (length(k) > 1) {
	if (is.null(names(k))) names(k) <- paste("lag", k, sep = "")
	return(do.call("merge.zoo", lapply(k, lag.zoo, x = x, ...)))
   }
   nr <- NROW(x)
   if (k != round(k)) {
	k <- round(k)
	warning("k is not an integer")
   }
   if (k == 0) return(x)
   if (abs(k) > nr) k <- nr
   if (k > 0)  {
	   xx <- x[-seq(1, length = k),,]
	   attr(xx, "index") <- index(x)[-seq(to = nr,length = k)]
   } else {
	   xx <- x[-seq(to = nr, length = -k),,]
	   attr(xx, "index") <- index(x)[-seq(1, length = -k)]
   }
   return(xx)
}



lag.zooreg <- function(x, k = 1, ...)
{
   if (length(k) > 1) {
	if (is.null(names(k))) names(k) <- paste("lag", k, sep = "")
	return(do.call("merge.zoo", lapply(k, lag.zoo, x = x, ...)))
   }
   nr <- NROW(x)
   freq <- attr(x, "frequency")
   
   if (k != round(k)) warning("k is not an integer")
   k <- round(k)

   ix <- index(x)
   ix <- if(identical(class(ix), "numeric") | identical(class(ix), "integer"))
     floor(freq*ix - k + .0001)/freq else ix - k/freq
   index(x) <- ix
   
   return(x)
}

diff.zoo <- function(x, lag = 1, differences = 1, arithmetic = TRUE, ...)
{
    stopifnot(lag >= 1, differences >= 1)
    if (!arithmetic) x <- log(x)
    for(i in 1:differences) {
	x <- x - lag(x, k = -lag)
    }
    if (!arithmetic) x <- exp(x)
    return(x)
}

