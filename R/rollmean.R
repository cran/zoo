# rollmean, rollmax, rollmedian (, rollmad) based on code posted by Jarek Tuszynski at
# https://www.stat.math.ethz.ch/pipermail/r-help/2004-October/057363.html
# ToDo: rollmad, currently rapply() can be used

rollmean <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
  UseMethod("rollmean")

rollmean.default <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
  x <- unclass(x)
  n <- length(x) 
  y <- x[k:n] - x[c(1, 1:(n-k))] # difference from previous
  y[1] <- sum(x[1:k])		 # find the first
  # apply precomputed differencest sum
  rval <- cumsum(y)/k
  if (na.pad) {
    rval <- switch(match.arg(align),
      "left" = { c(rval, rep(NA, k-1)) },
      "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
      "right" = { c(rep(NA, k-1), rval) })
  }
  return(rval)
}

rollmean.zoo <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{ 
  stopifnot(k <= NROW(x))
  index.x <- index(x)
  if(!na.pad) {
    n <- length(index.x)
    ix <- switch(match.arg(align),
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })
    index.x <- index.x[ix]
  }
  if(length(dim(x)) == 0) 
    return(zoo(rollmean.default(coredata(x), k, na.pad, align), index.x, attr(x, "frequency")))
  else
    return(zoo(apply(coredata(x), 2, rollmean.default, k=k, na.pad=na.pad, align=align),
        index.x, attr(x, "frequency")))
}

rollmean.ts <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
  as.ts(rollmean(as.zoo(x), k = k, na.pad = na.pad, align = align, ...))


rollmax <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
  UseMethod("rollmax")

rollmax.default <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
  n <- length(x) 
  rval <- rep(0, n) 
  a <- 0
  for (i in k:n) {
  rval[i] <- if (is.na(a) || is.na(rval[i=1]) || a==rval[i-1]) 
      max(x[(i-k+1):i]) # calculate max of window
  else 
      max(rval[i-1], x[i]); # max of window = rval[i-1] 
  a <- x[i-k+1] # point that will be removed from window
  }
  rval <- rval[-seq(k-1)]
  if (na.pad) {
    rval <- switch(match.arg(align),
      "left" = { c(rval, rep(NA, k-1)) },
      "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
      "right" = { c(rep(NA, k-1), rval) })
  }
  return(rval)
} 

rollmax.zoo <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{ 
  stopifnot(k <= NROW(x))
  index.x <- index(x)
  if(!na.pad) {
    n <- length(index.x)
    ix <- switch(match.arg(align),
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })
    index.x <- index.x[ix]
  }
  if (length(dim(x)) == 0) 
    return(zoo(rollmax.default(coredata(x), k, na.pad, align), index.x, attr(x, "frequency")))
  else
    return(zoo(apply(coredata(x), 2, rollmax.default, k=k, na.pad = na.pad, align=align), index.x,
       attr(x, "frequency")))
}

rollmax.ts <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
  as.ts(rollmax(as.zoo(x), k = k, na.pad = na.pad, align = align, ...))



rollmedian <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
  UseMethod("rollmedian")

rollmedian.default <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
  ## interfaces runmed from `stats'
  stopifnot(k <= length(x), k %% 2 == 1)
  n <- length(x)
  m <- k %/% 2
  rval <- runmed(x, k, ...)
  attr(rval, "k") <- NULL
  rval <- rval[-c(1:m, (n-m+1):n)]
  if (na.pad) {
    rval <- switch(match.arg(align),
      "left" = { c(rval, rep(NA, k-1)) },
      "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
      "right" = { c(rep(NA, k-1), rval) })
  }
  return(rval)
}

rollmedian.zoo <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...) { 
  stopifnot(all(!is.na(x)), k <= NROW(x), k %% 2 == 1)
  # todo:
  # rather than abort we should do a simple loop to get the medians
  # for those columns with NAs.
  index.x <- index(x)
  m <- k %/% 2
  n <- NROW(x)
  align <- match.arg(align)
  
  if(!na.pad) {
    n <- length(index.x)
    ix <- switch(align,
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })
    index.x <- index.x[ix]
  }
  
  rollmedian0 <- function(x, k, na.pad, ...) {
    x <- runmed(x, k, ...)[-c(seq(m),seq(to=n,len=m))]
    if (na.pad) {
      x <- switch(align,
        "left" = { c(x, rep(NA, k-1)) },
        "center" = { c(rep(NA, floor((k-1)/2)), x, rep(NA, ceiling((k-1)/2))) },
        "right" = { c(rep(NA, k-1), x) })
    }
    return(x)
  }
  if (length(dim(x)) == 0)
    return(zoo(rollmedian0(coredata(x), k, na.pad = na.pad, ...), index.x,
      attr(x, "frequency")))
  else
    return(zoo(apply(coredata(x), 2, rollmedian0, k = k, na.pad = na.pad, ...), 
      index.x, attr(x, "frequency")))
}

rollmedian.ts <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
  as.ts(rollmedian(as.zoo(x), k = k, na.pad = na.pad, align = align, ...))
