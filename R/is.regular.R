is.regular <- function(x, strict = FALSE) {
  UseMethod("is.regular")
}

is.regular.zoo <- function(x, strict = FALSE)
{
  delta <- try(diff(as.numeric(index(x))))
  if(class(delta) == "try-error") FALSE
  else if(strict) identical(all.equal(delta, rep.int(delta[1], length(delta))), TRUE)
  else identical(all.equal(delta/min(delta), round(delta/min(delta))), TRUE)
}

is.regular.ts <- function(x, strict = FALSE) TRUE

is.regular.zooreg <- function(x, strict = FALSE)
{
  if(strict) is.regular.zoo(x, strict = TRUE) else TRUE
}

is.regular.default <- function(x, strict = FALSE) {
  is.regular(as.zoo(x), strict = strict)
}

frequency.zooreg <- function(x, ...) 
{
  attr(x, "frequency")
}

frequency.zoo <- function(x, ...)
{
  freq <- attr(x, "frequency")
  if(!is.null(freq)) return(freq)

  d <- try(diff(as.numeric(index(x))))
  reg <- if(class(d) == "try-error") FALSE
    else identical(all.equal(d/min(d), round(d/min(d))), TRUE)
  if(!reg) return(NULL)

  deltat <- min(d)
  freq <- 1/deltat
  if(freq > 1 && identical(all.equal(freq, round(freq)), TRUE)) freq <- round(freq)
  return(freq)
}

"frequency<-" <- function(x, value)
  UseMethod("frequency<-")
  
"frequency<-.zoo" <- function(x, value) {
  ix <- try(as.numeric(index(x)))
  freqOK <- if(class(ix) == "try-error") FALSE
    else identical(all.equal(ix*value, round(ix*value)), TRUE)
  stopifnot(freqOK)
  attr(x, "frequency") <- value
  class(x) <- c("zooreg", "zoo")
  return(x)
}

"frequency<-.zooreg" <- function(x, value) {
  stopifnot(identical(all.equal(as.numeric(index(x)) * value, round(as.numeric(index(x))*value)), TRUE))
  attr(x, "frequency") <- value
  return(x)
}

deltat.zoo <- function(x, ...)
{
  rval <- frequency.zoo(x, ...)
  if(is.null(rval)) NULL else 1/rval
}

deltat.zooreg <- function(x, ...)
{
  1/frequency.zooreg(x, ...)
}

cycle.zooreg <- function(x, ...)
{
  freq <- frequency(x)
  ix <- as.numeric(index(x))
  d <- diff(ix)
  if(!identical(all.equal(freq*d, round(freq*d)), TRUE))
    stop(paste(sQuote("cycle"), "not available for", sQuote("x")))  
  return(zoo(round((ix - floor(ix)) * freq) + 1, order.by = index(x), freq))
}

cycle.zoo <- function(x, ...)
{
  if(is.regular(x)) cycle.zooreg(x)
    else stop(sQuote("x"), "is not regular")
}
