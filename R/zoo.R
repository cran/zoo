zoo <- function(x, order.by)
{
  index <- order(order.by)
  order.by <- order.by[index]

  if(NROW(x) != length(index)) stop(paste("dimensions of", dQuote("x"), "and", dQuote("order.by"), "do not match"))
  
  if(is.vector(x))
    x <- x[index]
  else if(is.matrix(x))
    x <- x[index, , drop = FALSE]
  else if(is.data.frame(x))
    x <- x[index, , drop = FALSE]  
  else
    stop(paste(dQuote("x"), "has to be a vector or matrix"))

  attr(x, "index") <- order.by
  class(x) <- "zoo"
  return(x)
}

"[.zoo" <- function(x, i, j, drop = NULL, ...)
{
  if(!is.zoo(x)) stop("method is only for zoo objects")
  x.index <- index(x)
  attr(x, "index") <- NULL
  nclass <- class(x)[-(1:which(class(x) == "zoo"))]
  if(length(nclass) < 1) nclass <- NULL 
  class(x) <- nclass
  if(NCOL(x) < 2) {
    x <- as.matrix(x)
    drop <- TRUE
  } else {
    drop <- FALSE
  }
  if(missing(i)) i <- 1:nrow(x)
  if(missing(j)) j <- 1:ncol(x)
  return(zoo(x[i, j, drop = drop, ...], x.index[i]))
}

print.zoo <- function(x, ...)
{
  if(!is.zoo(x)) stop("method is only for zoo objects")
  x.index <- index(x)
  attr(x, "index") <- NULL
  nclass <- class(x)[-(1:which(class(x) == "zoo"))]
  if(length(nclass) < 1) nclass <- NULL 
  class(x) <- nclass
  cat("Value:\n")
  print(x)
  cat("\nIndex:\n")
  print(x.index)
  invisible(x)
}

is.zoo <- function(object)
  inherits(object, "zoo")



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



as.zoo <- function(x, ...)
{
  UseMethod("as.zoo")
}

as.zoo.default <- function(x, ...)
{
  zoo(structure(x, dim = dim(x)), index(x))
}
  
as.zoo.ts <- function(x, ...)
{
  rval <- as.vector(x)
  dim(rval) <- dim(x)
  zoo(rval, time(x))
}  

as.zoo.irts <- function(x, ...)
{
  zoo(x$value, x$time)
}



plot.zoo <- function(x,
  plot.type = c("multiple", "single"), panel = lines,
  xlab = "Index", ylab = NULL, main = NULL, ylim = NULL,
  oma = c(6, 0, 5, 0), col = 1, lty = 1, nc, ...)
{
  plot.type <- match.arg(plot.type)
  nser <- NCOL(x)
  x.index <- index(x)
  if(is.ts(x.index)) x.index <- as.vector(x.index)

  if(plot.type == "multiple" && nser > 1) {
    if(is.null(main)) main <- deparse(substitute(x))
    if(is.null(ylab)) ylab <- colnames(x)
    if(is.null(ylab)) ylab <- paste("Series", 1:nser)
    ylab <- rep(ylab, length.out = nser)
    col <- rep(col, length.out = nser)
    lty <- rep(lty, length.out = nser)

    panel <- match.fun(panel)
    if(nser > 10) stop("Can't plot more than 10 series")
    if(missing(nc)) nc <- if(nser >  4) 2 else 1
    oldpar <- par("mar", "oma", "mfcol")
    on.exit(par(oldpar))
    par(mar = c(0, 5.1, 0, 2.1), oma = oma)
    nr <- ceiling(nser / nc)
    par(mfcol = c(nr, nc))
    for(i in 1:nser) {
      if(i%%nr==0 || i == nser)
        plot(x.index, x[, i], xlab= "", ylab= ylab[i], type = "n", ...)
      else {      
        plot(x.index, x[, i], axes = FALSE, xlab= "", ylab= ylab[i], type = "n", ...)
        box()
        axis(2, xpd = NA)
      }
      panel(x.index, x[, i], col = col[i], lty = lty[i], ...)
    }
    par(oldpar)
  } else {
    if(is.null(ylab)) ylab <- deparse(substitute(x))
    if(is.null(main)) main <- ""
    if(is.null(ylim)) ylim <- range(x)
    
    col <- rep(col, length.out = nser)
    dummy <- rep(range(x), length.out = length(index(x)))
	    
    plot(x.index, dummy, xlab= xlab, ylab= ylab[1], type = "n", ylim = ylim, ...)
    box()
    y <- as.matrix(x)
    for(i in 1:nser) {
      panel(x.index, y[, i], col = col[i], lty = lty[i], ...)
    }
  }
  title(main)
  return(invisible(x))
}

lines.zoo <- function(x, type = "l", ...)
{
  if(NCOL(x) == 1) lines(index(x), x, type = type, ...)
    else stop("Can't plot lines for multivariate zoo object")
}

