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
    if(is.null(drop)) drop <- TRUE
  } else {
    if(is.null(drop)) drop <- FALSE
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
      if(i%%nr==0 || i == nser) {
        plot(x.index, x[, i], xlab= "", ylab= ylab[i], type = "n", ...)
	mtext(xlab, side = 1, line = 3)
      } else {      
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
    lty <- rep(lty, length.out = nser)
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

str.zoo <- function(object, ...)
{
  str(unclass(object), ...)
}

window.zoo <- function(x, start = NULL, end = NULL, ...)
{
  indexes <- index(x)

  if(is.null(start)) {
    if(is.null(end)) {
      return(x)
    } else {
      wi <- which(indexes <= end)
      return(x[wi,,])
    }
  } else {
    if(is.null(end)) {
      wi <- which(indexes >= start)
    } else {
      wi <- which(indexes >= start & indexes <= end)
    }
    return(x[wi,,])
  }
}

rbind.zoo <- function(..., deparse.level = 1)
{  
  args <- list(...)
  indexes <- do.call("c", lapply(args, index))

  my.table <- function(x) {
    x <- x[order(x)]
    table(match(x,x))
  }
  if(max(my.table(indexes)) > 1) stop("indexes overlap")

  ncols <- sapply(args, NCOL)  
  if(!all(ncols == ncols[1])) stop("number of columns differ")

  if(ncols[1] > 1)
    zoo(do.call("rbind", lapply(args, unclass)), indexes)
  else
    zoo(do.call("c", lapply(args, unclass)), indexes)
}

merge.zoo <- function(..., all = TRUE)
{
  ## args <- list(x, y, ...)
  args <- list(...)
  if(!all(unlist(lapply(args, is.zoo)))) stop("all arguments must be zoo objects")

  makeNames <- function(l) {
      nm <- names(l)
      fixup <- if (is.null(nm)) 
	  seq(along = l)
      else nm == ""
      dep <- sapply(l[fixup], function(x) deparse(x)[1])
      if (is.null(nm)) 
	  return(dep)
      if (any(fixup)) 
	  nm[fixup] <- dep
      nm
  }
  ## zoonames <- makeNames(as.list(substitute(list(x, y, ...)))[-1])
  zoonames <- makeNames(as.list(substitute(list(...)))[-1])

  all <- rep(all, length(args))
  indexlist <- lapply(args, index)
  indexclasses <- sapply(indexlist, function(x) class(x)[1])
  if(!all(indexclasses == indexclasses[1]))
    warning("not all indexes are of the same class")
  indexes <- do.call("c", indexlist)

  sort.unique <- function(x) {
    x <- x[match(x,x) == seq(length = length(x))]
    x[order(x)]
  }
  my.table <- function(x) {
    x <- x[order(x)]
    table(match(x,x))
  }

  indexintersect <- sort.unique(indexes)[my.table(indexes) == length(args)]
  indexunion <- do.call("c", indexlist[all])
  if(is.null(indexunion)) indexunion <- indexes[0] 

  indexes <- sort.unique(c(indexunion, indexintersect))
  match0 <- function(a, b, nomatch = 0, ...) match(a, b, nomatch = nomatch, ...)

  f <- function(a) {
    z <- matrix(NA, length(indexes), NCOL(a))
    z[match0(index(a), indexes),] <- a[match0(indexes, index(a)),, drop = FALSE]
    z
  }

  rval <- do.call("cbind", lapply(args, f))
  
  fixcolnames <- function(a) {
    if(NCOL(a) < 2) {
      return("")
    } else {
      rval <- colnames(a)
      if(is.null(rval)) { 
        rval <- as.character(1:NCOL(a))
      } else {
        rval[rval == ""] <- as.character(which(rval == ""))
      }
      rval <- paste(".", rval, sep = "")
      return(rval)
    }
  }
  
  zoocolnames <- lapply(args, fixcolnames)
  zoocolnames <- unlist(lapply(seq(along = args), function(i) paste(zoonames[i], zoocolnames[[i]], sep = "")))
  colnames(rval) <- zoocolnames
  
  zoo(rval, indexes)
}
