plot.zoo <- function(x,
  plot.type = c("multiple", "single"), panel = lines, 
  xlab = "Index", ylab = NULL, main = NULL, ylim = NULL,
  oma = c(6, 0, 5, 0), col = 1, lty = 1, pch = 1, nc, ...)
{
  parm <- function(nams, x, n, def, recycle = sum(unnamed) > 0) {
  # if nams are the names of our variables and x is a parameter
  # specification such as list(a = c(1,2), c(3,4)) then 
  # create a new list which uses the named variables from x
  # and assigns the unnamed in order.  For the remaining variables
  # assign them the default value if recycle = FALSE or recycle the
  # unnamed variables if recycle = TRUE.  The default value for
  # recycle is TRUE if there is at least one unnamed variable
  # in x and is false if there are only named variables in x.
	stopifnot(all(names(x) %in% c("", nams)))
	y <- vector(mode = "list", length = length(nams))
	names(y) <- nams
	in.x <- nams %in% names(x)
	unnamed <- if (is.null(names(x))) rep(TRUE, length(x)) else names(x) == ""
	if (!recycle) y[] <- def
	y[in.x] <- x[nams[in.x]]
	if (recycle) {
		stopifnot(sum(unnamed) > 0)
		y[!in.x] <- x[unnamed]
	} else {
		y[which(!in.x)[seq(len=sum(unnamed))]] <- x[unnamed]
	}
	lapply(y, function(y) if (length(y)==1) y else rep(y, length.out = n))
  }
  recycle <- function(a, len, nser)
     rep(lapply(as.list(a), rep, length.out = len), length.out = nser)
  plot.type <- match.arg(plot.type)
  dots <- list(...)
  nser <- NCOL(x)
  x.index <- index(x)
  if(is.ts(x.index)) x.index <- as.vector(x.index)
  cn <- if (is.null(colnames(x))) paste("V", seq(NCOL(x)), sep = "")
	else colnames(x)

  if(plot.type == "multiple" && nser > 1) {
    if(is.null(main)) main <- deparse(substitute(x))
    if(is.null(ylab)) ylab <- colnames(x)
    if(is.null(ylab)) ylab <- paste("Series", 1:nser)
    ylab <- rep(ylab, length.out = nser)
    lty <- rep(lty, length.out = nser)
    col <- parm(cn, as.list(col), NROW(x), 1)
    pch <- parm(cn, as.list(pch), NROW(x), par("pch"))

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
	args <- list(x.index, x[,i], xlab = "", ylab = ylab[i], ...)
	args$type <- "n"
	do.call("plot", args)
	mtext(xlab, side = 1, line = 3)
      } else {      
        args <- list(x.index, x[,i], axes = FALSE, xlab = "", ylab = ylab[i], ...)
	args$type <- "n"
	do.call("plot", args)
        box()
        axis(2, xpd = NA)
      }
      panel(x.index, x[, i], col = col[[i]], pch = pch[[i]], lty = lty[i], ...)
    }
    par(oldpar)
  } else {
    if(is.null(ylab)) ylab <- deparse(substitute(x))
    if(is.null(main)) main <- ""
    if(is.null(ylim)) ylim <- range(x, na.rm = TRUE)

    lty <- rep(lty, length.out = nser)
    col <- parm(cn, as.list(col), NROW(x), 1)
    pch <- parm(cn, as.list(pch), NROW(x), par("pch"))
   
    dummy <- rep(range(x, na.rm = TRUE), length.out = length(index(x)))
	    
    args <- list(x.index, dummy, xlab = xlab, ylab = ylab[1], ylim = ylim, ...)
    args$type <- "n"
    do.call("plot", args)
    box()
    y <- as.matrix(x)
    for(i in 1:nser) {
      panel(x.index, y[, i], col = col[[i]], pch = pch[[i]], lty = lty[i], ...)
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
