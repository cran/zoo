# only change is to allow type to be a list so you can specify a mixture
# points, lines, steps, etc. one one plot command.

plot.zoo <- function(x, screens = 1,
  plot.type = c("multiple", "single"), panel = lines, 
  xlab = "Index", ylab = NULL, main = NULL, ylim = NULL,
  oma = c(6, 0, 5, 0), mar = c(0, 5.1, 0, 2.1), 
  col = 1, lty = 1, pch = 1, type = "l", nc, widths = 1, heights = 1, ...)
{
  parm <- function(nams, x, n, m, def, recycle = sum(unnamed) > 0) {
  # if nams are the names of our variables and x is a parameter
  # specification such as list(a = c(1,2), c(3,4)) then 
  # create a new list which uses the named variables from x
  # and assigns the unnamed in order.  For the remaining variables
  # assign them the default value if recycle = FALSE or recycle the
  # unnamed variables if recycle = TRUE.  The default value for
  # recycle is TRUE if there is at least one unnamed variable
  # in x and is false if there are only named variables in x.
  # n is the length of the series and m is the total number of series
  # It only needs to know whether m is 1 or greater than m.
	stopifnot(all(names(x) %in% c("", nams)))
	if (!is.list(x)) x <- if (nser == 1) list(x) else as.list(x)
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
  cn <- if (is.null(colnames(x))) paste("V", seq(length = nser), sep = "")
	else colnames(x)

  screens <- parm(cn, screens, NROW(x), nser, 1)
  screens <- as.factor(unlist(screens))[drop = TRUE]
  ngraph <- length(levels(screens))
  if(nser > 1 && (plot.type == "multiple" || ngraph > 1)) {
    if (ngraph == 1) { 
	screens <- as.factor(seq(nser))
	ngraph <- nser
    }
    if(is.null(main)) main <- deparse(substitute(x))
    main.outer <- TRUE
    if(is.null(ylab)) ylab <- colnames(x)[!duplicated(screens)]
    if(is.null(ylab)) ylab <- paste("Series", which(!duplicated(screens)))
    ylab <- rep(ylab, length.out = ngraph)
    lty <- rep(lty, length.out = nser)
    col <- parm(cn, col, NROW(x), nser, 1)
    pch <- parm(cn, pch, NROW(x), nser, par("pch"))
    type <- parm(cn, type, NROW(x), nser, "l")
    if (!is.null(ylim)) {
        if (is.list(ylim)) ylim <- lapply(ylim, range, na.rm = TRUE)
	else ylim <- list(range(ylim, na.rm = TRUE))
	ylim <- lapply(parm(cn, ylim, 2, nser, NULL), function(x) 
		if (is.null(x) || length(na.omit(x)) ==0) NULL 
		else range(x, na.rm = TRUE))
    }
    panel <- match.fun(panel)
    if(missing(nc)) nc <- if(ngraph >  4) 2 else 1
    oldpar <- par(no.readonly = TRUE)
    on.exit({ par(oldpar) })
    nr <- ceiling(ngraph / nc)
    layout(matrix(seq(nr*nc), nr), widths = widths, heights = heights)
    par(mar = mar, oma = oma)
    ranges <- if (is.null(ylim))
	by(1:ncol(x), screens, function(idx) range(x[,idx], na.rm = TRUE))
        else by(1:ncol(x), screens, function(idx) 
		if (is.null(ylim[[idx]])) range(x[,idx], na.rm = TRUE)
		else ylim[[idx]])
    for(j in seq(along = levels(screens))) {
      range. <- rep(ranges[[j]], length.out = length(time(x)))
      if(j%%nr==0 || j == length(levels(screens))) {
	args <- list(x.index, range., xlab = "", ylab = ylab[j], ...)
	args$type <- "n"
	do.call("plot", args)
	mtext(xlab, side = 1, line = 3)
      } else {      
        args <- list(x.index, range., axes = FALSE, xlab = "", ylab = ylab[j], ...)
	args$type <- "n"
	do.call("plot", args)
        box()
        axis(2, xpd = NA)
      }

	for(i in which(screens == levels(screens)[j]))
	      panel(x.index, x[, i], 
                col = col[[i]], pch = pch[[i]], 
		lty = lty[i], type = type[[i]], ...)
	}
  } else {
    if(is.null(ylab)) ylab <- deparse(substitute(x))
    if(is.null(main)) main <- ""
    main.outer <- FALSE
    if(is.null(ylim)) ylim <- range(x, na.rm = TRUE)
	else ylim <- range(c(ylim, recursive = TRUE), na.rm = TRUE)

    lty <- rep(lty, length.out = nser)
    col <- parm(cn, col, NROW(x), nser, 1)
    pch <- parm(cn, pch, NROW(x), nser, par("pch"))
    type <- parm(cn, type, NROW(x), nser, "l")
   
    dummy <- rep(range(x, na.rm = TRUE), 
	length.out = length(index(x)))
	    
    args <- list(x.index, dummy, xlab = xlab, ylab = ylab[1], ylim = ylim, ...)
    args$type <- "n"
    do.call("plot", args)
    box()
    y <- as.matrix(x)
    for(i in 1:nser) {
      panel(x.index, y[, i], col = col[[i]], pch = pch[[i]], lty = lty[i], 
		type = type[[i]], ...)
    }
  }
  title(main, outer = main.outer)
  return(invisible(x))
}

lines.zoo <- function(x, type = "l", ...)
{
  if(NCOL(x) == 1) lines(index(x), x, type = type, ...)
    else stop("Can't plot lines for multivariate zoo object")
}

