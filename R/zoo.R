zoo <- function (x = NULL, order.by = index(x), frequency = NULL) 
{
    ## process index "order.by"    
    if(length(unique(MATCH(order.by, order.by))) < length(order.by))
      warning(paste("some methods for", dQuote("zoo"),
      "objects do not work if the index entries in", sQuote("order.by"), "are not unique"))
    index <- ORDER(order.by)
    order.by <- order.by[index]

    if(missing(x) || is.null(x)) 
      x <- numeric()
    else if (is.vector(x)) 
      x <- rep(x, length.out = length(index))[index]
    else if (is.factor(x))         
      x <- factor(rep(as.character(x), length.out = length(index))[index],
        levels = levels(x), ordered = is.ordered(x))
    else if (is.matrix(x) || is.data.frame(x)) 
      x <- (x[rep(1:NROW(x), length.out = length(index)), , 
        drop = FALSE])[index, , drop = FALSE]
    else stop(paste(dQuote("x"), ": attempt to define illegal zoo object"))
    if(is.matrix(x) || is.data.frame(x)) x <- as.matrix(x)

    if(!is.null(frequency)) {
      delta <- suppressWarnings(try(diff(as.numeric(order.by)), silent = TRUE))
      freqOK <- if(class(delta) == "try-error" || any(is.na(delta))) FALSE
        else if(length(delta) < 1) TRUE
        else identical(all.equal(delta*frequency, round(delta*frequency)), TRUE)
      if(!freqOK) {
        warning(paste(dQuote("order.by"), "and", dQuote("frequency"),
        	"do not match:", dQuote("frequency"), "ignored"))
        frequency <- NULL
      } else {
        if(frequency > 1 && identical(all.equal(frequency, round(frequency)), TRUE))
	  frequency <- round(frequency)
      }
    }

    attr(x, "oclass") <- attr(x, "class")
    attr(x, "index") <- order.by
    attr(x, "frequency") <- frequency
    class(x) <- if(is.null(frequency)) "zoo" else c("zooreg", "zoo")
    return(x)
}

print.zoo <- function (x, style = ifelse(length(dim(x)) == 0,
    "horizontal", "vertical"), quote = FALSE, ...) 
{
    style <- match.arg(style, c("horizontal", "vertical", "plain"))
    if (is.null(dim(x)) && length(x) == 0) style <- "plain"
    if (length(dim(x)) > 0 && style == "horizontal") style <- "plain"
    if (style == "vertical") {
	y <- as.matrix(coredata(x))
        if (length(colnames(x)) < 1) {
            colnames(y) <- rep("", NCOL(x))
        }
        rownames(y) <- index2char(index(x), frequency = attr(x, "frequency"))
        print(y, quote = quote, ...)
    }
    else if (style == "horizontal") {
        y <- as.vector(x)
        names(y) <- index2char(index(x), frequency = attr(x, "frequency"))
        print(y, quote = quote, ...)
    }
    else {
        x.index <- index(x)
        cat("Data:\n")
        print(coredata(x))
        cat("\nIndex:\n")
        print(x.index)
    }
    invisible(x)
}

summary.zoo <- function(object, ...) 
{
	y <- as.data.frame(object)
	if (length(colnames(object)) < 1) {
		lab <- deparse(substitute(object))
		colnames(y) <- if (NCOL(object) == 1) lab
		  else paste(lab, 1:NCOL(object), sep=".")
	}
	if (NROW(y) > 0) {
		summary(cbind(data.frame(Index = index(object)), y), ...)
	} else summary(data.frame(Index = index(object)), ...)
}


is.zoo <- function(object)
  inherits(object, "zoo")

str.zoo <- function(object, ...)
{
  cls <- if(inherits(object, "zooreg")) "zooreg" else "zoo"
  if(NROW(object) < 1) cat(paste(sQuote(cls), "series (without observations)\n")) else {
    cat(paste(sQuote(cls), " series from ", start(object), " to ", end(object), "\n", sep = ""))
    cat("  Data:")
    str(coredata(object), ...)
    cat("  Index: ")
    str(index(object), ...)
    if(cls == "zooreg") cat(paste("  Frequency:", attr(object, "frequency"), "\n"))
  }
}

"[.zoo" <- function(x, i, j, drop = TRUE, ...)
{
  if(!is.zoo(x)) stop("method is only for zoo objects")
  x.index <- index(x)
  rval <- coredata(x)
  if(missing(i)) i <- 1:NROW(rval)

  ## also support that i can be index:
  ## if i is not numeric/integer/logical, it is interpreted to be the index
  if (all(class(i) == "logical"))
    i <- which(i)
  else if (inherits(i, "zoo") && all(class(coredata(i)) == "logical")) {
    i <- which(coredata(merge(zoo(,time(x)), i)))
  } else if(!((all(class(i) == "numeric") || all(class(i) == "integer")))) 
    i <- which(MATCH(x.index, i, nomatch = 0) > 0)
  
  if(length(dim(rval)) == 2) {
	drop. <- if (length(i) == 1) FALSE else drop
        rval <- if (missing(j)) rval[i, , drop = drop., ...]
		else rval[i, j, drop = drop., ...]
	if (drop && length(rval) == 1) rval <- c(rval)
	rval <- zoo(rval, x.index[i])
  } else
	rval <- zoo(rval[i], x.index[i])

  attr(rval, "oclass") <- attr(x, "oclass")
  attr(rval, "levels") <- attr(x, "levels")
  attr(rval, "frequency") <- attr(x, "frequency")
  if(!is.null(attr(rval, "frequency"))) class(rval) <- c("zooreg", class(rval))

  return(rval)
}

"$.zoo" <- function(object, x) {
  if(length(dim(object)) != 2) stop("not possible for univariate zoo series")
  if(is.null(colnames(object))) stop("only possible for zoo series with column names")
  wi <- pmatch(x, colnames(object))
  if(is.na(wi)) NULL else object[, wi]
}

"$<-.zoo" <- function(object, x, value) {
  if(length(dim(object)) != 2) stop("not possible for univariate zoo series")
  if(is.null(colnames(object))) stop("only possible for zoo series with column names")
  wi <- match(x, colnames(object))
  if(is.na(wi)) {
    object <- cbind(object, value)
    colnames(object)[NCOL(object)] <- x  
  } else {
    if(is.null(value)) {
      object <- object[, -wi, drop=FALSE]
    } else {   
      object[, wi] <- value
    }
  }
  object
}

head.zoo <- function(x, n = 6, ...) {
	stopifnot(length(n) == 1L)
	xlen <- NROW(x)
    n <- if (n < 0L) 
        max(NROW(x) + n, 0L)
    else min(n, xlen)
	if (length(dim(x)) == 0) x[seq_len(n)]
	else x[seq_len(n),, drop = FALSE]
}
 
tail.zoo <- function(x, n = 6, ...) {
    stopifnot(length(n) == 1L)
    xlen <- NROW(x)
    n <- if (n < 0L) 
        max(xlen + n, 0L)
    else min(n, xlen)
	if (length(dim(x)) == 0) x[seq.int(to = xlen, length.out = n)]
	else x[seq.int(to = xlen, length.out = n),, drop = FALSE]
}

range.zoo <- function(..., na.rm = FALSE)
    range(sapply(list(...), coredata), na.rm = na.rm)


scale.zoo <- function (x, center = TRUE, scale = TRUE) {
	x[] <- xs <- scale(coredata(x), center = center, scale = scale)
	attributes(x) <- c(attributes(x), attributes(xs))
	x
}

with.zoo <- function(data, expr, ...) {
    stopifnot(length(dim(data)) == 2)
    eval(substitute(expr), as.list(data), enclos = parent.frame())
}

xtfrm.zoo <- function(x) coredata(x)

subset.zoo <- function (x, subset, select, drop = FALSE, ...) 
{
    if (missing(select)) 
        vars <- TRUE
    else {
        nl <- as.list(1:ncol(x))
        names(nl) <- colnames(x)
        vars <- eval(substitute(select), nl, parent.frame())
    }
    if (missing(subset)) {
        subset <- rep(TRUE, NROW(x))
    } else {
        e <- substitute(subset)
	if("time" %in% colnames(x)) {
	  xdf <- as.data.frame(x)
          subset <- eval(e, xdf, parent.frame())
          xdf$time <- time(x)
          subset2 <- eval(e, xdf, parent.frame())
	  if(!identical(subset, subset2))
  	      warning("'time' is a column in 'x' (not the time index)")
	} else {
          subset <- eval(e, cbind(as.data.frame(x), time = time(x)), parent.frame())
	}
        if (!is.logical(subset)) stop("'subset' must be logical")
    }
    x[subset & !is.na(subset), vars, drop = drop]
}

names.zoo <- function(x) {
  cx <- coredata(x)
  if(is.matrix(cx)) colnames(cx) else names(cx)
}

"names<-.zoo" <- function(x, value) {
  if(is.matrix(coredata(x))) {
    colnames(x) <- value
  } else {
    names(coredata(x)) <- value
  }
  x
}

rev.zoo <- function(x) {
	zoo(coredata(x), time(x)[rev(ORDER(time(x)))])
}

ifelse.zoo <- function(test, yes, no) {
	merge(test, yes, no, retclass = NULL)
	ifelse(test, yes, no)
}
