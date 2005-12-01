zoo <- function (x, order.by = index(x), frequency = NULL) 
{
    index <- ORDER(order.by)
    order.by <- order.by[index]
    if (missing(x) || is.null(x)) 
        x <- numeric()
    else if (is.vector(x)) 
        x <- rep(x, length.out = length(index))[index]
    else if (is.factor(x))         
        x <- factor(rep(as.character(x), length.out = length(index))[index], labels = levels(x))
    else if (is.matrix(x) || is.data.frame(x)) 
        x <- (x[rep(1:NROW(x), length.out = length(index)), , 
            drop = FALSE])[index, , drop = FALSE]
    else stop(paste(dQuote("x"), ": attempt to define illegal zoo object"))
    if(is.matrix(x) || is.data.frame(x)) x <- as.matrix(x)

    if(!is.null(frequency)) {
        d <- try(diff(as.numeric(order.by)), silent = TRUE)
	ok <- if(class(d) == "try-error" || length(d) < 1 || any(is.na(d))) FALSE
	else {	    
            deltat <- min(d)
	    dd <- d/deltat
	    if(identical(all.equal(dd, round(dd)), TRUE)) {	    
                freq <- 1/deltat
                if(freq > 1 && identical(all.equal(freq, round(freq)), TRUE)) freq <- round(freq)
  	        identical(all.equal(frequency %% freq, 0), TRUE)
	    } else {
	        FALSE
	    }
	}
	if(!ok) {
	  warning(paste(dQuote("order.by"), "and", dQuote("frequency"),
	          "do not match:", dQuote("frequency"), "ignored"))
	  frequency <- NULL
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
        y <- format(eval(as.matrix(x), parent.frame(n = 3)))
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
	summary(cbind(data.frame(Index = index(object)), y), ...)
}


is.zoo <- function(object)
  inherits(object, "zoo")

str.zoo <- function(object, ...)
{
  str(unclass(object), ...)
}

"[.zoo" <- function(x, i, j, drop = TRUE, ...)
{
  if(!is.zoo(x)) stop("method is only for zoo objects")
  x.index <- index(x)
  rval <- coredata(x)
  if(missing(i)) i <- 1:NROW(rval)

  ## also support that i can be index:
  ## if i is not numeric/integer/logical, it is interpreted to be the index
  if(!(all(class(i) == "numeric") ||
       all(class(i) == "integer") ||
       all(class(i) == "logical")))
    i <- which(x.index %in% i)
  
  if(length(dim(rval)) == 2) {
	if (length(i) == 1) drop <- FALSE
        if(missing(j)) 
		rval <- zoo(rval[i, , drop = drop, ...], x.index[i])
	  else
		rval <- zoo(rval[i, j, drop = drop, ...], x.index[i])
  } else
	rval <- zoo(rval[i], x.index[i])

  attr(rval, "oclass") <- attr(x, "oclass")
  attr(rval, "levels") <- attr(x, "levels")
  attr(rval, "frequency") <- attr(x, "frequency")
  if(!is.null(attr(rval, "frequency"))) class(rval) <- c("zooreg", class(rval))

  return(rval)
}

head.zoo <- function(x, n = 6, ...) {
	if (length(dim(x)) == 0)
		x[seq(length = min(n, length(x)))]
	else
		x[seq(length = min(n, nrow(x))),, drop = FALSE]
}
 
tail.zoo <- function(x, n = 6, ...) {
	if (length(dim(x)) == 0)
		x[seq(to = length(x), length = min(n, length(x)))]
	else
		x[seq(to = nrow(x), length = min(n, nrow(x))),, drop = FALSE]
}

range.zoo <- function(..., na.rm = FALSE)
    range(sapply(list(...), coredata), na.rm = na.rm)
