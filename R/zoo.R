zoo <- function (x, order.by = index(x), frequency = NULL) 
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
        # y <- format(eval(as.matrix(x), parent.frame(n = 3)))
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


scale.zoo <- function (x, center = TRUE, scale = TRUE) {
	x[] <- xs <- scale(coredata(x), center = center, scale = scale)
	attributes(x) <- c(attributes(x), attributes(xs))
	x
}

