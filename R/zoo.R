zoo <- function (x, order.by = index(x)) 
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
    attr(x, "oclass") <- attr(x, "class")
    attr(x, "index") <- order.by
    class(x) <- "zoo"
    return(x)
}

print.zoo <-
function (x, style = ifelse(length(dim(x)) == 0, "horizontal", 
    "vertical"), quote = FALSE, ...) 
{
    style <- match.arg(style, c("horizontal", "vertical", "plain"))
    if (is.null(dim(x)) && length(x) == 0) style <- "plain"
    if (length(dim(x)) > 0 && style == "horizontal") style <- "plain"
    if (style == "vertical") {
        y <- format(eval(as.matrix(x), parent.frame(n = 3)))
        if (length(colnames(x)) < 1) {
            colnames(y) <- rep("", NCOL(x))
        }
        rownames(y) <- index2char(index(x))
        print(y, quote = quote, ...)
    }
    else if (style == "horizontal") {
        y <- as.vector(x)
        names(y) <- index2char(index(x))
        print(y, quote = quote, ...)
    }
    else {
        x.index <- index(x)
        attr(x, "index") <- NULL
        cat("Data:\n")
        print(unclass(x))
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
  attr(x, "index") <- NULL
  nclass <- class(x)[-(1:which(class(x) == "zoo"))]
  if(length(nclass) < 1) nclass <- NULL 
  class(x) <- nclass
  if(missing(i)) i <- 1:NROW(x)
  if(length(dim(x)) == 2) {
	# we had previously just j to all cols if missing 
	# but that did not work for zero columns
	# so we now process the two cases separately
        if(missing(j)) 
		zoo(x[i, , drop = drop, ...], x.index[i])
	else
		zoo(x[i, j, drop = drop, ...], x.index[i])
   } else
	zoo(x[i], x.index[i])
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

index2char <- function(x, ...) UseMethod("index2char")
index2char.default <- function(x, ...) as.character(x)
