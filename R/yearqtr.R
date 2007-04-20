## class creation
yearqtr <- function(x) structure(floor(4*x + .001)/4, class = "yearqtr")

## coercion to yearqtr: always go via numeric
as.yearqtr <- function(x, ...) UseMethod("as.yearqtr")
as.yearqtr.default <- function(x, ...) as.yearqtr(as.numeric(x))
as.yearqtr.numeric <- function(x, ...) structure(floor(4*x + .0001)/4, class = "yearqtr")
as.yearqtr.integer <- function(x, ...) structure(x, class = "yearqtr")
as.yearqtr.dates <-
as.yearqtr.Date <- 
as.yearqtr.POSIXt <- function(x, ...) as.yearqtr(as.yearmon(x))
as.yearqtr.character <- function(x, ...) as.yearqtr(as.Date(x, ...))

## coercion from yearqtr
# returned Date is the fraction of the way through the period given by frac
as.Date.yearqtr <- function(x, frac = 0, ...) {
	x <- unclass(x)
	year <- floor(x + .001)
	month <- floor(12 * (x - year) + 1 + .5 + .001)
	dd.start <- as.Date(paste(year, month, 1, sep = "-"))
	dd.end <- dd.start + 100 - as.numeric(format(dd.start + 100, "%d")) 
	as.Date((1-frac) * as.numeric(dd.start) + frac * as.numeric(dd.end))
}
as.POSIXct.yearqtr <- function(x, tz = "") as.POSIXct(as.Date(x), tz = tz)
as.POSIXlt.yearqtr <- function(x, tz = "") as.POSIXlt(as.Date(x), tz = tz)
as.numeric.yearqtr <- function(x, ...) unclass(x)
as.character.yearqtr <- function(x, ...) format.yearqtr(x, ...)

## other methods for class yearqtr
c.yearqtr <- function(...)
    as.yearqtr(do.call("c", lapply(list(...), as.numeric)))

format.yearqtr <- function(x, ...) 
{
	x <- unclass(x)
	year <- floor(x + .001)
	qtr <- floor(4*(x - year) + 1 + .5 + .001)
	xx <- paste(year, " Q", qtr, sep = "")
	names(xx) <- names(x)
	xx
}

print.yearqtr <- function(x, ...) { 
    print(format(x), ...)
    invisible(x) 
}

"[.yearqtr" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

axis.yearqtr <- function (side, x, at, format, ...) 
    axis.Date(side, as.Date(x), at, format, ...)

MATCH.yearqtr <- function(x, table, nomatch = NA, ...)
    match(floor(4*as.numeric(x) + .001), floor(4*as.numeric(table) + .001), nomatch = nomatch, ...)

Ops.yearqtr <- function(e1, e2) {
    e1 <- as.numeric(e1)
    e2 <- as.numeric(e2)
    rval <- NextMethod(.Generic)
    if(is.numeric(rval)) rval <- as.yearqtr(rval)
    return(rval)
}


"-.yearqtr" <- function (e1, e2) 
{
    if (!inherits(e1, "yearqtr")) 
        stop("Can only subtract from yearqtr objects")
    if (nargs() == 1) 
	return(- as.numeric(e1))
    if (inherits(e2, "yearqtr")) 
        return(as.numeric(e1) - as.numeric(e2))
    if (!is.null(attr(e2, "class"))) 
      stop("can only subtract yearqtr objects and numbers from yearqtr objects")
    structure(unclass(as.yearqtr(e1)) - e2, class = "yearqtr")
}


axis.yearqtr <- function(side, x, ...) axis.Date(side, as.Date(x), ...)

Axis.yearqtr <- function(x=NULL, at=NULL, ..., side, labels=NULL)
	Axis(x=as.Date(x), at=at, ..., side=side, labels=labels)

