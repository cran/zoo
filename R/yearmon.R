## class creation
yearmon <- function(x) structure(floor(12*x + .0001)/12, class = "yearmon")

## coercion to yearmon: always go via numeric
as.yearmon <- function(x, ...) UseMethod("as.yearmon")
as.yearmon.default <- function(x, ...) as.yearmon(as.numeric(x))
as.yearmon.numeric <- function(x, ...) structure(floor(12*x + .001)/12, class = "yearmon")
as.yearmon.integer <- function(x, ...) structure(x, class = "yearmon")
as.yearmon.dates <- 
as.yearmon.Date <- 
as.yearmon.POSIXt <- function(x, ...) as.yearmon(with(as.POSIXlt(x, tz="GMT"), 1900 + year + mon/12))


## coercion from yearmon
# returned Date is the fraction of the way through the period given by frac
as.Date.yearmon <- function(x, frac = 0, ...) {
     x <- unclass(x)
     year <- floor(x + .001)
     month <- floor(12 * (x - year) + 1 + .5 + .001)
     dd.start <- as.Date(paste(year, month, 1, sep = "-")) 
     dd.end <- dd.start + 32 - as.numeric(format(dd.start + 32, "%d"))
     as.Date((1-frac) * as.numeric(dd.start) + frac * as.numeric(dd.end))
}
as.POSIXct.yearmon <- function(x, tz = "") as.POSIXct(as.Date(x), tz = tz)
as.POSIXlt.yearmon <- function(x, tz = "") as.POSIXlt(as.Date(x), tz = tz)
as.numeric.yearmon <- function(x) unclass(x)
as.character.yearmon <- function(x) format.yearmon(x)

## other methods for class yearmon
c.yearmon <- function(...)
    as.yearmon(do.call("c", lapply(list(...), as.numeric)))

format.yearmon <- function (x, format = "%b %Y", ...) 
{
    xx <- format(as.Date(x), format = format, ...)
    names(xx) <- names(x)
    xx
}

print.yearmon <- function(x, ...) { 
    print(format(x), ...)
    invisible(x) 
}

"[.yearmon" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

axis.yearmon <- function (side, x, at, format, ...) 
    axis.Date(side, as.Date(x), at, format, ...)

MATCH.yearmon <- function(x, table, nomatch = NA, ...)
    match(floor(12*as.numeric(x) + .001), floor(12*as.numeric(table) + .001), nomatch = nomatch, ...)

Ops.yearmon <- function(e1, e2) {
    e1 <- as.numeric(e1)
    e2 <- as.numeric(e2)
    rval <- NextMethod(.Generic)
    if(is.numeric(rval)) rval <- as.yearmon(rval)
    return(rval)
}
