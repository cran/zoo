## class creation
yearmon <- function(x) structure(floor(12*x + .0001)/12, class = "yearmon")

## coercion to yearmon: always go via numeric
as.yearmon <- function(x, ...) UseMethod("as.yearmon")
as.yearmon.default <- function(x, ...) as.yearmon(as.numeric(x))
as.yearmon.numeric <- function(x, ...) yearmon(x)
as.yearmon.integer <- function(x, ...) structure(x, class = "yearmon")
as.yearmon.dates <- 
as.yearmon.Date <- 
as.yearmon.POSIXt <- function(x, ...) as.yearmon(with(as.POSIXlt(x, tz="GMT"), 1900 + year + mon/12))
as.yearmon.character <- function(x, format = "", ...) {
   if (format == "") {
        nch <- nchar(gsub("[^-]", "", x))
        if (length(table(nch)) != 1) 
            stop("yearmon variable can only have one format")
        format <- if (nch == 1) "%Y-%m" else "%Y-%m-%d"
   }
   has.short.keys <- rep(regexpr("%[mbByY%]", format) > 0, length(x))
   has.no.others <- regexpr("%", gsub("%[mbByY%]", "", format)) < 0
   z <- ifelse(has.short.keys & has.no.others,
      as.Date( paste("01", x, sep = "-"), paste("%d", format, sep = "-"), ... ),
      as.Date(x, format, ...))
   as.yearmon(as.Date(z))
}

## coercion from yearmon
# returned Date is the fraction of the way through the period given by frac
as.Date.yearmon <- function(x, frac = 0, ...) {
     x <- unclass(x)
     year <- floor(x + .001)
     month <- floor(12 * (x - year) + 1 + .5 + .001)
     dd.start <- as.Date(paste(year, month, 1, sep = "-")) 
     dd.end <- dd.start + 32 - as.numeric(format(dd.start + 32, "%d"))
     as.Date((1-frac) * as.numeric(dd.start) + frac * as.numeric(dd.end), origin = "1970-01-01")
}
as.POSIXct.yearmon <- function(x, tz = "", ...) as.POSIXct(as.Date(x), tz = tz, ...)
as.POSIXlt.yearmon <- function(x, tz = "", ...) as.POSIXlt(as.Date(x), tz = tz, ...)
as.numeric.yearmon <- function(x, ...) unclass(x)
as.character.yearmon <- function(x, ...) format.yearmon(x, ...)
as.data.frame.yearmon <- function(x, row.names = NULL, optional = FALSE, ...) 
{
  nrows <- length(x)
  nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
  if (is.null(row.names)) {
    if (nrows == 0) 
        row.names <- character(0)
    else if(length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
    }
    else if(optional) row.names <- character(nrows)
    else row.names <- seq_len(nrows)
  }
  names(x) <- NULL
  value <- list(x)
  if(!optional) names(value) <- nm
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}

## other methods for class yearmon
c.yearmon <- function(...)
    as.yearmon(do.call("c", lapply(list(...), as.numeric)))

format.yearmon <- function(x, format = "%b %Y", ...) 
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

MATCH.yearmon <- function(x, table, nomatch = NA, ...)
    match(floor(12*as.numeric(x) + .001), floor(12*as.numeric(table) + .001), nomatch = nomatch, ...)

Ops.yearmon <- function(e1, e2) {
    e1 <- as.numeric(as.yearmon(e1))
    e2 <- as.numeric(as.yearmon(e2))
    rval <- NextMethod(.Generic)
    if(is.numeric(rval)) rval <- as.yearmon(rval)
    return(rval)
}

"-.yearmon" <- function (e1, e2) 
{
    if (!inherits(e1, "yearmon")) 
        stop("Can only subtract from yearmon objects")
    if (nargs() == 1) 
	return(- as.numeric(e1))
    if (inherits(e2, "yearmon")) 
        return(as.numeric(e1) - as.numeric(e2))
    if (!is.null(attr(e2, "class"))) 
      stop("can only subtract yearmon objects and numbers from yearmon objects")
    structure(unclass(as.yearmon(e1)) - e2, class = "yearmon")
}

Axis.yearmon <- function(x = NULL, at = NULL, ..., side, labels = NULL)
    axis.yearmon(x = x, at = at, ..., side = side, labels = TRUE)

axis.yearmon <- function (side, x, at, format, labels = TRUE, ..., N1 = 25, N2 = 2) {
    # If years in range > N1 then only years shown.  
    # If years in range > N2 then month ticks are not labelled.
    mat <- missing(at) || is.null(at)
    if (!mat) # at not missing
        x <- as.yearmon(at)
    else x <- as.yearmon(x)
    range <- par("usr")[if (side%%2) 
        1:2
    else 3:4]
    # range[1] <- ceiling(range[1])
    # range[2] <- floor(range[2])
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    class(z) <- "yearmon"
    if (d > N1) { # axis has years only
        z <- structure(pretty(z), class = "yearmon")
    } else if (d > N2) { # axis has all years and unlabelled months
        z <- seq(min(x), max(x), 1/12)
	# z <- seq(floor(min(x)), ceiling(max(x)))
    } else { # years and months
        z <- seq(min(x), max(x), 1/12)
    }
    if (!mat) 
        z <- x[is.finite(x)]
    z <- z[z >= range[1] & z <= range[2]]
    z <- sort(unique(z))
    class(z) <- "yearmon"
    if (identical(labels, TRUE)) {
	if (missing(format)) format <- c("%Y", "%b")
	if (length(format) == 1) format <- c(format, "")
	if (d <= N2) labels <- format.yearmon(z, format = format[2])
	idx <- format.yearmon(z, format = "%m") == "01"
	labels[idx] <- format.yearmon(z[idx], format = format[1])
    } else if (identical(labels, FALSE)) 
        labels <- rep("", length(z))
    axis(side, at = z, labels = labels, ...)
}

summary.yearmon <- function(object, ...)
  summary(as.numeric(object), ...)
