na.spline <- function(object, ...) UseMethod("na.spline")

na.spline.zoo <- function(object, x = index(object), xout, ..., na.rm = TRUE, maxgap = Inf, along) {

    if (!missing(along)) {
        warning("along to be deprecated - use x instead")
        if (missing(x)) x <- along
    }

    missing.xout <- missing(xout) || is.null(xout)
    if (is.function(x)) x <- x(index(object))
    if (!missing.xout && is.function(xout)) xout <- xout(index(object))
    order.by <- if (missing.xout) index(object) else xout
    xout <- if (missing.xout) x else xout

    if (missing.xout || identical(xout, index(object))) {
        result <- object
    } else {
        object.x <- object
        if (!identical(class(x), class(xout))) {
            index(object.x) <- as.numeric(x)
            xout <- as.numeric(xout)
        } else {
            index(object.x) <- x
        }
        objectm <- merge(object.x, zoo(, xout))
        if (length(dim(objectm)) == 2) colnames(objectm) <- colnames(object)
        result <- window(objectm, index = xout)
    }
    result[] <- na.spline.default(object, x = x, xout = xout, na.rm = FALSE, ..., maxgap = maxgap)
    if ((!missing(order.by) && !is.null(order.by)) || !missing.xout) {
        index(result) <- order.by
    }

    if (na.rm) {
        result <- na.trim(result, is.na = "all", maxgap = maxgap)
    }

    result

}

na.spline.zooreg <- function(object, ...) {
    object. <- structure(object, class = setdiff(class(object), "zooreg"))
    as.zooreg(na.spline(object., ...))
}


na.spline.default <- function(object, x = index(object), xout = x, ..., na.rm = TRUE, maxgap = Inf, along) {

    if (!missing(along)) {
        warning("along to be deprecated - use x instead")
        if (missing(x)) x <- along
    }

    na.spline.vec <- function(x, y, xout = x, ...) {
        na <- is.na(y)
	if(sum(!na) < 1L) {
	    ## splinefun() cannot be applied here, hence simply:
	    yf <- rep.int(NA, length(xout))
	    mode(yf) <- mode(y)
	    if(any(!na)) {
	        if(x[!na] %in% xout) {
		    yf[xout == x[!na]] <- y[!na]
		}
	    }
	    return(yf)
	}
	if(all(!na) && (length(xout) > maxgap) && !all(xout %in% x)) {
	    ## for maxgap to work correctly 'y' has to contain
	    ## actual NAs and be expanded to the full x-index
	    xf <- sort(unique(c(x, xout)))
	    yf <- rep.int(NA, length(xf))
	    yf[MATCH(x, xf)] <- y
	    x <- xf
	    y <- yf
	}
        yf <- splinefun(x[!na], y[!na], ...)(xout)
        if (maxgap < length(y)) {
            ## construct a series like y but with only gaps > maxgap
            ## (actual values don't matter as we only use is.na(ygap) below)
            ygap <- .fill_short_gaps(y, seq_along(y), maxgap = maxgap)
            ## construct y values at 'xout', keeping NAs from ygap
            ## (using indexing, as approx() does not allow NAs to be propagated)
            ix <- splinefun(x, seq_along(y), ...)(xout)
            yx <- ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]), NA, yf)
            yx
        } else {
            yf
        }
    }

    if (!identical(length(x), length(index(object)))) {
        stop("x and index must have the same length")
    }
    x. <- as.numeric(x)
    if (missing(xout) || is.null(xout)) xout <- x.
    xout. <- as.numeric(xout)
    object. <- coredata(object)

    result <- if (length(dim(object.)) < 2) {
        na.spline.vec(x., coredata(object.), xout = xout., ...)
    } else {
        apply(coredata(object.), 2, na.spline.vec, x = x., xout = xout., ...)
    }

    if (na.rm) {
        result <- na.trim(result, is.na = "all", maxgap = maxgap)
    }

    result

}

na.spline.ts <- function(object, ...) {
    as.ts(na.spline(as.zoo(object), ...))
}

