na.spline <- function(object, ...) UseMethod("na.spline")

# interpolates object along along which defaults to index(object)
# along has to be numeric, is otherwise coerced
na.spline.default <- function(object, along = index(object), na.rm = TRUE, ...)
{
	along <- as.numeric(along)
	na.spline.0 <- function(y) {
		na <- is.na(y)
		if(all(!na)) return(y)
		y[na] <- splinefun(along[!na], y[!na], ...)(along[na])
		return(y)
	}

        object[] <- if (length(dim(object)) == 0) na.spline.0(object)
        	else apply(object, 2, na.spline.0)
        if (na.rm) na.omit(object) else object
}

