
na.approx <- function(object, ...) UseMethod("na.approx")

# interpolates object along along which defaults to index(object)
# along has to be numeric, is otherwise coerced
na.approx.default <- function(object, along = index(object), na.rm = TRUE, ...)
{
	along <- as.numeric(along)
	na.approx.0 <- function(y) {
		na <- is.na(y)
		if(all(!na)) return(y)
		y[na] <- approx(along[!na], y[!na], along[na], ...)$y
		return(y)
	}

        object[] <- if (length(dim(object)) == 0) na.approx.0(object)
        	else apply(object, 2, na.approx.0)
        if (na.rm) na.omit(object) else object
}
