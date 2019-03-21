na.trim <- function(object, ...) UseMethod("na.trim")
na.trim.default <- function (object, sides = c("both", "left", "right"), 
	is.na = c("any", "all"), maxgap = Inf, ...)
{
   is.na <- match.arg(is.na, c("any", "all"))
   nisna <- if (is.na == "any" || length(dim(object)) < 2L)  {
	complete.cases(object)
   } else rowSums(!is.na(object)) > 0
   rlength <- function(x) if(all(!x)) length(x) else min(which(x)) - 1L
   idx <- switch(match.arg(sides),
       left = {
           idx0 <- cumsum(nisna) > 0
	   idx0 | rlength(idx0) > maxgap
       },
       right = {
           idx0 <- cumsum(rev(nisna) > 0) > 0
	   rev(idx0) | rlength(idx0) > maxgap
       },
       both = {
           idx0l <- cumsum(nisna) > 0
	   idx0r <- cumsum(rev(nisna) > 0) > 0
	   (idx0l | rlength(idx0l) > maxgap) & (rev(idx0r) | rlength(idx0r) > maxgap)
       }
   )
   if (length(dim(object)) < 2L)
       object[idx]
   else
       object[idx,, drop = FALSE]
}

## need a 'ts' method because indexing destroys ts attributes
na.trim.ts <- function (object, ...)
{
    as.ts(na.trim(as.zoo(object), ...))
}
