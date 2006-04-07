na.trim <- function(object, ...) UseMethod("na.trim")
na.trim.default <- function (object, sides = c("both", "left", "right"), ...)
{
   nisna <- complete.cases(object)
   idx <- switch(match.arg(sides), left = cumsum(nisna) > 0,
       right = rev(cumsum(rev(nisna) > 0)),
       both = (cumsum(nisna) > 0) & rev(cumsum(rev(nisna)) > 0))
   if (length(dim(object)) == 0)
       object[idx]
   else
       object[idx,]
}

