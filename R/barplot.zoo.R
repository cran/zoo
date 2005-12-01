barplot.zoo <- function(height, names = NULL, ...)
{
  x <- coredata(height)
  if(!is.null(dim(x))) x <- t(x)
  if(is.null(names)) names <- index2char(index(height))
  barplot(x, names = names, ...)
}
