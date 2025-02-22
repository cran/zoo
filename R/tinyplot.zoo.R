tinyplot.zoo <- function(object, facet, type = "l", facet.args = list(free = TRUE), ylab = "", ...)
{
  ## convert to data.frame (and assure correct label processing by fortify.zoo)
  lab <- deparse(substitute(object))
  if(NCOL(object) == 1L) {
    if(is.null(dim(object))) dim(object) <- c(NROW(object), 1L)
    if(is.null(colnames(object))) colnames(object) <- lab
  }
  if(is.null(colnames(object))) colnames(object) <- paste(lab, 1:NCOL(object), sep = ".")
  df <- fortify.zoo(object, melt = TRUE)

  ## default for facet
  single <- nlevels(df$Series) == 1L
  if(missing(facet)) {
    auto <- TRUE
    facet <- if(single) NULL else ~ Series
  } else {
    auto <- FALSE
  }
  if (is.null(facet)) facet.args <- NULL
  
  ## call tinyplot
  if(single | (!is.null(facet) & auto)) {
    tinyplot::tinyplot(Value ~ Index, data = df, facet = facet, facet.args = facet.args, type = type, ylab = ylab, ...)
  } else {
    tinyplot::tinyplot(Value ~ Index | Series, data = df, facet = facet, facet.args = facet.args, type = type, ylab = ylab, ...)
  }
}
