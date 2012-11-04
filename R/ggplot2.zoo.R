fortify.zoo <- function(model, data, melt = FALSE, ...)
{
  ## dimensions
  n <- NROW(model)
  k <- NCOL(model)

  ## series labels
  lab <- colnames(model)
  if(is.null(lab)) lab <- rep.int(deparse(substitute(model)), k)
  lab <- make.unique(lab)
  
  ## either long format (melt = TRUE) or wide format (melt = FALSE)
  if(melt) {
    df <- if(k == 1L) {
      data.frame(index(model), factor(rep.int(1, n), labels = lab), coredata(model))
    } else {
      data.frame(index(model)[rep.int(1:n, k)],
        factor(rep(1:k, each = n), levels = 1:k, labels = lab),
	as.vector(coredata(model)))
    }
    names(df) <- c("Index", "Series", "Value")
  } else {
    df <- cbind(data.frame(index(model)), coredata(model))
    names(df) <- c("Index", lab)  
  }
  
  return(df)
}


autoplot.zoo <- function(object, geom = "line", facets, ...)
{
  ## need ggplot2 package
  stopifnot(require("ggplot2"))

  ## convert to data.frame (and assure correct label
  ## processing by fortify.zoo)
  lab <- deparse(substitute(object))
  if(NCOL(object) == 1L) {
    if(is.null(dim(object))) dim(object) <- c(NROW(object), 1L)
    if(is.null(colnames(object))) colnames(object) <- lab
  }
  if(is.null(colnames(object))) colnames(object) <- paste(lab, 1:NCOL(object), sep = ".")
  df <- fortify.zoo(object, melt = TRUE)

  ## default for facets
  single <- nlevels(df$Series) == 1L
  if(missing(facets)) {
    auto <- TRUE
    facets <- if(single) NULL else Series ~ .
  } else {
    auto <- FALSE
  }  

  ## "fake" variables for nonstandard evaluation
  Index <- Value <- Series <- NULL
  
  ## call qplot
  gg <- if(single | (!is.null(facets) & auto)) {
    qplot(Index, Value, data = df, geom = geom, facets = facets, ...) + ylab(if(single) levels(df$Series) else "") + xlab("Index")
  } else {
    qplot(Index, Value, data = df, group = Series, geom = geom, facets = facets, colour = Series, linetype = Series, shape = Series,
      ...) + ylab("") + xlab("Index")
  }
  return(gg)
}

facet_free <- function (facets = Series ~ ., margins = FALSE, scales = "free_y", ...) {
    stopifnot(require("ggplot2"))
	facet_grid(facets, margins = margins, scales = scales, ...)
}

