aggregate.zoo <- function(x, by, FUN, ..., regular = NULL, frequency = NULL)
{
  ## index processing
  my.unique <- function(x) x[MATCH(x, x) == seq(length = length(x))]
  my.sort <- function(x) x[order(x)]
  if(is.function(by)) by <- by(index(x))
  if(!is.list(by)) by <- list(by)

  ## sanity checks and option processing
  stopifnot(length(time(x)) == length(by[[1]]))
  if(is.null(frequency)) {
    if(is.null(regular)) regular <- inherits(x, "zooreg")
  } else {
    if(identical(regular, FALSE)) warning(paste(sQuote("regular"), "is ignored"))
    regular <- TRUE
  }

  ## aggregate data
  df <- aggregate(coredata(x), by, match.fun(FUN), ...)
  if(length(unique(as.character(df[,1]))) == length(df[,1]))
      row.names(df) <- df[, 1]
  df <- df[, -1]
  if(is.matrix(x)) df <- as.matrix(df)
  
  ## regularity processing, set up return value
  ix <- my.sort(my.unique(by[[1]]))
  rval <- zoo(df, ix)
  
  if(regular) {
    freq <- ifelse(is.null(frequency), frequency(rval), frequency)
    rval <- zoo(df, ix, freq)
  }
  
  return(rval)
}
