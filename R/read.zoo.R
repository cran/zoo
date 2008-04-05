read.zoo <- function(file, format = "", tz = "", FUN = NULL,
  regular = FALSE, index.column = 1, aggregate = FALSE, ...)
{
  ## `file' and `...' is simply passed to read.table
  ## the first column is interpreted to be the index, the rest the coredata
  ## it is transformed to an arbitrary index class by `FUN'
  ## defaults for `FUN' are guessed and are numeric, Date or POSIXct

  ## read data
  rval <- read.table(file, ...)

  ## if `file' does not contain data
  if(NROW(rval) < 1) {
    if(is.data.frame(rval)) rval <- as.matrix(rval)
    if(NCOL(rval) > 1) rval <- rval[,-index.column]
    rval <- zoo(rval)
    return(rval)
  }

  ## extract index
  if(NCOL(rval) < 1) stop("data file must specify at least one column")
  
  ## extract index, retain rest of the data
  if (NCOL(rval) == 1) ix <- seq(length = NROW(rval))
  else {
    ix <- rval[,index.column]
    rval <- rval[,-index.column]
  }
  if(is.factor(ix)) ix <- as.character(ix)
  if(is.data.frame(rval)) rval <- as.matrix(rval)
    
  ## index transformation functions
  toDate <- if(missing(format)) function(x, ...) as.Date(as.character(x))
              else function(x, format) as.Date(as.character(x), format = format)
  toPOSIXct <- if (missing(format)) {
        function(x, tz) as.POSIXct(as.character(x), tz = tz)
  } else function(x, format, tz) {
        as.POSIXct(strptime(as.character(x), tz = tz, format = format))
  }
  toDefault <- function(x, ...) {
    rval <- try(toPOSIXct(x), silent = TRUE)
    if(inherits(rval, "try-error"))
      rval <- try(toDate(x), silent = TRUE)
    else {
      hms <- as.POSIXlt(rval)
      hms <- hms$sec + 60 * hms$min + 3600 * hms$hour
      if(isTRUE(all.equal(hms, rep.int(hms[1], length(hms))))) {
        rval2 <- try(toDate(x), silent = TRUE)
        if(!inherits(rval2, "try-error")) rval <- rval2
      }
    }
    if(inherits(rval, "try-error")) rval <- rep(NA, length(x))
    return(rval)
  }
  toNumeric <- function(x, ...) x
  
  ## setup default FUN
  if(is.null(FUN)) {
    FUN <- if (!missing(tz)) toPOSIXct
        else if (!missing(format)) toDate
        else if (is.numeric(ix)) toNumeric
        else toDefault
  }
  
  ## compute index from (former) first column
  ix <- if (missing(format)) {
    if (missing(tz)) FUN(ix) else FUN(ix, tz = tz)
  } else {
    if (missing(tz)) FUN(ix, format = format) 
    else FUN(ix, format = format, tz = tz)
  }
  
  ## sanity checking
  if(any(is.na(ix))) stop("index contains NAs")
  if(length(ix) != NROW(rval)) stop("index does not match data")
  
  ## setup zoo object and return 
  ## Suppress duplicates warning if aggregate specified
  if(identical(aggregate, TRUE)) {
    agg.fun <- mean
  } else if(identical(aggregate, FALSE)) {
    agg.fun <- NULL
  } else {
    agg.fun <- match.fun(aggregate)
    if(!is.function(agg.fun)) stop(paste("invalid specification of", sQuote("aggregate")))
  }
  remove(list = "aggregate")
  withCallingHandlers(rval <- zoo(rval, ix), warning = 
    function(w) {
        if (!is.null(agg.fun) && !is.na(pmatch("some methods for", w$message)))
            invokeRestart("muffleWarning")
    }
  )

  if(regular && is.regular(rval)) rval <- as.zooreg(rval)
  if (!is.null(agg.fun)) rval <- aggregate(rval, time(rval), agg.fun)
  return(rval)
}

write.zoo <- function(x, file = "", index.name = "Index",
  row.names = FALSE, col.names = NULL, ...)
{
  if(is.null(col.names)) col.names <- !is.null(colnames(x))
  dx <- as.data.frame(x)  
  stopifnot(all(names(dx) != index.name))
  dx[[index.name]] <- index(x)
  dx <- dx[, c(ncol(dx), 1:(ncol(dx)-1))]
  write.table(dx, file = file, row.names = row.names, col.names = col.names, ...)
}
