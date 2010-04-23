read.zoo <- function(file, format = "", tz = "", FUN = NULL,
  regular = FALSE, index.column = 1, drop = TRUE, FUN2 = NULL, 
  split = NULL, aggregate = FALSE, ...)
{
  ## read data
  rval <- if (is.data.frame(file)) file else read.table(file, ...)

  ## if `file' does not contain data
  if(NROW(rval) < 1) {
    if(is.data.frame(rval)) rval <- as.matrix(rval)
    if(NCOL(rval) > 1) rval <- rval[,-index.column, drop = drop]
    rval2 <- zoo(rval)
    return(rval2)
  }

  ## extract index
  if(NCOL(rval) < 1) stop("data file must specify at least one column")
  
  ## extract index, retain rest of the data
  ix <- if (NCOL(rval) == 1) ix <- seq_len(NROW(rval))
  else rval[,index.column]

  # split. is col number of split column (or Inf, -Inf or NULL)
  split. <- if (is.character(split)) match(split, colnames(rval), nomatch = 0)
  else split

  rval2 <- if (is.null(split.)) {
	 rval[ , - index.column, drop = drop]
  } else {

     split.values <- if (is.character(split) || is.finite(split)) rval[, split]
	 else {
		# Inf: first value in each run is first series, etc.
	    # -Inf: last value in each run is first series, etc.
		if (split == Inf) ave(ix, ix, FUN = seq_along)
	    else if (split == -Inf) ave(ix, ix, FUN = function(x) rev(seq_along(x)))
	    else ix
	 }

	 if (split. == 0) {
		stop(paste("split:", split, "not found in colnames:", colnames(rval)))
	 }
	 rval[,-c(split., index.column), drop = drop]
  }

  if(is.factor(ix)) ix <- as.character(ix)
  rval3 <- if(is.data.frame(rval2)) as.matrix(rval2) else rval2
    
  ## index transformation functions

  toDate <- if(missing(format)) {
     function(x, ...) as.Date(format(x, scientific = FALSE))
  } else {
     function(x, format) as.Date(format(x, scientific = FALSE), format = format)
  }

  toPOSIXct <- if (missing(format)) {
        function(x, tz) as.POSIXct(format(x, scientific = FALSE), tz = tz)
  } else function(x, format, tz) {
        as.POSIXct(strptime(format(x, scientific = FALSE), tz = tz, format = format))
  }

  toDefault <- function(x, ...) {
    rval. <- try(toPOSIXct(x), silent = TRUE)
    if(inherits(rval., "try-error"))
      rval. <- try(toDate(x), silent = TRUE)
    else {
      hms <- as.POSIXlt(rval.)
      hms <- hms$sec + 60 * hms$min + 3600 * hms$hour
      if(isTRUE(all.equal(hms, rep.int(hms[1], length(hms))))) {
        rval2. <- try(toDate(x), silent = TRUE)
        if(!inherits(rval2., "try-error")) rval. <- rval2.
      }
    }
    if(inherits(rval., "try-error")) rval. <- rep(NA, length(x))
    return(rval.)
  }

  toNumeric <- function(x, ...) x
  
  ## setup default FUN
  if(is.null(FUN)) {
    FUN <- if (!missing(tz)) toPOSIXct
        else if (!missing(format)) toDate
        else if (is.numeric(ix)) toNumeric
        else toDefault
  }
  FUN <- match.fun(FUN)
  
  ## compute index from (former) first column
  ix <- if (missing(format)) {
    if (missing(tz)) FUN(ix) else FUN(ix, tz = tz)
  } else {
    if (missing(tz)) FUN(ix, format = format) 
    else FUN(ix, format = format, tz = tz)
  }

  if (!is.null(FUN2)) {
	FUN2 <- match.fun(FUN2)
	ix <- FUN2(ix)
  }
  
  ## sanity checking
  if(any(is.na(ix))) {
    idx <- which(is.na(ix))
	msg <- if (length(idx) == 1)
		paste("index has bad entry at data row", idx)
	else if (length(idx) <= 100)
		paste("index has bad entries at data rows:", paste(idx, collapse = " "))
	else paste("index has", length(idx), "bad entries at data rows:", 
		paste(head(idx, 100), collapse = " "), "...")
	stop(msg)
  }
  if(length(ix) != NROW(rval3)) stop("index does not match data")
  
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

  if (is.null(split)) {

	rval4 <- if (!is.null(agg.fun)) aggregate(zoo(rval3), ix, agg.fun)
	else zoo(rval3, ix)
    rval8 <- if(regular && is.regular(rval4)) as.zooreg(rval4) else rval4

  } else {

	split.matrix <- split.data.frame
	rval4 <- split(rval3, split.values)
	ix <- split(ix, split.values)
	rval5 <- mapply(zoo, rval4, ix)
    rval6 <- if(regular) {
		lapply(rval5, function(x) if (is.regular(x)) as.zooreg(x) else x)
	} else rval5

    rval8 <- if (!is.null(agg.fun)) {
		f.ag <- function(z) aggregate(z, time(z), agg.fun)
		rval7 <- lapply(seq_along(rval6), f.ag)
		do.call(merge, rval7)
    } else rval6

  }
	
  return(rval8)
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
