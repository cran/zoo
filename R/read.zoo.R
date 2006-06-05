read.zoo <- function(file, format = "", tz = "", FUN = NULL, regular = FALSE, ...)
{
  ## `file' and `...' is simply passed to read.table
  ## the first column is interpreted to be the index, the rest the coredata
  ## it is transformed to an arbitrary index class by `FUN'
  ## defaults for `FUN' are guessed and are numeric, Date or POSIXct

  ## read data
  rval <- read.table(file, ...)

  ## extract index
  if(NCOL(rval) < 1) stop("data file must specify at least one column")
  
  ## extract index, retain rest of the data
  if (NCOL(rval) == 1) ix <- seq(length = NROW(rval))
  else {
              ix <- rval[,1]
              rval <- rval[,-1]
  }
  if(is.data.frame(rval)) rval <- as.matrix(rval)
    
  ## index transformation functions
  toDate <- if(format == "") function(x) as.Date(as.character(x))
              else function(x) as.Date(as.character(x), format = format)
  toPOSIXct <- function(x) as.POSIXct(as.character(x), tz = tz)
  toDefault <- function(x) {
    rval <- try(toDate(x), silent = TRUE)
    if(class(rval) == "try-error") rval <- try(toPOSIXct(x), silent = TRUE)
    if(class(rval) == "try-error") rval <- rep(NA, length(x))
    return(rval)
  }
  toNumeric <- function(x) x
  
  ## setup default FUN
  if(is.null(FUN)) {
    FUN <- if(format != "") toDate
           else if(tz != "") toPOSIXct
           else if(is.numeric(ix)) toNumeric
           else toDefault        
  }
  
  ## compute index from (former) first column
  ix <- FUN(ix)
  
  ## sanity checking
  if(any(is.na(ix))) stop("index contains NAs")
  if(length(ix) != NROW(rval)) stop("index does not match data")
  
  ## setup zoo object and return
  rval <- zoo(rval, ix)
  if(regular && is.regular(rval)) rval <- as.zooreg(rval)
  return(rval)
}
