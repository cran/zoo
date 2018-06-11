
# fill is a 3 component list or is coerced to one representing
# fill char to left of leftmost non-NA, fill character to interior NAs
#  and fill char to right of rightmost non-NA
# If component is "extend" then left or rightmost NA is extended or interior
#  NA is linearly interpolated
# If component is NULL then the corresponding NA is dropped.

na.fill0 <- function(object, fill, ix = !is.na(object))
{
  if (length(object) == 0L) {
    object
  } else if (length(fill) == 0L || sum(lengths(as.list(fill))) == 0L) {
    structure(object[ix], na.action = which(!ix))
  } else if (length(fill) == 1L) {
    if (identical(as.list(fill)[[1L]], "extend"))
      stop("fill cannot be 'extend'")
    if (!is.logical(ix)) ix <- seq_along(object) %in% ix
    replace(object, !ix, as.list(fill)[[1L]])
  } else {
    fill <- rep(as.list(fill), length = 3L)
    if (identical(fill[[2L]], "extend")) 
      stop("fill[[2L]] cannot be 'extend'")
    ix <- if (is.logical(ix)) rep(ix, length = length(object)) else seq_along(object) %in% ix
    wx <- which(ix)
    if (length(wx) == 0L) {
      object[] <- fill[[2L]]
      object
    } else {
      rng <- range(wx)

      if (identical(fill[[1L]], "extend")) fill[[1L]] <- object[rng[1L]]
      if (identical(fill[[3L]], "extend")) fill[[3L]] <- object[rng[2L]]

      fill_lens <- lengths(fill)

      pre <- seq_along(ix) < rng[1L]
      post <- seq_along(ix) > rng[2L]

      if (fill_lens[2L]) object[!ix] <- fill[[2L]]
      if (fill_lens[1L]) object[pre] <- fill[[1L]]
      if (fill_lens[3L]) object[post] <- fill[[3L]]

      omit <- (pre & !fill_lens[1L]) |
              (!pre & !post & !ix & !fill_lens[2L]) |
              (post & !fill_lens[3L])
      object <- object[!omit]
      if (sum(omit)) structure(object, na.action = which(omit)) else object
    }
  }
}

na.fill <- function(object, fill, ...) UseMethod("na.fill")

na.fill.zoo <- function(object, fill, ix, ...) {

	if (length(dim(object)) == 2 && NCOL(object) > 1) {
		ixmiss <- missing(ix)
		L <- lapply(1:NCOL(object), 
				function(i) {
					if (ixmiss) ix <- !is.na(object[,i])
					na.fill(object[,i], fill, ix, ...)
				})
		out <- do.call("merge", c(L, all = FALSE))
		colnames(out) <- colnames(object)
		return(out)
	}

	if (missing(ix)) ix <- !is.na(object)

	if ((is.logical(ix) && any(ix)) || (!is.logical(ix) && length(ix))) {

		n <- length(object)
		# integer indexes for output points which are present
		wix <- if (is.logical(ix)) which(ix) else ix
		# min and max integer index
		wx.min <- head(wix, 1) 
		wx.max <- tail(wix, 1)
		# similar to wrng <- wx.min:wx.max
		wrng <- seq(wx.min, length.out = wx.max - wx.min + 1)

		# recycle to length 3
		fill <- rep(as.list(fill), length.out = 3)
		# we will be coercing fill values to the class of coredata(data).
		# This allows fill=c("extend", NA) to work even though NA is coerced to
		#  a character NA.
		as.cls <- if (is.integer(coredata(object))) {
		  as.integer
		} else if(is.numeric(coredata(object))) {
		  as.numeric
		} else if(is.character(coredata(object))) {
		  as.character
                } else {
		  as.logical
		}
		fill <- lapply(fill, function(x) if (is.character(x) &&
			pmatch(x, "extend", nomatch = 0)) "extend" else as.cls(x))
		# fill points on left
		if (length(fill[[1]]) > 0) 
			if (!is.null(fill[[1]])) object[seq_len(wx.min - 1)] <- 
				if (is.character(fill[[1]]) && !is.na(fill[[1]]) && fill[[1]] == "extend")
						object[[wx.min]] else fill[[1]]
		# fill intermediate points
		# - this is for zoo method, for zooreg method it would be possible to
		#   perform linear interpolation in proportion to time rather than
		#   in proportion to the integer index
		if (length(fill[[2]]) > 0) {
			if (is.character(fill[[2]]) && !is.na(fill[[2]]) && fill[[2]] == "extend") object[wrng] <- 
					# as.list(approx(wix, unlist(object[wix]), xout = wrng)$y)
					approx(wix, unlist(object[wix]), xout = wrng)$y
			else object[intersect(which(!ix), wrng)] <- fill[[2]]
		}
		# fill points on right
		if (length(fill[[3]]) > 0) 
			object[seq(wx.max + 1, length.out = n - wx.max)] <- 
				if (is.character(fill[[3]]) && !is.na(fill[[3]]) && fill[[3]] == "extend")
						object[[wx.max]] else fill[[3]]

		keep <- seq_len(n)
		if (length(fill[[1]]) == 0) keep <- unique(pmax(wx.min, keep))
		if (length(fill[[2]]) == 0) {
			wrng <- seq(wx.min, length.out = wx.max - wx.min + 1)
			keep <- setdiff(keep, intersect(which(!ix), wrng))
		}
		if (length(fill[[3]]) == 0) keep <- unique(pmin(wx.max, keep)) 
		return(object[keep, , drop = is.null(dim(object))])
	} else if(length(fill)) {
	  fill <- unlist(fill[1])[1]
	  object[is.na(object)] <- if(!is.na(fill) && fill == "extend") NA else fill
	  return(object)
	}
}

na.fill.default <- function(object, fill, ix, ...) {
	coredata(na.fill(zoo(object), fill, ix, ...))
}
	
na.fill.ts <- function(object, fill, ix, ...) {
	as.ts(na.fill(as.zoo(object), fill, ix, ...))
}

