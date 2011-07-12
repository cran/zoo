as.zoo.tis <- function(x, ...) {
	as.zoo(as.zooreg(x, ...))
}

as.zoo.tis <- function(x, class = "ti", ...) {
	if (class == "ti") {
		as.zoo(as.zooreg(x, class = "ti", ...))
	} else if (class == "numeric") {
		zoo(stripTis(x), time(ti(x), offset = 0))
    } else {
		asFun <- paste("as", class, sep = ".")
		zoo(stripTis(x), 
			do.call(asFun, list(POSIXct(ti(x), offset = 0, tz = "GMT"))), ...)
	}
}

as.zooreg.tis <- function(x, frequency = NULL, class = "ti", ...) {
	if (class == "ti")
		zooreg(stripTis(x), start = start(x), ...)
	else 
		as.zooreg(as.zoo(x, class = class, ...), frequency = frequency)
}

