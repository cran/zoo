\name{na.locf}
\alias{na.locf}
\alias{na.locf.data.frame}
\alias{na.locf.list}
\alias{na.locf.default}
\title{Last Observation Carried Forward}
\description{
Generic function for replacing each \code{NA} with the most recent
non-\code{NA} prior to it.
}
\usage{
na.locf(object, na.rm = TRUE, \dots)
\method{na.locf}{default}(object, na.rm = TRUE, fromLast, rev,
        maxgap = Inf, \dots)
}
\arguments{
  \item{object}{an object.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{fromLast}{logical. Causes observations to be carried backward rather
    than forward.  Default is \code{FALSE}.
    This corresponds to NOCB (next observation carried backward).}
  \item{rev}{Use \code{fromLast} instead.  This argument will
    be eliminated in the future in favor of \code{fromLast}.}
  \item{maxgap}{maximum number of consecutive \code{NA}s to
    fill. Any longer gaps will be left unchanged.}
  \item{\dots}{further arguments passed to methods.}
}

\value{
An object in which each \code{NA} in the input object is replaced
by the most recent non-\code{NA} prior to it.  If there are no earlier non-\code{NA}s then 
the \code{NA} is omitted (if \code{na.rm = TRUE}) or it is not replaced (if \code{na.rm = FALSE}).  

Note that if a multi-column zoo object has a column entirely composed of
\code{NA} then with \code{na.rm = TRUE}, the default,
the above implies that the resulting object will have
zero rows. Use \code{na.rm = FALSE} to preserve the \code{NA} values instead.
}

\seealso{\code{\link{zoo}}}

\examples{
az <- zoo(1:6)

bz <- zoo(c(2,NA,1,4,5,2))
na.locf(bz)
na.locf(bz, fromLast = TRUE)

cz <- zoo(c(NA,9,3,2,3,2))
na.locf(cz)

# generate and fill in missing dates
# by merging with a zero width series having those dates
# and then applying na.locf
z <- zoo(c(0.007306621, 0.007659046, 0.007681013,
	0.007817548, 0.007847579, 0.007867313),
	as.Date(c("1993-01-01", "1993-01-09", "1993-01-16",
	"1993-01-23", "1993-01-30", "1993-02-06")))
dd <- seq(start(z), end(z), "day")
na.locf(merge(z, zoo(, dd)))

## get 5th of every month or most recent date prior to 5th if 5th missing.
## Result has index of the date actually used.

z <- zoo(c(1311.56, 1309.04, 1295.5, 1296.6, 1286.57, 1288.12, 
1289.12, 1289.12, 1285.33, 1307.65, 1309.93, 1311.46, 1311.28, 
1308.11, 1301.74, 1305.41, 1309.72, 1310.61, 1305.19, 1313.21, 
1307.85, 1312.25, 1325.76), as.Date(c(13242, 13244, 
13245, 13248, 13249, 13250, 13251, 13252, 13255, 13256, 13257, 
13258, 13259, 13262, 13263, 13264, 13265, 13266, 13269, 13270, 
13271, 13272, 13274)))

# z.na is same as z but with missing days added (with NAs)
# It is formed by merging z with a zero with series having all the dates.

rng <- range(time(z))
z.na <- merge(z, zoo(, seq(rng[1], rng[2], by = "day")))

# use na.locf to bring values forward picking off 5th of month
na.locf(z.na)[as.POSIXlt(time(z.na))$mday == 5]

## this is the same as last one except instead of always using the
## 5th of month in the result we show the date actually used

# idx has NAs wherever z.na does but has 1, 2, 3, ... instead of
# z.na's data values (so idx can be used for indexing)

idx <- na.locf(seq_along(z.na) + (0 * z.na))

# pick off those elements of z.na that correspond to 5th

z.na[idx[as.POSIXlt(time(z.na))$mday == 5]]

## only fill single-day gaps

merge(z.na, filled1 = na.locf(z.na, maxgap = 1))
}
\keyword{ts}
