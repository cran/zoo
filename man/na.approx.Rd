\name{na.approx}
\alias{na.approx}
\alias{na.approx.zoo}
\alias{na.approx.zooreg}
\alias{na.approx.ts}
\alias{na.approx.default}
\alias{na.spline}
\alias{na.spline.zoo}
\alias{na.spline.zooreg}
\alias{na.spline.ts}
\alias{na.spline.default}
\title{Replace NA by Interpolation}
\description{
Generic functions for replacing each \code{NA} with interpolated
values.
}
\usage{
na.approx(object, \dots) 
\method{na.approx}{zoo}(object, x = index(object), xout, \dots, na.rm = TRUE, along)
\method{na.approx}{zooreg}(object, \dots) 
\method{na.approx}{ts}(object, \dots)
\method{na.approx}{default}(object, x = index(object), xout, \dots, na.rm = TRUE, maxgap = Inf, along) 

na.spline(object, \dots) 
\method{na.spline}{zoo}(object, x = index(object), xout, \dots, na.rm = TRUE, along)
\method{na.spline}{zooreg}(object, \dots) 
\method{na.spline}{ts}(object, \dots)
\method{na.spline}{default}(object, x = index(object), xout, \dots, na.rm = TRUE, maxgap = Inf, along) 

}
\arguments{
  \item{object}{object in which \code{NA}s are to be replaced}
  \item{x, xout}{Variables to be used for interpolation as in \code{\link{approx}}.}
  \item{na.rm}{logical. If the result of the (spline) interpolation
    still results in \code{NA}s, should these be removed?}
  \item{maxgap}{maximum number of consecutive \code{NA}s to
    fill. Any longer gaps will be left unchanged. Note that all methods listed
    above can accept \code{maxgap} as it is ultimately passed to the 
    \code{default} method.}
  \item{along}{deprecated.}
  \item{\dots}{further arguments passed to methods. The \code{n} argument of \code{\link{approx}} is currently not supported.}
}

\details{
  Missing values (\code{NA}s) are replaced by linear interpolation via
  \code{\link{approx}} or cubic spline interpolation via \code{\link{spline}},
  respectively.  

  It can also be used for series disaggregation by specifying \code{xout}.

  By default the index associated with \code{object} is used
  for interpolation. Note, that if this calls \code{index.default}
  this gives an equidistant spacing \code{1:NROW(object)}. If \code{object}
  is a matrix or data.frame, the interpolation is done separately for
  each column.

  If \code{obj} is a plain vector then \code{na.approx(obj, x, y, xout, ...)}
  returns \code{approx(x = x[!na], y = coredata(obj)[!na], xout = xout, ...)}
  (where \code{na} indicates observations with \code{NA}) such that \code{xout}
  defaults to \code{x}.
  
  If \code{obj} is a \code{zoo}, \code{zooreg} or \code{ts} object its
  \code{coredata} value is processed as described and its time index is \code{xout} if
  specified and \code{index(obj)} otherwise. If \code{obj} is two dimensional
  then the above is applied to each column separately. For examples, see below.

  If \code{obj} has more than one column, the above strategy is applied to
  each column.
}

\value{
  An object of similar structure as \code{object} with (internal) \code{NA}s
  replaced by interpolation. Leading or trailing \code{NA}s are omitted if
  \code{na.rm = TRUE} or not replaced if \code{na.rm = FALSE}. 
}

\seealso{\code{\link{zoo}}, \code{\link{approx}}, \code{\link{na.contiguous}},
  \code{\link{na.locf}}, \code{\link{na.omit}}, \code{\link{na.trim}}, \code{\link{spline}},
  \code{\link[stinepack]{stinterp}}
} 

\examples{

z <- zoo(c(2, NA, 1, 4, 5, 2), c(1, 3, 4, 6, 7, 8))

## use underlying time scale for interpolation
na.approx(z) 
## use equidistant spacing
na.approx(z, 1:6)

# with and without na.rm = FALSE
zz <- c(NA, 9, 3, NA, 3, 2)
na.approx(zz, na.rm = FALSE)
na.approx(zz)

d0 <- as.Date("2000-01-01")
z <- zoo(c(11, NA, 13, NA, 15, NA), d0 + 1:6)

# NA fill, drop or keep leading/trailing NAs
na.approx(z)
na.approx(z, na.rm = FALSE)

# extrapolate to point outside of range of time points
# (a) drop NA, (b) keep NA, (c) extrapolate using rule = 2 from approx()
na.approx(z, xout = d0 + 7)
na.approx(z, xout = d0 + 7, na.rm = FALSE)
na.approx(z, xout = d0 + 7, rule = 2)

# use splines - extrapolation handled differently
z <- zoo(c(11, NA, 13, NA, 15, NA), d0 + 1:6)
na.spline(z)
na.spline(z, na.rm = FALSE)
na.spline(z, xout = d0 + 1:6)
na.spline(z, xout = d0 + 2:5)
na.spline(z, xout = d0 + 7)
na.spline(z, xout = d0 + 7, na.rm = FALSE)

## using na.approx for disaggregation
zy <- zoo(1:3,  2000:2001)

# yearly to monthly series
zmo <- na.approx(zy, xout = as.yearmon(2000+0:13/12))
zmo

# monthly to daily series
sq <- seq(as.Date(start(zmo)), as.Date(end(zmo), frac = 1), by = "day")
zd <- na.approx(zmo, x = as.Date, xout = sq)
head(zd)

# weekly to daily series
zww <- zoo(1:3, as.Date("2001-01-01") + seq(0, length = 3, by = 7))
zww
zdd <- na.approx(zww, xout = seq(start(zww), end(zww), by = "day"))
zdd

# The lines do not show up because of the NAs
plot(cbind(z, z), type = "b", screen = 1)
# use na.approx to force lines to appear
plot(cbind(z, na.approx(z)), type = "b", screen = 1)

# Workaround where less than 2 NAs can appear in a column
za <- zoo(cbind(1:5, NA, c(1:3, NA, 5), NA)); za

ix <- colSums(!is.na(za)) > 0
za[, ix] <- na.approx(za[, ix]); za

# using na.approx to create regularly spaced series
# z has points at 10, 20 and 40 minutes while output also has a point at 30
if(require("chron")) {
  tt <- as.chron("2000-01-01 10:00:00") + c(1, 2, 4) * as.numeric(times("00:10:00"))
  z <- zoo(1:3, tt)
  tseq <- seq(start(z), end(z), by = times("00:10:00"))
  na.approx(z, xout = tseq)
}
}

\keyword{ts}
