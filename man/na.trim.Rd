\name{na.trim}
\alias{na.trim}
\alias{na.trim.default}
\alias{na.trim.ts}
\title{Trim Leading/Trailing Missing Observations}
\description{
Generic function for removing leading and trailing \code{NA}s.
}
\usage{
na.trim(object, \dots)
\method{na.trim}{default}(object, sides = c("both", "left", "right"), 
	is.na = c("any", "all"), \dots)
}
\arguments{
  \item{object}{an object.}
  \item{sides}{character specifying whether \code{NA}s are to be removed from
both sides, just from the left side or just from the right side.}
  \item{is.na}{If "any" then a row will be regarded as \code{NA} if it has
	any \code{NA}s.  If "all" then a row will be regarded as \code{NA}
	only if all elements in the row are \code{NA}.  For one dimensional
	zoo objects this argument has no effect.}
  \item{\dots}{further arguments passed to methods.}
}

\value{
An object in which leading and/or trailing
\code{NA}s have been removed.
}

\seealso{\code{\link{na.approx}}, \code{\link{na.contiguous}}, \code{\link{na.locf}}, \code{\link{na.omit}}, \code{\link{na.spline}}, \code{\link[stinepack]{stinterp}}, \code{\link{zoo}}}
\examples{
# examples of na.trim
x <- zoo(c(1, 4, 6), c(2, 4, 6))
xx <- zoo(matrix(c(1, 4, 6, NA, 5, 7), 3), c(2, 4, 6))
na.trim(x)
na.trim(xx)

# using na.trim for alignment
# cal defines the legal dates
# all dates within the date range of x should be present
cal <- zoo(,c(1, 2, 3, 6, 7))
x <- zoo(c(12, 16), c(2, 6))
na.trim(merge(x, cal))

}
\keyword{ts}
