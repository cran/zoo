\name{as.Date.numeric}
\alias{as.Date.numeric}
\alias{as.Date.ts}

\title{Date Conversion Functions from Numeric, Integer and ts Objects}
\description{
  Functions to convert numeric and related classes to objects of
  class \code{"Date"} representing calendar dates.
}
\usage{
\method{as.Date}{numeric}(x, origin = "1970-01-01", \dots)
\method{as.Date}{ts}(x, offset = 0, \dots)
}
\arguments{
  \item{x}{numeric or an object of class \code{"ts"}, respectively.}
  \item{origin}{A specification of the origin for the dates (\code{x} days since \code{origin}).}
  \item{offset}{A value added to \code{time(x)}.}
  \item{\dots}{Further arguments. Currently not used.}
}
\details{
  The \code{as.Date} method for numeric arguments \code{x} interprets \code{x}
  the number of days since the \code{origin} (default: 1970-01-01). 
  Negative values are allowed.

  The \code{as.Date.ts} inspects \code{time(x)} and \code{frequency(x)}:
  If the frequency is 1 or 4 or 12, \code{time(x)} is regarded to be annual,
  quarterly or monthly data respectively. If the frequency is something else,
  no coercion is done.
}
\value{
  The \code{\link{as.Date}} methods return an object of class \code{"\link{Date}"}.
  In the case of \code{as.Date.ts} applied to a yearly, quarterly or monthly
  series the earliest date, i.e., first of year, quarter or month, is returned.
}
\seealso{
  \link{Date} for details of the date class
}
\examples{

as.Date(0) # uses origin = "1970-01-01"

# all three result in origin and next 9 days
as.Date(0:9)
as.Date(0) + 0:9
x <- ts(rnorm(10), start = 0)
as.Date(unclass(time(x)))

# annual/quarterly/monthly series:
xa <- ts(rnorm(10), start = 2001)
xq <- ts(rnorm(10), start = 2001, freq = 4)
xm <- ts(rnorm(10), start = 2001, freq = 12)
as.Date(time(xa))
as.Date(time(xq))
as.Date(time(xm))

# using offset argument
xa2 <- ts(rnorm(10))
as.Date(time(xa2), offset = 2000)
}
\keyword{utilities}
\keyword{chron}
