\name{as.Date.numeric}
\alias{as.Date.numeric}
\alias{as.Date.integer}
\alias{as.Date.ts}

\title{Date Conversion Functions from Numeric, Integer and ts Objects}
\description{
  Functions to convert numeric and related classes to objects of
  class \code{"Date"} representing calendar dates.
}
\usage{
\method{as.Date}{numeric}(x, \dots)

\method{as.Date}{integer}(x, \dots)
\method{as.Date}{ts}(x, offset = 0, \dots)
}
\arguments{
  \item{x}{An object of class \code{"numeric"}, \code{"integer"}
    or \code{"ts"} respectively.}
  \item{offset}{A value added to \code{time(x)}.}
  \item{\dots}{Further arguments. Currently not used.}
}
\details{
  The \code{as.Date.integer} method interprets the argument \code{x} as
  the number of days since the origin 1970-01-01 so that the origin
  corresponds to 0.  Negative values are allowed.

  The \code{as.Date.numeric} method like the \code{as.Date.integer}
  method applied to \code{floor(x)}.

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

as.Date(0) # origin, i.e. 1970-01-01

# all three result in origin and next 9 days
as.Date(0:9)
as.Date(0) + 0:9
myts <- ts(rnorm(10), start = 0)
as.Date(unclass(time(myts)))

# both result in ten firsts of year starting 2001-01-01
as.Date(ts(rnorm(10)), offset = 2000)
as.Date(ts(rnorm(10), start = 2001))  

# both result in ten firsts of month starting with 1990-01-01
as.Date(ts(rnorm(10), start = c(90,1), freq = 12), offset = 1900)  
as.Date(ts(rnorm(10), start = c(1990,1), freq = 12))  

}
\keyword{utilities}
\keyword{chron}
