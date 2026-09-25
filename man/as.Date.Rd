\name{as.Date}
\alias{as.Date}
\alias{as.Date.numeric}
\alias{as.Date.ts}
\alias{as.Date.yearmon}
\alias{as.Date.yearqtr}

\title{Coercion to the Date Class}

\description{
Alternative to the \code{\link[base]{as.Date}} coercion function from
base R which was necessary to make sure that the default \code{origin}
in the \code{as.Date} \code{numeric} had a default (January 1, 1970)
whereas the method in base R long had no default.
}

\usage{
as.Date(x, \dots)

\method{as.Date}{numeric}(x, origin, \dots)

\method{as.Date}{ts}(x, offset = 0, \dots)

\method{as.Date}{yearmon}(x, frac = 0, \dots)

\method{as.Date}{yearqtr}(x, frac = 0, \dots)
}
\arguments{
  \item{x}{an object to be converted.}
  \item{origin}{a \code{Date} object, or something which can be coerced by
    \code{as.Date(origin, \dots)} to such an object or \code{\link{missing}}.
    In that case, \code{"1970-01-01"} is used.}
  \item{offset}{numeric offset to be added to the time scale prior to coercion.}
  \item{frac}{numeric between 0 and 1 inclusive that indicates the fraction
    of the way through the period that the result represents.  The default is
    0 which means the beginning of the period.}
  \item{\dots}{further arguments to be passed from or to other methods.}
}

\value{
  An object of class \code{\link[base]{Date}}.
}

\seealso{\code{\link[base]{as.Date}}}

\examples{
## day zero (epoch): Jan 1, 1970
as.Date(0)

## conversion of July 2000 to date (beginning, mid or end of month)
ym <- yearmon(2000.5)
as.Date(ym, frac = 0)
as.Date(ym, frac = 0.5)
as.Date(ym, frac = 1)

## conversion of quarterly time series to dates (beginning of the months)
x <- ts(1:4, start = 2000, freq = 4)
as.Date(x)
}

\keyword{utilities}
\keyword{chron}
