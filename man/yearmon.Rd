\name{yearmon}
\alias{yearmon}
\alias{as.yearmon}
\alias{as.yearmon.default} 
\alias{as.yearmon.numeric}
\alias{as.yearmon.integer}
\alias{as.yearmon.dates}
\alias{as.yearmon.Date}
\alias{as.yearmon.POSIXt}
\alias{as.yearmon.character}
\alias{as.Date.yearmon}
\alias{as.POSIXct.yearmon}
\alias{as.POSIXlt.yearmon}
\alias{as.numeric.yearmon}
\alias{as.character.yearmon}
\alias{c.yearmon}
\alias{format.yearmon}
\alias{print.yearmon}
\alias{[.yearmon}
\alias{axis.yearmon}
\alias{MATCH.yearmon}
\alias{Ops.yearmon}

\title{An Index Class for Monthly Data}
\description{
  \code{"yearmon"} is a class for representing monthly data.
}
\usage{
yearmon(x)
}
\arguments{
  \item{x}{numeric (interpreted as being \dQuote{in years}).}
}
\details{
  The yearmon class is used to represent monthly data. Internally it holds
  the data as year plus 0 for January, 1/12 for February, 2/12 for March
  and so on in order that its internal representation is the same as 
  \code{ts} class with \code{frequency = 12}. If \code{x} is not in this
  format it is rounded via \code{floor(12*x + .0001)/12}.
  
  There are coercion methods available for various classes including:
  default coercion to \code{"yearmon"} (which coerces to \code{"numeric"} first)
  and coercion from \code{"yearmon"} to \code{"Date"} (see below), \code{"POSIXct"},
  \code{"POSIXlt"}, \code{"numeric"}, \code{"character"}.
}
\value{
  Returns its argument converted to class \code{yearmon}.
}

\seealso{\code{\link{yearqtr}}, \code{\link{zoo}}, \code{\link{zooreg}}, \code{\link{ts}}}

\examples{
x <- yearmon(2000 + seq(0, 23)/12)
x

# returned Date is the fraction of the way through
# the period given by frac (= 0 by default)
as.Date(x)
as.Date(x, frac = 1)
as.POSIXct(x)

z <- zoo(rnorm(24), x, frequency = 12)
z
as.ts(z)
}
\keyword{ts}
