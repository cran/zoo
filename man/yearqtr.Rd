\name{yearqtr}
\alias{yearqtr}
\alias{as.yearqtr}
\alias{as.yearqtr.default} 
\alias{as.yearqtr.numeric}
\alias{as.yearqtr.integer}
\alias{as.yearqtr.dates}
\alias{as.yearqtr.Date}
\alias{as.yearqtr.POSIXt}
\alias{as.yearqtr.character}
\alias{as.Date.yearqtr}
\alias{as.POSIXct.yearqtr}
\alias{as.POSIXlt.yearqtr}
\alias{as.numeric.yearqtr}
\alias{as.character.yearqtr}
\alias{c.yearqtr}
\alias{format.yearqtr}
\alias{print.yearqtr}
\alias{[.yearqtr}
\alias{axis.yearqtr}
\alias{MATCH.yearqtr}
\alias{Ops.yearqtr}
\alias{-.yearqtr}
\alias{axis.yearqtr}
\alias{Axis.yearqtr}

\title{An Index Class for Quaterly Data}
\description{
  \code{"yearqtr"} is a class for representing quaterly data.
}
\usage{
yearqtr(x)
}
\arguments{
  \item{x}{numeric (interpreted as being \dQuote{in years}).}
}
\details{
  The yearqtr class is used to represent quaterly data. Internally it holds
  the data as year plus 0 for Quarter 1, 1/4 for Quarter 2
  and so on in order that its internal representation is the same as 
  \code{ts} class with \code{frequency = 4}. If \code{x} is not in this
  format it is rounded via \code{floor(4*x + .0001)/4}.
  
  There are coercion methods available for various classes including:
  default coercion to \code{"yearqtr"} (which coerces to \code{"numeric"} first)
  and coercion from \code{"yearqtr"} to \code{"Date"} (see below), \code{"POSIXct"},
  \code{"POSIXlt"}, \code{"numeric"}, \code{"character"}.
}
\value{
  Returns its argument converted to class \code{yearqtr}.
}

\seealso{\code{\link{yearmon}}, \code{\link{zoo}}, \code{\link{zooreg}}, \code{\link{ts}}}

\examples{
x <- yearqtr(2000 + seq(0, 7)/4)
x

# returned Date is the fraction of the way through
# the period given by frac (= 0 by default)
as.Date(x)
as.Date(x, frac = 1)
as.POSIXct(x)

z <- zoo(rnorm(8), x, frequency = 4)
z
as.ts(z)
}
\keyword{ts}
