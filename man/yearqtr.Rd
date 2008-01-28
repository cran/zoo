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
\alias{as.data.frame.yearqtr}
\alias{c.yearqtr}
\alias{format.yearqtr}
\alias{print.yearqtr}
\alias{summary.yearqtr}
\alias{[.yearqtr}
\alias{MATCH.yearqtr}
\alias{Ops.yearqtr}
\alias{-.yearqtr}
\alias{Axis.yearqtr}
\alias{axis.yearqtr}

\title{An Index Class for Quarterly Data}
\description{
  \code{"yearqtr"} is a class for representing quarterly data.
}

\usage{
yearqtr(x)
as.yearqtr(x, \dots)
\method{format}{yearqtr}(x, format = "\%Y Q\%q", \dots)
}

\arguments{
  \item{x}{for \code{yearqtr} a numeric (interpreted as being 
  \dQuote{in years}).  For \code{as.yearqtr} another date class
  object.  For the \code{"yearqtr"} method of \code{format} an 
  object of class \code{"yearqtr"} or if called as \code{format.yearqtr}
  then an object with an \code{as.yearqtr} method that can be coerced 
  to \code{"yearqtr"}.}
  \item{format}{character string specifying format.
  \code{"\%C"}, \code{"\%Y"}, \code{"\%y"} and \code{"\%q"}, if present,
  are replaced with the century, year, last two digits of the year, 
  and quarter (i.e. a number between 1 and 4), respectively.}
  \item{\dots}{other arguments.  Currently not used.}
}

\details{
  The yearqtr class is used to represent quarterly data. Internally it holds
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
  \code{yearqtr} and \code{as.yearqtr} return the first argument converted to 
  class \code{yearqtr}.
  The \code{format} method returns a character string representation of
  its argument first argument.
}

\seealso{\code{\link{yearmon}}, \code{\link{zoo}}, \code{\link{zooreg}}, \code{\link{ts}},
  code{\link{strptime}}.}

\examples{
x <- yearqtr(2000 + seq(0, 7)/4)
x

format(x, "\%Y Quarter \%q")

# returned Date is the fraction of the way through
# the period given by frac (= 0 by default)
dd <- as.Date(x)
format.yearqtr(dd)
as.Date(x, frac = 1)
as.POSIXct(x)

zz <- zoo(rnorm(8), x, frequency = 4)
zz
as.ts(zz)

}
\keyword{ts}
