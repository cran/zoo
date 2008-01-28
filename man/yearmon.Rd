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
\alias{as.data.frame.yearmon}
\alias{c.yearmon}
\alias{format.yearmon}
\alias{print.yearmon}
\alias{summary.yearmon}
\alias{[.yearmon}
\alias{MATCH.yearmon}
\alias{Ops.yearmon}
\alias{-.yearmon}
\alias{Axis.yearmon}
\alias{axis.yearmon}

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
  \code{"POSIXlt"}, \code{"numeric"}, \code{"character"}.  In the case of
  \code{as.yearmon.character} the \code{format} argument is the same as for
  \code{"Date"}.   One can specify a year and month with no day.  

}
\value{
  Returns its argument converted to class \code{yearmon}.
}

\seealso{\code{\link{yearqtr}}, \code{\link{zoo}}, \code{\link{zooreg}}, \code{\link{ts}}}

\examples{
x <- yearmon(2000 + seq(0, 23)/12)
x

as.yearmon("mar07", "\%b\%y")
as.yearmon("2007-03-01")
as.yearmon("2007-12")

# returned Date is the fraction of the way through
# the period given by frac (= 0 by default)
as.Date(x)
as.Date(x, frac = 1)
as.POSIXct(x)

z <- zoo(rnorm(24), x, frequency = 12)
z
as.ts(z)

## convert data fram to multivariate monthly "ts" series
## 1.read raw data
Lines.raw <- "ID Date Count
123 20 May 1999 1
123 21 May 1999 3
222 1 Feb 2000 2
222 3 Feb 2000 4
"
DF <- read.table(textConnection(Lines.raw), skip = 1,
 col.names = c("ID", "d", "b", "Y", "Count"))
## 2. fix raw date
DF$yearmon <- as.yearmon(paste(DF$b, DF$Y), "\%b \%Y")
## 3. aggregate counts over months, convert to zoo and merge over IDs
ag <- function(DF) aggregate(zoo(DF$Count), DF$yearmon, sum)
z <- do.call("merge.zoo", lapply(split(DF, DF$ID), ag))
## 4. convert to "zooreg" and then to "ts"
frequency(z) <- 12
as.ts(z)
}
\keyword{ts}
