\name{yearmon}
\alias{yearmon}
\alias{as.yearmon}
\alias{as.yearmon.default} 
\alias{as.yearmon.numeric}
\alias{as.yearmon.integer}
\alias{as.yearmon.dates}
\alias{as.yearmon.Date}
\alias{as.yearmon.timeDate}
\alias{as.yearmon.jul}
\alias{as.yearmon.POSIXt}
\alias{as.yearmon.character}
\alias{as.yearmon.date}
\alias{as.yearmon.factor}
\alias{as.Date}
\alias{as.Date.numeric}
\alias{as.Date.ts}
\alias{as.Date.yearmon}
\alias{as.POSIXct.yearmon}
\alias{as.POSIXlt.yearmon}
\alias{as.list.yearmon}
\alias{as.numeric.yearmon}
\alias{as.character.yearmon}
\alias{as.data.frame.yearmon}
\alias{c.yearmon}
\alias{cycle.yearmon}
\alias{format.yearmon}
\alias{is.yearmon}
\alias{mean.yearmon}
\alias{print.yearmon}
\alias{range.yearmon}
\alias{summary.yearmon}
\alias{unique.yearmon}
\alias{[.yearmon}
\alias{[[.yearmon}
\alias{MATCH.yearmon}
\alias{Ops.yearmon}
\alias{Sys.yearmon}
\alias{-.yearmon}
\alias{xtfrm.yearmon}

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
  The \code{"yearmon"} class is used to represent monthly data. Internally it holds
  the data as year plus 0 for January, 1/12 for February, 2/12 for March
  and so on in order that its internal representation is the same as 
  \code{ts} class with \code{frequency = 12}. If \code{x} is not in this
  format it is rounded via \code{floor(12*x + .0001)/12}.
  
  There are coercion methods available for various classes including:
  default coercion to \code{"yearmon"} (which coerces to \code{"numeric"} first)
  and coercions to and from \code{"yearmon"} to \code{"Date"} (see below), 
  \code{"POSIXct"},
  \code{"POSIXlt"}, \code{"numeric"}, \code{"character"} and \code{"jul"}.  
  The last one is from the \code{"tis"} package available on CRAN.
  In the case of \code{as.yearmon.POSIXt} the conversion is with respect to
  GMT.  (Use \code{as.yearmon(format(...))} for other time zones.)
  In the case of
  \code{as.yearmon.character} the \code{format} argument uses the same
  percent code as 
  \code{"Date"}.   These are described in \code{\link{strptime}}.  Unlike
  \code{"Date"} one can specify a year and month with no day.  
  Default formats of \code{"\%Y-\%m"}, \code{"\%Y-\%m-\%d"} and \code{"\%b \%Y"}.

  There is an \code{is.numeric} method which returns \code{FALSE}.

  \code{as.Date.yearmon} and \code{as.yearmon.yearqtr} 
  each has an optional
  second argument of \code{"frac"} which is a number between 0 and 1 inclusive
  that indicates the fraction of the way through the period that the result
  represents.  The default is 0 which means the beginning of the period.

  There is also a \code{date} method for \code{as.yearmon} usable with objects
  created with package \code{date}.

  \code{Sys.yearmon()} returns the current year/month and methods for
  \code{min}, \code{max} and \code{range} are defined (by defining 
  a method for \code{Summary}).

  A \code{yearmon} \code{mean} method is also defined.
}

\value{
  Returns its argument converted to class \code{yearmon}.
}

\seealso{\code{\link{yearqtr}}, \code{\link{zoo}}, \code{\link{zooreg}}, \code{\link{ts}}}

\examples{
x <- as.yearmon(2000 + seq(0, 23)/12)
x

as.yearmon("mar07", "\%b\%y")
as.yearmon("2007-03-01")
as.yearmon("2007-12")

# returned Date is the fraction of the way through
# the period given by frac (= 0 by default)
as.Date(x)
as.Date(x, frac = 1)
as.POSIXct(x)

# given a Date, x, return the Date of the next Friday
nextfri <- function(x) 7 * ceiling(as.numeric(x - 1)/7) + as.Date(1)

# given a Date, d, return the same Date in the following month
# Note that as.Date.yearmon gives first Date of the month.
d <- as.Date("2005-1-1") + seq(0,90,30)
next.month <- function(d) as.Date(as.yearmon(d) + 1/12) + 
	as.numeric(d - as.Date(as.yearmon(d)))
next.month(d)

# 3rd Friday in last month of the quarter of Date x
as.Date(as.yearmon(as.yearqtr(x)) + 2/12) + 14

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

xx <- zoo(seq_along(x), x)

## aggregating over year 
as.year <- function(x) as.numeric(floor(as.yearmon(x)))
aggregate(xx, as.year, mean)

}
\keyword{ts}
