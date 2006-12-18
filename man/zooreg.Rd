\name{zooreg}
\alias{zooreg}
\alias{frequency.zooreg}
\alias{frequency.zoo}
\alias{deltat.zooreg}
\alias{deltat.zoo}
\alias{cycle.zooreg}
\alias{cycle.zoo}
\alias{as.zooreg}
\alias{as.zooreg.default}
\alias{as.zooreg.ts}
\alias{as.ts.zooreg}
\alias{as.zoo.zooreg}
\alias{as.zooreg.zoo}
\alias{index<-.zooreg}
\alias{time<-.zooreg}
\alias{lag.zooreg}

\title{Regular zoo Series}

\description{
\code{zooreg} is the creator for the S3 class \code{"zooreg"}
for regular \code{"zoo"} series. It inherits from \code{"zoo"}
and is the analogue to \code{\link{ts}}.
}

\usage{
zooreg(data, start = 1, end = numeric(), frequency = 1, 
  deltat = 1, ts.eps = getOption("ts.eps"), order.by = NULL)
}

\arguments{
  \item{data}{a numeric vector, matrix or a factor.}
  \item{start}{the time of the first observation.  Either a single number or
    a vector of two integers, which specify a natural time unit
    and a (1-based) number of samples into the time unit.}
  \item{end}{the time of the last observation, specified in the same way
    as \code{start}.}
  \item{frequency}{the number of observations per unit of time.}
  \item{deltat}{the fraction of the sampling period between successive
    observations; e.g., 1/12 for monthly data.  Only one of
    \code{frequency} or \code{deltat} should be provided.}
  \item{ts.eps}{time series comparison tolerance.  Frequencies are considered
    equal if their absolute difference is less than \code{ts.eps}.}
  \item{order.by}{a vector by which the observations in \code{x}
    are ordered. If this is specified the arguments \code{start} and
    \code{end} are ignored and \code{zoo(data, order.by, frequency)} is
    called. See \code{\link{zoo}} for more information.}
}

\details{
Strictly regular series are those whose time points are equally spaced.
Weakly regular series are strictly regular time series in which some
of the points may have been removed but still have the original
underlying frequency associated with them.
\code{"zooreg"} is a subclass of \code{"zoo"} that is used to represent both weakly
and strictly regular series. Internally, it is the same as \code{"zoo"} except 
it also has a \code{"frequency"} attribute. Its index class is more restricted 
than \code{"zoo"}. The index: 1. must be numeric or a class which can be coerced
via \code{as.numeric} (such as \code{\link{yearmon}}, \code{\link{yearqtr}},
\code{\link{Date}}, \code{\link{POSIXct}} etc.). 2. when converted to numeric
must be expressable as multiples of 1/frequency. 3.
group generic functions \code{\link{Ops}} should be defined, i.e.,
adding/substracting a numeric to/from the index class should produce the correct
value of the index class again.

\code{zooreg} is the \code{zoo} analogue to \code{\link{ts}}. The arguments
are almost identical, only in the case where \code{order.by} is specified,
\code{\link{zoo}} is called with \code{zoo(data, order.by, frequency)}. It
creates a regular series of class \code{"zooreg"} which inherits from \code{"zoo"}.
It is essentially a \code{"zoo"} series with an additional \code{"frequency"}
attribute. In the creation of \code{"zooreg"} objects (via \code{\link{zoo}},
\code{\link{zooreg}}, or coercion functions) it is always check whether the
index specified complies with the frequency specified.

The class \code{"zooreg"} offers two advantages over code \code{"ts"}: 1. The
index does not have to be plain numeric (although that is the default), it just
must be coercable to numeric, thus printing and plotting can be customized.
2. This class can not only represent strictly regular series, but also series
with an underlying regularity, i.e., where some observations from a regular grid
are omitted.

Hence, \code{"zooreg"} is a bridge between \code{"ts"} and \code{"zoo"} and
can be employed to coerce back and forth between the two classes. The coercion
function \code{as.zoo.ts} returns therefore an object of class \code{"zooreg"}
inheriting from \code{"zoo"}. Coercion between \code{"zooreg"} and \code{"zoo"}
is also available and drops or tries to add a frequency respectively.

For checking whether a series is strictly regular or does have an underlying
regularity the generic function \code{\link{is.regular}} can be used.

Methods to standard generics for regular series such as \code{\link{frequency}},
\code{\link{deltat}} and \code{\link{cycle}} are available for both \code{"zooreg"}
and \code{"zoo"} objects. In the latter case, it is checked first (in a data-driven way)
whether the series is in fact regular or not.
}

\value{
An object of class \code{"zooreg"} which inherits from \code{"zoo"}.
It is essentially a \code{"zoo"} series with a \code{"frequency"}
attribute.
}

\seealso{\code{\link{zoo}}, \code{\link{is.regular}}}

\examples{
## equivalent specifications of a quarterly series
## starting in the second quarter of 1959.
zooreg(1:10, frequency = 4, start = c(1959, 2))
as.zoo(ts(1:10, frequency = 4, start = c(1959, 2)))
zoo(1:10, seq(1959.25, 1961.5, by = 0.25), frequency = 4)

## use yearqtr class for indexing the same series
z <- zoo(1:10, yearqtr(seq(1959.25, 1961.5, by = 0.25)), frequency = 4)
z
z[-(3:4)]

## create a regular series with a "Date" index
zooreg(1:5, start = Sys.Date())
## or with "yearmon" index
zooreg(1:5, end = yearmon(2000))

## lag and diff (as it is defined in terms of lag)
## act different on zoo and zooreg objects!
x <- c(1, 4, 5, 6)
zz <- zoo(x, x)
zr <- as.zooreg(zz)
lag(zz)
lag(zr)
diff(zz)
diff(zr)

## standard methods available for regular series
frequency(z)
deltat(z)
cycle(z)
cycle(z[-(3:4)])
}
\keyword{ts}
