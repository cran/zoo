\name{MATCH}
\alias{MATCH}
\alias{MATCH.default}
\alias{MATCH.times}
\alias{MATCH.timeDate}
\alias{MATCH.Date}
\alias{MATCH.POSIXct}
\alias{MATCH.POSIXlt}

\title{Value Matching}

\description{
\code{MATCH} is a generic function for value matching.
}

\usage{
MATCH(x, table, nomatch = NA, \dots)
\method{MATCH}{times}(x, table, nomatch = NA, units = "sec", eps = 1e-10, \dots)
}
\arguments{
  \item{x}{an object.}
  \item{table}{the values to be matched against.}
  \item{nomatch}{the value to be returned in the case when no match is
    found.  Note that it is coerced to \code{integer}.}
  \item{units}{See \code{\link[chron]{trunc.times}}.}
  \item{eps}{See \code{\link[chron]{trunc.times}}.}
  \item{\dots}{further arguments to be passed to methods.}
}

\details{
\code{MATCH} is a new generic function which aims at providing
the functionality of the non-generic base function \code{\link[base]{match}}
for arbitrary objects. Currently, there is a default method which
simply calls \code{\link[base]{match}} and various methods for time/date
objects.

The \code{MATCH} method for \code{Date} objects coerces the \code{table}
to \code{Date} as well (if necessary) and then uses
\code{match(unclass(x), unclass(table), ...}. Similarly, the \code{MATCH}
methods for \code{POSIXct}, \code{POSIXlt}, and \code{timeDate} coerce
both \code{x} and \code{table} to \code{POSIXct} and then match the unclassed
objects.

\code{MATCH.times} is used for \code{chron} objects. \code{x} will
match any time in \code{table} less than \code{units} away.
}


\seealso{\code{\link[base]{match}}}

\examples{
MATCH(1:5, 2:3)
}
\keyword{manip}
