\name{is.regular}
\alias{is.regular}
\alias{is.regular.zoo}
\alias{is.regular.ts}
\alias{is.regular.zooreg}
\alias{is.regular.default}

\title{Check Regularity of a Series}

\description{
\code{is.regular} is a regular function for checking whether a series of ordered observations
has an underlying regularity or is even strictly regular.
}

\usage{
is.regular(x, strict = FALSE)
}

\arguments{
  \item{x}{an object (representing a series of ordered observations).}
  \item{strict}{logical. Should strict regularity be checked? See details.}
}

\details{
A time series can either be irregular (unequally spaced), strictly regular (equally spaced)
or have an underlying regularity, i.e., be created from a regular series by
omitting some observations. Here, the latter property is called \emph{regular}.
Consequently, regularity follows from strict regularity but not vice versa.

\code{is.regular} is a generic function for checking regularity (default) or
strict regularity. Currently, it has methods for \code{"ts"} objects (which are
always strictly regular), \code{"zooreg"} objects (which are at least regular),
\code{"zoo"} objects (which can be either irregular, regular or even strictly regular)
and a default method. The latter coerces \code{x} to \code{"zoo"} before checking
its regularity. 
}

\value{
A logical is returned indicating whether \code{x} is (strictly) regular.
}

\seealso{\code{\link{zooreg}}, \code{\link{zoo}}}

\examples{
## checking of a strictly regular zoo series
z <- zoo(1:10, seq(2000, 2002.25, by = 0.25), frequency = 4)
z
class(z)
frequency(z) ## extraction of frequency attribute
is.regular(z)
is.regular(z, strict = TRUE)
## by omitting observations, the series is not strictly regular
is.regular(z[-3])
is.regular(z[-3], strict = TRUE)

## checking of a plain zoo series without frequency attribute
## which is in fact regular
z <- zoo(1:10, seq(2000, 2002.25, by = 0.25))
z
class(z)
frequency(z) ## data driven computation of frequency
is.regular(z)
is.regular(z, strict = TRUE)
## by omitting observations, the series is not strictly regular
is.regular(z[-3])
is.regular(z[-3], strict = TRUE)

## checking of an irregular zoo series
z <- zoo(1:10, rnorm(10))
z
class(z)
frequency(z) ## attempt of data-driven frequency computation
is.regular(z)
is.regular(z, strict = TRUE)
}
\keyword{ts}
