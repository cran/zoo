\name{lag.zoo}
\alias{lag.zoo}
\alias{diff.zoo}
\title{Lags and Differences of zoo Objects}
\description{
Methods for computing lags and differences of \code{"zoo"} objects.
}

\usage{
\method{lag}{zoo}(x, k = 1, \dots)
\method{diff}{zoo}(x, lag = 1, differences = 1, arithmetic = TRUE, \dots)
}

\arguments{
  \item{x}{a \code{"zoo"} object.}
  \item{k, lag}{the number of lags (in units of observations).
    Note the sign of \code{k} behaves as in \code{\link[stats]{lag}}.}
  \item{differences}{an integer indicating the order of the difference.}
  \item{arithmetic}{logical. Should arithmetic (or geometric) differences be computed?}
  \item{\dots}{currently not used.}
}

\details{
These methods for \code{"zoo"} objects behave analogously to the default
methods. The only additional argument is \code{arithmetic} in \code{diff}.
}

\value{
The lagged or differenced \code{"zoo"} object.
}


\note{
  Note the sign of \code{k}: a series lagged by a positive \code{k}
  is shifted \emph{earlier} in time.
}


\seealso{\code{\link{zoo}}, \code{\link[stats]{lag}}, \code{\link[stats]{diff}}}

\examples{
x <- zoo(11:21)

lag(x, k = 1)
lag(x, k = -1)
# this pairs each value of x with the next or future value
merge(x, lag1 = lag(x, k=1))
diff(x^2)
}
\keyword{ts}
