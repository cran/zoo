\name{lag.zoo}
\alias{lag.zoo}
\alias{diff.zoo}
\title{Lags and Differences of zoo Objects}
\description{
Methods for computing lags and differences of \code{"zoo"} objects.
}

\usage{
\method{lag}{zoo}(x, k = 1, na.pad = FALSE, \dots)
\method{diff}{zoo}(x, lag = 1, differences = 1, arithmetic = TRUE, na.pad = FALSE, \dots)
}

\arguments{
  \item{x}{a \code{"zoo"} object.}
  \item{k, lag}{For \code{lag} the number of lags (in units of observations).
    Note the sign of \code{k} behaves as in \code{\link[stats]{lag}}.
 	For \code{diff} it is the number of backward lags used (or if negative the
	number of forward lags.}
  \item{differences}{an integer indicating the order of the difference.}
  \item{arithmetic}{logical. Should arithmetic (or geometric) differences be computed?}
  \item{na.pad}{logical. If \code{TRUE} it adds any times that would not otherwise have been in
    the result with a value of \code{NA}.  If \code{FALSE} those times are dropped.}
  \item{\dots}{currently not used.}
}

\details{
These methods for \code{"zoo"} objects behave analogously to the default
methods. The only additional arguments are \code{arithmetic} in \code{diff}
\code{na.pad} in \code{lag.zoo} which can also be specified in \code{diff.zoo}
as part of the dots.
Also, \code{"k"} can be a vector of lags in which case the names of
\code{"k"}, if any, are used in naming the result.


}

\value{
The lagged or differenced \code{"zoo"} object.
}


\note{
  Note the sign of \code{k}: a series lagged by a positive \code{k}
  is shifted \emph{earlier} in time.

  \code{lag.zoo} and \code{lag.zooreg} can give different results.
  For a lag of 1 \code{lag.zoo} moves points to the adjacent time point
  whereas \code{lag.zooreg} moves the time by \code{deltat}.  This
  implies that a point in a \code{zoo} series cannot be lagged to a time 
  point that is not already in the series whereas this is possible for
  a \code{zooreg} series.  
}


\seealso{\code{\link{zoo}}, \code{\link[stats]{lag}}, \code{\link[base]{diff}}}

\examples{
x <- zoo(11:21)

lag(x, k = 1)
lag(x, k = -1)
# this pairs each value of x with the next or future value
merge(x, lag1 = lag(x, k=1))
diff(x^3)
diff(x^3, -1)
diff(x^3, na.pad = TRUE)

}
\keyword{ts}
