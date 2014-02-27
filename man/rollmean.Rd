\name{rollmean}

\alias{rollmean}
\alias{rollmax}
\alias{rollmedian}
\alias{rollsum}

\alias{rollmeanr}
\alias{rollmaxr}
\alias{rollmedianr}
\alias{rollsumr}

\alias{rollmean.zoo}
\alias{rollmax.zoo}
\alias{rollmedian.zoo}
\alias{rollsum.zoo}

\alias{rollmean.ts}
\alias{rollmax.ts}
\alias{rollmedian.ts}
\alias{rollsum.ts}

\alias{rollmean.default}
\alias{rollmax.default}
\alias{rollmedian.default}
\alias{rollsum.default}

\title{Rolling Means/Maximums/Medians/Sums}

\description{
Generic functions for computing rolling means, maximums, medians, and sums of ordered observations.
}

\usage{
rollmean(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
  align = c("center", "left", "right"), ...)

rollmax(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
  align = c("center", "left", "right"), ...)

rollmedian(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
  align = c("center", "left", "right"), ...)

rollsum(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
  align = c("center", "left", "right"), ...)

rollmeanr(..., align = "right")
rollmaxr(..., align = "right")
rollmedianr(..., align = "right")
rollsumr(..., align = "right")
}

\arguments{
  \item{x}{an object (representing a series of observations).}
  \item{k}{integer width of the rolling window. Must be odd for \code{rollmedian}.}  
  \item{fill}{a three-component vector or list (recycled otherwise) providing
    filling values at the left/within/to the right of the data range.
    See the \code{fill} argument of \code{\link{na.fill}} for details.}
  \item{na.pad}{deprecated. Use \code{fill = NA} instead of \code{na.pad = TRUE}.}
  \item{align}{character specifying whether the index of the result
    should be left- or right-aligned or centered (default) compared
    to the rolling window of observations.}
  \item{\dots}{Further arguments passed to methods.}
}

\details{
These functions compute rolling means, maximums, medians, and sums respectively
and are thus similar to \code{\link{rollapply}} but are
optimized for speed.

Currently, there are methods for \code{"zoo"} and \code{"ts"} series and
default methods. The default method of \code{rollmedian}
is an interface to \code{\link{runmed}}.
The default method of \code{rollmean} does not handle inputs that contain
\code{NA}s. In such cases, use \code{\link{rollapply}} instead.
}

\value{
An object of the same class as \code{x} with the rolling mean/max/median/sum.
}

\seealso{\code{\link{rollapply}}, \code{\link{zoo}}, \code{\link{na.fill}}}

\examples{
x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x <- zoo(rnorm(12), x.Date)

rollmean(x, 3)
rollmax(x, 3)
rollmedian(x, 3)
rollsum(x, 3)

xm <- zoo(matrix(1:12, 4, 3), x.Date[1:4])
rollmean(xm, 3)
rollmax(xm, 3)
rollmedian(xm, 3)
rollsum(xm, 3)

rollapply(xm, 3, mean) # uses rollmean
rollapply(xm, 3, function(x) mean(x)) # does not use rollmean
}
\keyword{ts}
