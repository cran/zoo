\name{rollmean}
\alias{rollmean}
\alias{rollmax}
\alias{rollmedian}
\alias{rollmean.zoo}
\alias{rollmedian.zoo}
\alias{rollmax.zoo}
\alias{rollmean.ts}
\alias{rollmedian.ts}
\alias{rollmax.ts}
\alias{rollmean.default}
\alias{rollmedian.default}
\alias{rollmax.default}
\title{Rolling Means/Maximums/Medians}
\description{
Generic functions for computing rolling means, maximums and medians of ordered observations.
}
\usage{
rollmean(x, k, na.pad = FALSE, align = c("center", "left", "right"), \dots)
rollmax(x, k, na.pad = FALSE, align = c("center", "left", "right"), \dots)
rollmedian(x, k, na.pad = FALSE, align = c("center", "left", "right"), \dots)
}
\arguments{
  \item{x}{an object (representing a series of observations).}
  \item{k}{integer width of the rolling window. Must be odd for \code{rollmedian}.}  
  \item{na.pad}{logical. Should \code{NA} padding be added at beginning?}
  \item{align}{character specifying whether result should be left- or
    right-aligned or centered (default).}
  \item{\dots}{Further arguments passed to methods.}
}

\details{
These functions compute rolling means, maximums and medians respectively
and are thus similar to \code{\link{rapply}} but are
optimized for speed.

Currently, there are methods for \code{"zoo"} and \code{"ts"} series and
default methods (intended for vectors). The default method of \code{rollmedian}
is an interface to \code{\link{runmed}}.
}

\value{
An object of the same class as \code{x} with the rolling mean/max/median.
}

\seealso{\code{\link{rapply}}, \code{\link{zoo}}}

\examples{
x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x <- zoo(rnorm(12), x.Date)

rollmean(x, 3)
rollmax(x, 3)
rollmedian(x, 3)

xm <- zoo(matrix(1:12, 4, 3), x.Date[1:4])
rollmean(xm, 3)
rollmax(xm, 3)
rollmedian(xm, 3)

rapply(xm, 3, mean) # uses rollmean
rapply(xm, 3, function(x) mean(x)) # does not use rollmean

}
\keyword{ts}
