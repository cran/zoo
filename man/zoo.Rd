\name{zoo}
\alias{zoo}
\alias{print.zoo}
\alias{is.zoo}
\alias{[.zoo}
\alias{str.zoo}
\alias{window.zoo}
\alias{rbind.zoo}
\title{Z's Ordered Observations}
\description{
\code{zoo} is the creator for an S3 class of indexed
totally ordered observations which includes irregular
time series.
}
\usage{
zoo(x = NA, order.by = index(x))
}
\arguments{
  \item{x}{a vector or matrix.}
  \item{order.by}{a vector by which the observations in \code{x}
    are ordered.}
}

\details{
\code{zoo} is still under development, but it provides \code{plot}
and \code{index} (or \code{time}) methods for \code{"zoo"} objects
and can convert \code{"ts"} and \code{"irts"} objects with
\code{\link{as.zoo}}.

The vector of indexes \code{order.by} can be of arbitrary class,
but it is essential that \code{order(order.by)} works. For other
functions it is assumed that \code{c()}, \code{length()},
\code{match()} and subsetting \code{[,} work.

If \code{x} is shorter or longer than \code{order.by} it is
expanded accordingly.
}

\value{
A vector or matrix with an \code{"index"} attribute of the same
dimension (\code{NROW(x)}) by which \code{x} is ordered.
}

\seealso{\code{\link{plot.zoo}}, \code{\link{index}}, \code{\link{merge.zoo}}}

\examples{
## simple creation and plotting
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)
plot(x)
time(x)

## subsetting
x[1:3]

# create a constant series
z <- zoo(1, seq(4)[-2])

# create a series with zero columns
z0 <- zoo(, 1:4)[,-1]
}
\keyword{ts}
