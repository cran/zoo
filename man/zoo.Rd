\name{zoo}
\alias{zoo}
\alias{print.zoo}
\alias{is.zoo}
\alias{[.zoo}
\title{Z's Ordered Observations}
\description{
\code{zoo} is the creator for an S3 class of indexed
totally ordered observations which includes irregular
time series.
}
\usage{
zoo(x, order.by)
}
\arguments{
  \item{x}{a vector or matrix.}
  \item{order.by}{a vector by which the observations in \code{x}
    are ordered.}
}

\details{
\code{zoo} is still under development, but it provides \code{plot}
and \code{time} methods for \code{"zoo"} objects and can convert
\code{"ts"} and \code{"irts"} objects with \code{\link{as.zoo}}.
}

\value{
A vector or matrix with an \code{"index"} attribute of the same
dimension (\code{NROW(x)}) by which \code{x} is ordered.
}

\seealso{\code{\link{plot.zoo}}, \code{\link{index}}}

\examples{
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)
plot(x)

time(x)

x[1:3]
}
\keyword{ts}
