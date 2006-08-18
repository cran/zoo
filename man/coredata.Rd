\name{coredata}
\alias{coredata}
\alias{coredata.default}
\alias{coredata.zoo}
\alias{coredata.ts}
\alias{coredata.its}
\alias{coredata.irts}
\alias{coredata<-}
\alias{coredata<-.zoo}
\alias{coredata<-.ts}
\alias{coredata<-.irts}
\alias{coredata<-.its}
\title{Extracting/Replacing the Core Data of Objects}
\description{
Generic functions for extracting the core data contained in
a (more complex) object and replacing it.
}

\usage{
coredata(x, \dots)
coredata(x) <- value
}

\arguments{
  \item{x}{an object.}
  \item{\dots}{further arguments passed to methods.}
  \item{value}{a suitable value object for use with \code{x}.}
}

\value{
In \code{zoo}, there are currently \code{coredata} methods for time series
objects of class \code{"zoo"}, \code{"ts"}, \code{"its"}, \code{"irts"}, all of
which strip off the index/time attributes and return only the observations.
The are also corresponding replacement methods for these classes.
}

\seealso{\code{\link{zoo}}}

\examples{
x.date <- as.Date(paste(2003, rep(1:4, 4:1), seq(1,20,2), sep = "-"))
x <- zoo(matrix(rnorm(20), ncol = 2), x.date)

## the full time series
x
## and only matrix of observations
coredata(x)

## change the observations
coredata(x) <- matrix(1:20, ncol = 2)
x
}
\keyword{ts}
