\name{index}
\alias{index}
\alias{index.default}
\alias{index.zoo}
\alias{index.ts}
\alias{time.zoo}
\alias{index<-}
\alias{index<-.zoo}
\alias{time<-}
\alias{time<-.zoo}
\alias{start.zoo}
\alias{end.zoo}
\title{Extracting/Replacing the Index of Objects}
\description{
Generic functions for extracting the index of an object
and replacing it.
}
\usage{
index(x, \dots)
index(x) <- value
}
\arguments{
  \item{x}{an object.}
  \item{\dots}{further arguments passed to methods.}
  \item{value}{an ordered vector of the same length
    as the \code{"index"} attribute of \code{x}.}
}

\details{
\code{index} is a generic function for extracting the index
of objects, currently it has a default method and a method
for \code{\link{zoo}} objects which is the same as the
\code{\link{time}} method for \code{\link{zoo}} objects.
Another pair of generic functions provides replacing
the \code{index} or \code{time} attribute. 
Methods are available for \code{"zoo"} objects only, see examples below.

The start and end of the index/time can be queried by using
the methods of \code{start} and \code{end}.
}

\seealso{\code{\link{time}}, \code{\link{zoo}}}

\examples{
x.date <- as.Date(paste(2003, 2, c(1, 3, 7, 9, 14), sep = "-"))
x <- zoo(rnorm(5), x.date)

## query index/time of a zoo object
index(x)
time(x)

## change class of index from Date to POSIXct
## relative to current time zone
x
index(x) <- as.POSIXct(format(time(x)),tz="")
x

## replace index/time of a zoo object
index(x) <- 1:5
x
time(x) <- 6:10
x

## query start and end of a zoo object
start(x)
end(x)

## query index of a usual matrix
xm <- matrix(rnorm(10), ncol = 2)
index(xm)
}
\keyword{ts}
