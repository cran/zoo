\name{index}
\alias{index}
\alias{index}
\alias{index.default}
\alias{index.zoo}
\alias{time.zoo}
\title{Extracting Index From Objects}
\description{
\code{index} is a generic function for extracting 
the index of an object.
}
\usage{
index(x, \dots)
}
\arguments{
  \item{x}{an object,}
  \item{\dots}{currently not used.}
}

\details{
\code{index} is a generic function for extracting the index
of objects, currently it has a default method and a method
for \code{\link{zoo}} objects which is the same as the
\code{\link{time}} method for \code{\link{zoo}} objects.
}

\seealso{\code{\link{time}}, \code{\link{zoo}}}

\examples{
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)
index(x)

x <- matrix(rnorm(10), ncol = 2)
index(x)
}
\keyword{ts}
