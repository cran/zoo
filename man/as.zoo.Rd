\name{as.zoo}
\alias{as.zoo}
\alias{as.zoo.default}
\alias{as.zoo.ts}
\alias{as.zoo.irts}
\title{Coercion to zoo}
\description{
A generic function for coercing objects to class \code{"zoo"}.
}
\usage{
as.zoo(x, \dots)
}
\arguments{
  \item{x}{an object,}
  \item{\dots}{currently not used.}
}

\details{
\code{as.zoo} currently has a default method and methods
for \code{\link{ts}} and \code{\link[tseries]{irts}} objects.
}

\value{
A \code{\link{zoo}} object.
}

\seealso{\code{\link{zoo}}, \code{\link{ts}}, \code{\link[tseries]{irts}}}

\examples{
as.zoo(rnorm(5))
}
\keyword{ts}
