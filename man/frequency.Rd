\name{frequency<-}
\alias{frequency<-}
\alias{frequency<-.zoo}
\alias{frequency<-.zooreg}
\title{Replacing the Index of Objects}
\description{
Generic function for replacing the frequency of an object.
}
\usage{
frequency(x) <- value
}
\arguments{
  \item{x}{an object.}
  \item{value}{a frequency.}
}

\details{
\code{frequency<-} is a generic function for replacing (or assigning)
the frequency of an object. Currently, there is a \code{"zooreg"} and
a \code{"zoo"} method. In both cases, the \code{value} is assigned
to the \code{"frequency"} of the object if it complies with the
\code{index(x)}.
}

\seealso{\code{\link{zooreg}}, \code{\link{index}}}

\examples{
z <- zooreg(1:5)
z
as.ts(z)
frequency(z) <- 3
z
as.ts(z)
}
\keyword{ts}
