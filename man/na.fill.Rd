\name{na.fill}
\alias{na.fill}
\alias{na.fill.ts}
\alias{na.fill.zoo}
\alias{na.fill.default}
\title{Fill NA or specified positions.}
\description{
Generic function for filling \code{NA} values or specified positions.
}
\usage{
na.fill(object, fill, \dots)
\method{na.fill}{ts}(object, fill, ix, \dots)
\method{na.fill}{zoo}(object, fill, ix, \dots)
\method{na.fill}{default}(object, fill, ix, \dots)
}
\arguments{
  \item{object}{an object.}
  \item{fill}{a three component list or a vector that is coerced to a 
    list. Shorter objects are recycled.  The three components 
    represent the fill value to the left of the data, within the
    interior of the data and to the right of the data,
    respectively.  The value of any component may be the keyword
    \code{"extend"} to indicate repetition of the
    leftmost or rightmost non-NA value or linear interpolation in the interior.
    \code{NULL} means that items are dropped rather than filled.}
  \item{ix}{logical. Should be the same length as the number of time points.
    Indicates which time points not to fill. This defaults to the non-NA values.}
  \item{\dots}{further arguments passed to methods.}
}

\details{Fill \code{NA} or indicated values.}

\seealso{\code{\link{na.approx}}}

\examples{

z <- zoo(c(NA, 2, NA, 1, 4, 5, 2, NA))
na.fill(z, "extend")
na.fill(z, c("extend", NA))
na.fill(z, -(1:3))
na.fill(z, list(NA, NULL, NA))

}
\keyword{ts}
