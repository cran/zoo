\name{merge.zoo}
\alias{merge.zoo}
\title{Merge Two or More zoo Objects}
\description{
Merge two zoo objects by common indexes (times), or do other
versions of database "join" operations.
}

\usage{
\method{merge}{zoo}(x, y, \dots, all = TRUE)
}

\arguments{
  \item{x, y, \dots}{two or more objects of class \code{"zoo"}.}
  \item{all}{logical vector of the same length as the number of \code{"zoo"}
    objects which should be merged (otherwise expanded). All indexes
    (times) of the objects corresponding to TRUE are included, for those
    corresponding to FALSE only the indexes present in all objects are
    included. For the default \code{all = TRUE} this corresponds to 
    a union of all indexes, for \code{all = FALSE} to the intersection
    of all indexes.}
}

\details{
The indexes of different
\code{"zoo"} objects can be of different classes and are coerced to
one class in the resulting object (with a warning).
}

\value{
An object of class \code{"zoo"}.
}

\seealso{\code{\link{zoo}}}

\examples{
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)

y1 <- zoo(matrix(1:10, ncol = 2), 1:5)
y2 <- zoo(matrix(rnorm(10), ncol = 2), 3:7)

merge(y1, y2, all = FALSE)
merge(x, y1, y2, all = TRUE)
}
\keyword{ts}
