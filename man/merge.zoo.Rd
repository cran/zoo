\name{merge.zoo}
\alias{merge.zoo}
\title{Merge Two or More zoo Objects}
\description{
Merge two zoo objects by common indexes (times), or do other
versions of database "join" operations.
}

\usage{
\method{merge}{zoo}(\dots, all = TRUE, fill = NA, suffixes = NULL)
}

\arguments{
  \item{\dots}{two or more objects of class \code{"zoo"}.}
  \item{all}{logical vector of the same length as the number of \code{"zoo"}
    objects which should be merged (otherwise expanded). All indexes
    (times) of the objects corresponding to TRUE are included, for those
    corresponding to FALSE only the indexes present in all objects are
    included. For the default \code{all = TRUE} this corresponds to 
    a union of all indexes, for \code{all = FALSE} to the intersection
    of all indexes.}
  \item{fill}{an element for filling the gaps in merged \code{"zoo"}
    objects which can arise when all = TRUE and some zoo arguments have some
    index values not in all other zoo arguments.}
  \item{suffixes}{character vector of the same length as the number of
    \code{"zoo"} objects that specifies the suffixes to be used for making
    the merged column names unique.}
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
## simple merging
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)

y1 <- zoo(matrix(1:10, ncol = 2), 1:5)
y2 <- zoo(matrix(rnorm(10), ncol = 2), 3:7)

## using arguments `fill' and `suffixes'
merge(y1, y2, all = FALSE)
merge(y1, y2, all = FALSE, suffixes = c("a", "b"))

merge(x, y1, y2, all = TRUE)
merge(x, y1, y2, all = TRUE, fill = 0)


## extend an irregular series to a regular one:
# create a constant series
z <- zoo(1, seq(4)[-2])
# create a series with zero columns
z0 <- zoo(, 1:4)[,-1]
# do the extension
merge(z, z0)
# same but with zero fill
merge(z, z0, fill = 0)
}
\keyword{ts}
