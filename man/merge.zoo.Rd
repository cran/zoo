\name{merge.zoo}
\alias{merge.zoo}
\alias{rbind.zoo}
\alias{cbind.zoo}
\title{Merge Two or More zoo Objects}
\description{
Merge two zoo objects by common indexes (times), or do other
versions of database \emph{join} operations.
}

\usage{
\method{merge}{zoo}(\dots, all = TRUE, fill = NA, suffixes = NULL,
  retclass = c("zoo", "list"))
}

\arguments{
  \item{\dots}{two or more objects of class \code{"zoo"}.}
  \item{all}{logical vector having the same length as the number of \code{"zoo"}
    objects to be merged (otherwise expanded).}
  \item{fill}{an element for filling gaps in merged \code{"zoo"}
    objects (if any).}
  \item{suffixes}{character vector of the same length as the number of
    \code{"zoo"} objects specifying the suffixes to be used for making
    the merged column names unique.}
  \item{retclass}{character that specifies the class of the returned result.
    It can be \code{"zoo"} (the default), \code{"list"} or \code{NULL}. For 
    details see below.}
}

\details{
The \code{merge} method for \code{"zoo"} objects combines the columns
of several objects along the union of the dates 
for \code{all = TRUE}, the default,
or the intersection of their dates for \code{all = FALSE}
filling up the created gaps (if any) with the \code{fill} pattern.

\code{all} can be a vector of the same length as the number of \code{"zoo"}
objects to merged (if not, it is expanded): All indexes
(times) of the objects corresponding to \code{TRUE} are included, for those
corresponding to \code{FALSE} only the indexes present in all objects are
included.  This allows intersection, union and left and right joins 
to be expressed.

If \code{retclass} is \code{"zoo"} (the default) a single merged \code{"zoo"}
object is returned. If it is set to \code{"list"} a list of \code{"zoo}
objects is returned. If \code{retclass = NULL} then instead of returning a value it updates each
argument (if it is a variable rather than an expression) in
place so as to extend or reduce it to use the common index vector.

The indexes of different
\code{"zoo"} objects can be of different classes and are coerced to
one class in the resulting object (with a warning).

The default \code{cbind} method is essentially the default \code{merge}
method, but does not support the \code{retclass} argument.
The \code{rbind}
method combines the dates of the \code{"zoo"} objects (duplicate dates are
not allowed) and combines the rows of the objects.
}

\value{
An object of class \code{"zoo"}.
}

\seealso{\code{\link{zoo}}}

\examples{
## simple merging
x.date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
x <- zoo(rnorm(5), x.date)

y1 <- zoo(matrix(1:10, ncol = 2), 1:5)
y2 <- zoo(matrix(rnorm(10), ncol = 2), 3:7)

## using arguments `fill' and `suffixes'
merge(y1, y2, all = FALSE)
merge(y1, y2, all = FALSE, suffixes = c("a", "b"))
merge(y1, y2, all = TRUE)
merge(y1, y2, all = TRUE, fill = 0)

## if different index classes are merged, as in
## the next merge example then ## a warning is issued and 
### the indexes are coerced.
## It is up to the user to ensure that the result makes sense.
merge(x, y1, y2, all = TRUE)

## extend an irregular series to a regular one:
# create a constant series
z <- zoo(1, seq(4)[-2])
# create a 0 dimensional zoo series
z0 <- zoo(, 1:4)
# do the extension
merge(z, z0)
# same but with zero fill
merge(z, z0, fill = 0)
}
\keyword{ts}
