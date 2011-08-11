\name{merge.zoo}
\alias{merge.zoo}
\alias{rbind.zoo}
\alias{c.zoo}
\alias{cbind.zoo}
\title{Merge Two or More zoo Objects}
\description{
Merge two zoo objects by common indexes (times), or do other
versions of database \emph{join} operations.
}

\usage{
\method{merge}{zoo}(\dots, all = TRUE, fill = NA, suffixes = NULL,
  check.names = FALSE, retclass = c("zoo", "list", "data.frame"),
  drop = TRUE)
}

\arguments{
  \item{\dots}{two or more objects, usually of class \code{"zoo"}.}
  \item{all}{logical vector having the same length as the number of \code{"zoo"}
    objects to be merged (otherwise expanded).}
  \item{fill}{an element for filling gaps in merged \code{"zoo"}
    objects (if any).}
  \item{suffixes}{character vector of the same length as the number of
    \code{"zoo"} objects specifying the suffixes to be used for making
    the merged column names unique.}
  \item{check.names}{See \code{link{read.table}}.}
  \item{retclass}{character that specifies the class of the returned result.
    It can be \code{"zoo"} (the default), \code{"list"} or \code{NULL}. For 
    details see below.}
  \item{drop}{logical. If a \code{"zoo"} object without observations is
    merged with a one-dimensional \code{"zoo"} object (vector or 1-column
    matrix), should the result be a vector (\code{drop = TRUE}) or a
    1-column matrix (\code{drop = FALSE})? The former is the default
    in the \code{Merge} method, the latter in the \code{cbind} method.}
}

\details{
The \code{merge} method for \code{"zoo"} objects combines the columns
of several objects along the union of the dates 
for \code{all = TRUE}, the default,
or the intersection of their dates for \code{all = FALSE}
filling up the created gaps (if any) with the \code{fill} pattern.

The first argument must be a \code{zoo} object.   If any of the remaining
arguments are plain vectors or matrices with the same length or number
of rows as the first argument then such arguments are coerced to \code{"zoo"}
using \code{as.zoo}.  If they are plain but have length 1 then they are
merged after all non-scalars such that their column is filled with the
value of the scalar.

\code{all} can be a vector of the same length as the number of \code{"zoo"}
objects to merged (if not, it is expanded): All indexes
(times) of the objects corresponding to \code{TRUE} are included, for those
corresponding to \code{FALSE} only the indexes present in all objects are
included.  This allows intersection, union and left and right joins 
to be expressed.

If \code{retclass} is \code{"zoo"} (the default) a single merged \code{"zoo"}
object is returned. If it is set to \code{"list"} a list of \code{"zoo"}
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
not allowed) and combines the rows of the objects. Furthermore, the
\code{c} method is identical to the \code{rbind} method.
}

\value{
An object of class \code{"zoo"} if \code{retclass="zoo"}, an object of
class \code{"list"} if \code{retclass="list"} or modified arguments as
explained above if \code{retclass=NULL}.   If the result is an object
of class \code{"zoo"} then its frequency is the common frequency of its
zoo arguments, if they have a common frequency.
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

merge(z, coredata(z),  1)


## merge multiple series represented in a long form data frame 
## into a multivariate zoo series and plot, one series for each site.
## Additional examples can be found here:
## https://stat.ethz.ch/pipermail/r-help/2009-February/187094.html
## https://stat.ethz.ch/pipermail/r-help/2009-February/187096.html
##
m <- 5 # no of years
n <- 6 # no of sites
sites <- LETTERS[1:n]
set.seed(1)
DF <- data.frame(site = sites, year = 2000 + 1:m, data = rnorm(m*n))
tozoo <- function(x) zoo(x$data, x$year) 
Data <- do.call(merge, lapply(split(DF, DF$site), tozoo))
plot(Data, screen = 1, col = 1:n, pch = 1:n, type = "o", xlab = "")
legend("bottomleft", legend = sites, lty = 1, pch = 1:n, col = 1:n)

## for each index value in x merge it with the closest index value in y
## but retaining x's times.
x<-zoo(1:3,as.Date(c("1992-12-13", "1997-05-12", "1997-07-13")))
y<-zoo(1:5,as.Date(c("1992-12-15", "1992-12-16", "1997-05-10","1997-05-19", "1997-07-13")))
f <- function(u) which.min(abs(as.numeric(index(y)) - as.numeric(u)))
ix <- sapply(index(x), f)
cbind(x, y = coredata(y)[ix])

## this merges each element of x with the closest time point in y at or
## after x's time point (whereas in previous example it could be before
## or after)
window(na.locf(merge(x, y), fromLast = TRUE), index(x))


}
\keyword{ts}
