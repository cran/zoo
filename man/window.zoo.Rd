\name{window.zoo}
\alias{window.zoo}
\alias{window<-.zoo}
\title{Extract/Replacing the Time Windows of Objects}
\description{
Methods for extracting time windows
of \code{"zoo"} objects and replacing it.
}

\usage{
\method{window}{zoo}(x, index. = index(x), start = NULL, end = NULL, \dots)
\method{window}{zoo}(x, index. = index(x), start = NULL, end = NULL, \dots) <- value
}

\arguments{
  \item{x}{an object.}
  \item{index.}{the index/time window which should be extracted.}
  \item{start}{an index/time value. Only the indexes in \code{index}
    which are greater or equal to \code{start} are used.  If the index
    class supports comparisons to character variables, as does \code{"Date"}
    class, \code{"yearmon"} class, \code{"yearqtr"} class and 
    the \code{chron} package classes \code{"dates"} and \code{"times"}
    then \code{start} may alternately be a character variable.}
  \item{end}{an index/time value. Only the indexes in \code{index}
    which are lower or equal to \code{end} are used.  Similar comments
    about character variables mentioned under \code{start} apply
    here too.}
  \item{value}{a suitable value object for use with \code{window(x)}.}
  \item{\dots}{currently not used.}
}

\value{
Either the time window of the object is extracted (and hence return a \code{"zoo"}
object) or it is replaced.
}

\seealso{\code{\link{zoo}}}

\examples{
## zoo example
x.date <- as.Date(paste(2003, rep(1:4, 4:1), seq(1,19,2), sep = "-"))
x <- zoo(matrix(rnorm(20), ncol = 2), x.date)
x

window(x, start = as.Date("2003-02-01"), end = as.Date("2003-03-01"))
window(x, index = x.date[1:6], start = as.Date("2003-02-01"))
window(x, index = x.date[c(4, 8, 10)])
window(x, index = x.date[c(4, 8, 10)]) <- matrix(1:6, ncol = 2)
x

## for classes that support comparisons with "character" variables
## start and end may be "character".
window(x, start = "2003-02-01")

## zooreg example (with plain numeric index)
z <- zooreg(rnorm(10), start = 2000, freq = 4)
window(z, start = 2001.75)
window(z, start = c(2001, 4))

## replace data at times of d0 which are in dn
d1 <- d0 <- zoo(1:10) + 100
dn <- - head(d0, 4)

window(d1, time(dn)) <- coredata(dn)

}
\keyword{ts}
