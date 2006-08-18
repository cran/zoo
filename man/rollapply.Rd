\name{rollapply}
\alias{rapply}
\alias{rapply-deprecated}
\alias{rollapply}
\alias{rollapply.zoo}
\alias{rollapply.ts}
\title{Apply Rolling Functions}
\description{
  A generic function for applying a function to rolling margins of an array.
}
\usage{
rollapply(data, width, FUN, by = 1, ascending = TRUE, by.column = TRUE,
 na.pad = FALSE, align = c("center", "left", "right"), \dots)
}
\arguments{
  \item{data}{the data to be used (representing a series of observations).}
  \item{width}{number of points per group.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be quoted.}
  \item{by}{calculate FUN for trailing width points at every by-th time
    point.}
  \item{ascending}{logical. If TRUE then points are passed to \code{FUN} in
    ascending order of time; otherwise, they are passed in descending order.}
  \item{by.column}{logical. If \code{TRUE}, \code{FUN} is applied to each column separately.}
  \item{na.pad}{logical. If \code{TRUE} 
    then additional elements or rows of \code{NA}s are added so that 
    result has same number of elements or rows as \code{data}.}
  \item{align}{character specifying whether result should be left- or
    right-aligned or centered (default).}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\details{
  Groups time points in successive sets of \code{width} time points and 
  applies \code{FUN} to the corresponding values.   If \code{FUN} is
  \code{mean}, \code{max} or \code{median} and \code{by.column} is 
  \code{TRUE} and there are no extra arguments
  then special purpose code is used to enhance performance.
  See \code{\link{rollmean}}, \code{\link{rollmax}} and \code{\link{rollmedian}}
  for more details.
  
  Currently, there are methods for \code{"zoo"} and \code{"ts"} series.
  
  In previous versions, this function was called \code{rapply}. It was renamed
  because from R 2.4.0 on, base R provides a different function \code{rapply}
  for recursive (and not rolling) application of functions. The function 
  \code{zoo::rapply} is still provided for backward compatibility, however
  it dispatches now to \code{rollapply} methods.
}
\value{
  A object of the same class as \code{data} with the results of the rolling function.
}

\seealso{\code{\link{rollmean}}}

\examples{
z <- zoo(11:15, as.Date(31:35)) 
rollapply(z, 2, mean)

z2 <- zoo(rnorm(6))
rollapply(z2, 3, mean, by = 3) # means of nonoverlapping groups of 3
aggregate(z2, c(3,3,3,6,6,6), mean) # same

rollapply(z2, 3, mean) # uses rollmean which is optimized for mean
rollmean(z2, 3) # same
rollapply(z2, 3, (mean)) # does not use rollmean
}
\keyword{iteration}
\keyword{array}
\keyword{ts}
