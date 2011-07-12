\name{rollapply}
\alias{rollapply}
\alias{rollapplyr}
\alias{rollapply.default}
\alias{rollapply.ts}
\alias{rollapply.zoo}
\title{Apply Rolling Functions}
\description{
  A generic function for applying a function to rolling margins of an array.
}
\usage{
rollapply(data, \dots)
\method{rollapply}{ts}(data, \dots)
\method{rollapply}{zoo}(data, width, FUN, \dots, by = 1, by.column = TRUE, 
    fill = if (na.pad) NA, na.pad = FALSE, partial = FALSE, 
    align = c("center", "left", "right"))
\method{rollapply}{default}(data, \dots)
rollapplyr(\dots, align = "right")
}
\arguments{
  \item{data}{the data to be used (representing a series of observations).}
  \item{width}{numeric vector or list. In the simplest case this is an integer
    specifying the window width which is aligned to the original sample according
    to the \code{align} argument. Alternatively, \code{width} can be a list
    regarded as offsets compared to the current time, see below for details.}
  \item{FUN}{the function to be applied.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{by}{calculate FUN at every \code{by}-th time point rather than 
    every point.  \code{by} is only used if \code{width} is length 1 and either
    a plain scalar or a list.}
  \item{by.column}{logical. If \code{TRUE}, \code{FUN} is applied to each column separately.}
  \item{fill}{a three-component vector or list (recycled otherwise) providing
    filling values at the left/within/to the right of the data range.
    See the \code{fill} argument of \code{\link{na.fill}} for details.}
  \item{na.pad}{deprecated. Use \code{fill = NA} instead of \code{na.pad = TRUE}.}
  \item{partial}{logical or numeric. If \code{FALSE} (default) then \code{FUN} is only 
    applied when all indexes of the rolling window are within the observed time range.
    If \code{TRUE}, then the subset of indexes that are in range are passed to \code{FUN}.
    A numeric argument to \code{partial} can be used to determin the minimal
    window size for partial computations. See below for more details.}
  \item{align}{specifyies whether the index of the result
    should be left- or right-aligned or centered (default) compared
    to the rolling window of observations. This argument is only used if
    \code{width} represents widths.}
}
\details{
  If \code{width} is a plain numeric vector its elements are regarded as widths
  to be interpreted in conjunction with \code{align} whereas if \code{width} is a list
  its components are regarded as offsets. In the above cases if the length of
  \code{width} is 1 then \code{width} is recycled for every \code{by}-th point.
  If \code{width} is a list its components represent integer offsets such that 
  the i-th component of the list refers to time points at positions
  \code{i + width[[i]]}.  If any of these points are below 1 or above the 
  length of \code{index(data)} then \code{FUN} is not evaluated for that
  point unless \code{partial = TRUE} and in that case only the valid
  points are passed.

  The rolling function can also be applied to partial windows by setting \code{partial = TRUE}
  For example, if \code{width = 3, align = "right"} then for the first point
  just that point is passed to \code{FUN} since the two points to its
  left are out of range.  For the same example, if \code{partial = FALSE} then \code{FUN} is not
  invoked at all for the first two points.  If \code{partial} is a numeric then it
  specifies the minimum number of offsets that must be within range.  Negative
  \code{partial} is interpreted as \code{FALSE}.

  If \code{FUN} is \code{mean}, \code{max} or \code{median} and \code{by.column} is 
  \code{TRUE} and width is a plain scalar and there are no other arguments
  then special purpose code is used to enhance performance.
  Also in the case of \code{mean} such special purpose code is only invoked if the
  \code{data} argument has no \code{NA} values.
  See \code{\link{rollmean}}, \code{\link{rollmax}} and \code{\link{rollmedian}}
  for more details.
  
  Currently, there are methods for \code{"zoo"} and \code{"ts"} series
  and \code{"default"} method for ordinary vectors and matrices.

  \code{rollapplyr} is a wrapper around \code{rollapply} that uses a default
  of \code{align = "right"}.
}
\value{
  A object of the same class as \code{data} with the results of the rolling function.
}

\seealso{\code{\link{rollmean}}}

\examples{
## rolling mean
z <- zoo(11:15, as.Date(31:35))
rollapply(z, 2, mean)

## non-overlapping means
z2 <- zoo(rnorm(6))
rollapply(z2, 3, mean, by = 3)      # means of nonoverlapping groups of 3
aggregate(z2, c(3,3,3,6,6,6), mean) # same

## optimized vs. customized versions
rollapply(z2, 3, mean)   # uses rollmean which is optimized for mean
rollmean(z2, 3)          # same
rollapply(z2, 3, (mean)) # does not use rollmean

## rolling regression:
## set up multivariate zoo series with
## number of UK driver deaths and lags 1 and 12
seat <- as.zoo(log(UKDriverDeaths))
time(seat) <- as.yearmon(time(seat))
seat <- merge(y = seat, y1 = lag(seat, k = -1),
  y12 = lag(seat, k = -12), all = FALSE)

## run a rolling regression with a 3-year time window
## (similar to a SARIMA(1,0,0)(1,0,0)_12 fitted by OLS)
rr <- rollapply(seat, width = 36,
  FUN = function(z) coef(lm(y ~ y1 + y12, data = as.data.frame(z))),
  by.column = FALSE, align = "right")

## plot the changes in coefficients
## showing the shifts after the oil crisis in Oct 1973
## and after the seatbelt legislation change in Jan 1983
plot(rr)

## different values of rule argument
z <- zoo(c(NA, NA, 2, 3, 4, 5, NA))
rollapply(z, 3, sum, na.rm = TRUE)
rollapply(z, 3, sum, na.rm = TRUE, fill = NULL)
rollapply(z, 3, sum, na.rm = TRUE, fill = NA)
rollapply(z, 3, sum, na.rm = TRUE, partial = TRUE)

# this will exclude time points 1 and 2
# It corresonds to align = "right", width = 3
rollapply(zoo(1:8), list(seq(-2, 0)), sum)

# but this will include points 1 and 2
rollapply(zoo(1:8), list(seq(-2, 0)), sum, partial = 1)
rollapply(zoo(1:8), list(seq(-2, 0)), sum, partial = 0)

# so will this
rollapply(zoo(1:8), list(seq(-2, 0)), sum, fill = NA)

# by = 3, align = "right"
L <- rep(list(NULL), 8)
L[seq(3, 8, 3)] <- list(seq(-2, 0))
str(L)
rollapply(zoo(1:8), L, sum)

rollapply(zoo(1:8), list(0:2), sum, fill = 1:3)
rollapply(zoo(1:8), list(0:2), sum, fill = 3)

L2 <- rep(list(-(2:0)), 10)
L2[5] <- list(NULL)
str(L2)
rollapply(zoo(1:10), L2, sum, fill = "extend")
rollapply(zoo(1:10), L2, sum, fill = list("extend", NULL))

rollapply(zoo(1:10), L2, sum, fill = list("extend", NA))

rollapply(zoo(1:10), L2, sum, fill = NA)
rollapply(zoo(1:10), L2, sum, fill = 1:3)
rollapply(zoo(1:10), L2, sum, partial = TRUE)
rollapply(zoo(1:10), L2, sum, partial = TRUE, fill = 99)

rollapply(zoo(1:10), list(-1), sum, partial = 0)
rollapply(zoo(1:10), list(-1), sum, partial = TRUE)

rollapply(zoo(cbind(a = 1:6, b = 11:16)), 3, rowSums, by.column = FALSE)

# these two are the same
rollapply(zoo(cbind(a = 1:6, b = 11:16)), 3, sum)
rollapply(zoo(cbind(a = 1:6, b = 11:16)), 3, colSums, by.column = FALSE)

# these two are the same
rollapply(zoo(1:6), 2, sum, by = 2, align = "right")
aggregate(zoo(1:6), c(2, 2, 4, 4, 6, 6), sum)

# these two are the same
rollapply(zoo(1:3), list(-1), c)
lag(zoo(1:3), -1)

# these two are the same
rollapply(zoo(1:3), list(1), c)
lag(zoo(1:3))

# these two are the same
rollapply(zoo(1:5), list(c(-1, 0, 1)), sum)
rollapply(zoo(1:5), 3, sum)

# these two are the same
rollapply(zoo(1:5), list(0:2), sum)
rollapply(zoo(1:5), 3, sum, align = "left")

# these two are the same
rollapply(zoo(1:5), list(-(2:0)), sum)
rollapply(zoo(1:5), 3, sum, align = "right")

# these two are the same
rollapply(zoo(1:6), list(NULL, NULL, -(2:0)), sum)
rollapply(zoo(1:6), 3, sum, by = 3, align = "right")

# these two are the same
rollapply(zoo(1:5), list(c(-1, 1)), sum)
rollapply(zoo(1:5), 3, function(x) sum(x[-2]))

# these two are the same
rollapply(1:5, 3, rev)
embed(1:5, 3)

# these four are the same
x <- 1:6
rollapply(c(0, 0, x), 3, sum, align = "right") - x
rollapply(x, 3, sum, partial = TRUE, align = "right") - x
rollapply(x, 3, function(x) sum(x[-3]), partial = TRUE, align = "right")
rollapply(x, list(-(2:1)), sum, partial = 0)

# same as Matlab's buffer(x, n, p) for valid non-negative p
# See http://www.mathworks.com/help/toolbox/signal/buffer.html
x <- 1:30; n <- 7; p <- 3
t(rollapply(c(rep(0, p), x, rep(0, n-p)), n, by = n-p, c))

# these three are the same
y <- 10 * seq(8); k <- 4; d <- 2
# 1
# from http://ucfagls.wordpress.com/2011/06/14/embedding-a-time-series-with-time-delay-in-r-part-ii/
Embed <- function(x, m, d = 1, indices = FALSE, as.embed = TRUE) {
    n <- length(x) - (m-1)*d
    X <- seq_along(x)
    if(n <= 0)
        stop("Insufficient observations for the requested embedding")
    out <- matrix(rep(X[seq_len(n)], m), ncol = m)
    out[,-1] <- out[,-1, drop = FALSE] +
        rep(seq_len(m - 1) * d, each = nrow(out))
    if(as.embed)
        out <- out[, rev(seq_len(ncol(out)))]
    if(!indices)
        out <- matrix(x[out], ncol = m)
    out
}
Embed(y, k, d)
# 2
rollapply(y, list(-d * seq(0, k-1)), c)
# 3
rollapply(y, d*k-1, function(x) x[d * seq(k-1, 0) + 1])

}
\keyword{iteration}
\keyword{array}
\keyword{ts}
