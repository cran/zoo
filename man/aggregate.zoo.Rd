\name{aggregate.zoo}
\alias{aggregate.zoo}
\title{Compute Summary Statistics of zoo Objects}
\description{
Splits a zoo object into subsets along a coarser index grid,
computes summary statistics for each, and returns the 
reduced zoo object.
}

\usage{
\method{aggregate}{zoo}(x, by, FUN, \dots)
}

\arguments{
  \item{x}{an object of class \code{"zoo"}.}
  \item{by}{index vector the same length as \code{index(x)} which defines
    aggregation groups and the new index to be associated with each group.}
  \item{FUN}{a scalar function to compute the summary statistics
    which can be applied to all subsets.}
  \item{\dots}{further arguments passed to \code{FUN}.}
}

\value{
An object of class \code{"zoo"}.
}

\seealso{\code{\link{zoo}}}

\examples{
## averaging over values in a month:
# long series
x.date <- as.POSIXct(paste("2003-", rep(1:4, 4:1), "-", sample(1:28, 10, replace = TRUE), sep = ""))
x <- zoo(rnorm(12), x.date)
# coarser dates
x.date2 <- as.POSIXct(paste("2003-", rep(1:4, 4:1), "-1", sep = ""))
x2 <- aggregate(x, x.date2, mean)
# compare time series
plot(x)
lines(x2, col = 2)


## aggregate on month and extend to monthly time series
# test data
if(require(chron)) {
y <- zoo(matrix(11:15,nr=5,nc=2), chron(c(15,20,80,100,110)))
colnames(y) <- c("A", "B")

# aggregate by month using first of month as times for coarser series
# Uses fact that chron dates format, by default, to mm/dd/yy
firstofmonth <- function(x) chron(sub("/../", "/01/", format(x)))
y2 <- aggregate.zoo(y, firstofmonth(time(y)), tail, 1)

# fill in missing months by merging with an empty series containing
# a complete set of 1st of the months
yrt2 <- range(time(y2))
y0 <- zoo(,seq(from = yrt2[1], to = yrt2[2], by = "month"))[,-1]
merge(y2, y0)
}


## create regular series using Date class
# test data
x <- zoo(1:5, structure(c(15,20,80,100,110), class = "Date"))

# aggregate by month using first of month as times for coarser series
# Uses fact that format.Date defaults to yyyy-mm-dd
firstofmonth <- function(x) as.Date(sub("..$", "01", format(x)))
x2 <- aggregate.zoo(x, firstofmonth(time(x)), tail, 1)

# fill in missing months by merging with an empty series containing
# a complete set of 1st of the months
xrt2 <- range(time(x2))
x0 <- zoo( , seq(from = xrt2[1], to = xrt2[2], by = "month"))[,-1]
merge(x2, x0)


}
\keyword{ts}
