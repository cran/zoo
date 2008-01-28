
\name{aggregate.zoo}
\alias{aggregate.zoo}
\title{Compute Summary Statistics of zoo Objects}
\description{
Splits a \code{"zoo"} object into subsets along a coarser index grid,
computes summary statistics for each, and returns the 
reduced \code{"zoo"} object.
}

\usage{
\method{aggregate}{zoo}(x, by, FUN, \dots, regular = NULL, frequency = NULL)
}

\arguments{
  \item{x}{an object of class \code{"zoo"}.}
  \item{by}{index vector of the same length as \code{index(x)} which defines
    aggregation groups and the new index to be associated with each group.
    If \code{by} is a function, then it is applied to \code{index(x)} to
    obtain the aggregation groups.}
  \item{FUN}{a scalar function to compute the summary statistics
    which can be applied to all subsets.}
  \item{\dots}{further arguments passed to \code{FUN}.}
  \item{regular}{logical. Should the aggregated series be coerced to class \code{"zooreg"}
    (if the series is regular)? The default is \code{FALSE} for \code{"zoo"} series and
    \code{TRUE} for \code{"zooreg"} series.}
  \item{frequency}{numeric indicating the frequency of the aggregated series
    (if a \code{"zooreg"} series should be returned. The default is to
    determine the frequency from the data if \code{regular} is \code{TRUE}.
    If \code{frequency} is specified, it sets \code{regular} to \code{TRUE}.
    See examples for illustration.}
}

\value{
An object of class \code{"zoo"} or \code{"zooreg"}.
}

\seealso{\code{\link{zoo}}}

\examples{
## averaging over values in a month:
# long series
x.date <- as.Date(paste(2004, rep(1:4, 4:1), seq(1,20,2), sep = "-"))
x <- zoo(rnorm(12), x.date)
# coarser dates
x.date2 <- as.Date(paste(2004, rep(1:4, 4:1), 1, sep = "-"))
x2 <- aggregate(x, x.date2, mean)
# compare time series
plot(x)
lines(x2, col = 2)

## aggregate a daily time series to a quarterly series
# create zoo series
tt <- as.Date("2000-1-1") + 0:300
z.day <- zoo(0:300, tt)

# function which returns corresponding first "Date" of quarter
first.of.quarter <- function(tt) as.Date(as.yearqtr(tt))

# average z over quarters
# 1. via "yearqtr" index (regular)
# 2. via "Date" index (not regular)
z.qtr1 <- aggregate(z.day, as.yearqtr, mean)
z.qtr2 <- aggregate(z.day, first.of.quarter, mean)

# The last one used the first day of the quarter but suppose
# we want the first day of the quarter that exists in the series
# (and the series does not necessarily start on the first day
# of the quarter).
z.day[!duplicated(as.yearqtr(time(z.day)))]

# This is the same except it uses the last day of the quarter.
# It requires R 2.6.0 which introduced the fromLast= argument.
\dontrun{
z.day[!duplicated(as.yearqtr(time(z.day)), fromLast = TRUE)]
}

# The aggregated series above are of class "zoo" (because z.day
# was "zoo"). To create a regular series of class "zooreg",
# the frequency can be automatically chosen
zr.qtr1 <- aggregate(z.day, as.yearqtr, mean, regular = TRUE)
# or specified explicitely
zr.qtr2 <- aggregate(z.day, as.yearqtr, mean, frequency = 4)


## aggregate on month and extend to monthly time series
if(require(chron)) {
y <- zoo(matrix(11:15,nr=5,nc=2), chron(c(15,20,80,100,110)))
colnames(y) <- c("A", "B")

# aggregate by month using first of month as times for coarser series
# using first day of month as repesentative time
y2 <- aggregate(y, as.Date(as.yearmon(time(y))), head, 1)

# fill in missing months by merging with an empty series containing
# a complete set of 1st of the months
yrt2 <- range(time(y2))
y0 <- zoo(,seq(from = yrt2[1], to = yrt2[2], by = "month"))
merge(y2, y0)
}

# given daily series keep only first point in each month at
# day 21 or more
z <- zoo(101:200, as.Date("2000-01-01") + seq(0, length = 100, by = 2))
zz <- z[as.numeric(format(time(z), "\%d")) >= 21]
zz[!duplicated(as.yearmon(time(zz)))]

# same except times are of "yearmon" class
aggregate(zz, as.yearmon, head, 1)

# aggregate POSIXct seconds data every 10 minutes
tt <- seq(10, 2000, 10)
x <- zoo(tt, structure(tt, class = c("POSIXt", "POSIXct")))
aggregate(x, time(x) - as.numeric(time(x)) \%\% 600, mean)
}
\keyword{ts}
