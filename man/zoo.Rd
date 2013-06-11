\name{zoo}
\alias{zoo}
\alias{with.zoo}
\alias{range.zoo}
\alias{print.zoo}
\alias{as.zoo.factor}
\alias{summary.zoo}
\alias{str.zoo}
\alias{is.zoo}
\alias{[.zoo}
\alias{[<-.zoo}
\alias{$.zoo}
\alias{$<-.zoo}
\alias{subset.zoo}
\alias{head.zoo}
\alias{tail.zoo}
\alias{Ops.zoo}
\alias{t.zoo}
\alias{cumsum.zoo}
\alias{cumprod.zoo}
\alias{cummin.zoo}
\alias{cummax.zoo}
\alias{mean.zoo}
\alias{median.zoo}
\alias{na.contiguous}
\alias{na.contiguous.data.frame}
\alias{na.contiguous.list}
\alias{na.contiguous.default}
\alias{na.contiguous.zoo}
\alias{scale.zoo}
\alias{xtfrm.zoo}
\alias{names.zoo}
\alias{names<-.zoo}
\alias{quantile.zoo}
\alias{rev.zoo}
\alias{transform.zoo}

\alias{index2char}
\alias{index2char.default}
\alias{index2char.numeric}

\alias{head.ts}
\alias{tail.ts}

\alias{ifelse.zoo}

\title{Z's Ordered Observations}
\description{
\code{zoo} is the creator for an S3 class of indexed
totally ordered observations which includes irregular
time series.
}
\usage{
zoo(x = NULL, order.by = index(x), frequency = NULL)
\method{print}{zoo}(x, style = , quote = FALSE, \dots) 
}
\arguments{
  \item{x}{a numeric vector, matrix or a factor.}
  \item{order.by}{an index vector with unique entries by which the
    observations in \code{x} are ordered. See the details for support
    of non-unique indexes.}
  \item{frequency}{numeric indicating frequency of \code{order.by}.
    If specified, it is checked whether \code{order.by} and \code{frequency}
    comply. If so, a regular \code{"zoo"} series is returned, i.e.,
    an object of class \code{c("zooreg", "zoo")}. See below and
    \code{\link{zooreg}} for more details.}
  \item{style}{a string specifying the printing style which can be
    \code{"horizontal"} (the default for vectors), \code{"vertical"}
    (the default for matrices) or \code{"plain"} (which first prints
    the data and then the index).}
  \item{quote}{logical. Should characters be quoted?}
  \item{\dots}{further arguments passed to the print methods of
    the data and the index.}
}

\details{
\code{zoo} provides infrastructure for ordered observations
which are stored internally in a vector or matrix with an
index attribute (of arbitrary class, see below).  The index
must have the same length as \code{NROW(x)} except in the 
case of a zero length numeric vector in which case the index
length can be any length. Emphasis has
been given to make all methods independent of the index/time class
(given in \code{order.by}). In principle, the data \code{x} could also
be arbitrary, but currently there is only support for vectors and matrices
and partial support for factors.

\code{zoo} is particularly aimed at irregular time series of numeric
vectors/matrices, but it also supports regular time series (i.e.,
series with a certain \code{frequency}).
\code{zoo}'s key design goals are independence of a particular
index/date/time class and consistency 
with \code{ts} and base \R by providing methods to standard generics. Therefore,
standard functions can be used to work with \code{"zoo"} objects and 
memorization of new commands is reduced.

When creating a \code{"zoo"} object with the function \code{zoo},
the vector of indexes \code{order.by} can be of (a single) arbitrary class
(if \code{x} is shorter or longer than \code{order.by} it is
expanded accordingly),
but it is essential that \code{ORDER(order.by)} works. For other
functions it is assumed that \code{c()}, \code{length()},
\code{MATCH()} and subsetting \code{[,} work. If this is not the case
for a particular index/date/time class, then methods for these 
generic functions should be created by the user. Note, that to achieve this,
new generic functions \code{\link{ORDER}} and \code{\link{MATCH}} are created in
the \code{zoo} package with default methods corresponding to
the non-generic base functions \code{\link[base]{order}} 
and \code{\link[base]{match}}. Note that the \code{\link[base]{order}} 
and hence the default \code{\link{ORDER}} typically work if there is a
\code{\link[base]{xtfrm}} method. Furthermore, for certain (but not for all)
operations the index class should have an \code{as.numeric} method (in 
particular for regular series) and an \code{as.character} method might improve
printed output (see also below).

The index observations \code{order.by} should typically be unique, such that
the observations can be totally ordered. Nevertheless, \code{zoo()} is able to create
\code{"zoo"} objects with duplicated indexes (with a warning) and simple methods such as \code{plot()}
or \code{summary()} will typically work for such objects. However, this is
not formally supported as the bulk of functionality provided in \pkg{zoo} requires
unique index observations/time stamps. See below for an example how to remove
duplicated indexes.

If a \code{frequency} is specified when creating a series via \code{zoo}, the
object returned is actually of class \code{"zooreg"} which inherits from \code{"zoo"}.
This is a subclass of \code{"zoo"} which relies on having a \code{"zoo"} series
with an additional \code{"frequency"} attribute (which has to comply with the
index of that series). Regular \code{"zooreg"} series can also be created by
\code{\link{zooreg}}, the \code{zoo} analogue of \code{\link{ts}}. See the 
respective help page and \code{\link{is.regular}} for further details.

Methods to standard generics for \code{"zoo"} objects currently
include: \code{print} (see above), \code{summary}, \code{str}, \code{head},
\code{tail}, \code{[} (subsetting), \code{rbind}, \code{cbind}, \code{merge}
(see \code{\link{merge.zoo}}), \code{aggregate} (see \code{\link{aggregate.zoo}}), \code{rev}, \code{split} (see \code{\link{aggregate.zoo}}), \code{barplot},
\code{plot} and \code{lines} (see \code{\link{plot.zoo}}). For multivariate
\code{"zoo"} series with column names the \code{$} extractor is available,
behaving similar as for \code{"data.frame"} objects.  Methods are also
available for \code{median} and \code{quantile}.

\code{ifelse.zoo} is not a method (because \code{ifelse} is not a generic) 
but must be written out including the \code{.zoo} suffix.

To \dQuote{prettify} printed output of \code{"zoo"} series the generic 
function \code{index2char} is used for turning index values into character
values. It defaults to using \code{as.character} but can be customized
if a different printed display should be used (although this should not
be necessary, usually).

The subsetting method \code{[} work essentially like the
corresponding functions for vectors or matrices respectively, i.e., takes
indexes of type \code{"numeric"}, \code{"integer"} or \code{"logical"}. But
additionally, it can be used to index with observations from the index class of
the series. If the index class of the series is one of the three classes above,
the corresponding index has to be encapsulated in \code{I()} to enforce usage of
the index class (see examples).   Subscripting by a zoo object whose
data contains logical values is undefined.

Additionally, \code{zoo} provides several generic functions and methods
to work (a) on the data contained in a \code{"zoo"} object, (b) the
index (or time) attribute associated to it, and (c) on both data and
index:

(a) The data contained in \code{"zoo"} objects can be extracted by
\code{coredata} (strips off all \code{"zoo"}-specific attributes) and modified
using \code{coredata<-}. Both are new generic functions with methods for
\code{"zoo"} objects, see \code{\link{coredata}}.

(b) The index associated with a \code{"zoo"} object can be extracted
by \code{index} and modified by \code{index<-}. As the interpretation
of the index as \dQuote{time} in time series applications is more natural,
there are also synonymous methods \code{time} and \code{time<-}. The
start and the end of the index/time vector can be queried by
\code{start} and \code{end}. See \code{\link{index}}.

(c) To work on both data and index/time, \code{zoo} provides methods
\code{lag}, \code{diff} (see \code{\link{lag.zoo}}) and \code{window}, 
\code{window<-} (see \code{\link{window.zoo}}).

In addition to standard group generic function (see \code{\link{Ops}}),
the following mathematical operations are available as methods for
\code{"zoo"} objects: transpose \code{t} which coerces to a matrix 
first, and \code{cumsum}, \code{cumprod}, \code{cummin}, \code{cummax}
which are applied column wise.

Coercion to and from \code{"zoo"} objects is available for objects of
various classes, in particular \code{"ts"}, \code{"irts"} and \code{"its"}
objects can be coerced to \code{"zoo"}, the reverse is available for
\code{"its"} and for \code{"irts"} (the latter in package \code{tseries}).
Furthermore, \code{"zoo"} objects can be coerced to vectors, matrices and
lists and data frames (dropping the index/time attribute). See \code{\link{as.zoo}}.

Several methods are available for \code{NA} handling in the data of 
\code{"zoo"} objects: 
\code{\link{na.aggregate}} which uses group means to fill in \code{NA}
values,
\code{\link{na.approx}} which uses linear interpolation to fill 
in \code{NA} values. 
\code{\link{na.contiguous}} which extracts the longest consecutive 
stretch of non-missing values in a \code{"zoo"} object,
\code{\link{na.fill}} which uses fixed specified values to replace \code{NA}
values,
\code{\link{na.locf}} which
replaces \code{NA}s by the last previous non-\code{NA}, 
\code{\link{na.omit}} which returns a \code{"zoo"}
object with incomplete observations removed,
\code{\link{na.spline}} which uses spline interpolation to fill 
in \code{NA} values and
\code{\link{na.StructTS}} which uses a seasonal Kalman filter to fill in
\code{NA} values,
\code{\link{na.trim}} which trims runs of \code{NA}s off the beginning and
end but not in the interior.  Yet another \code{NA} routine can be found in
the \code{stinepack} package where \code{\link[stinepack]{na.stinterp}}
performs Stineman interpolation.

A typical task to be performed on ordered observations is to evaluate some
function, e.g., computing the mean, in a window of observations that is moved
over the full sample period. The generic function \code{\link{rollapply}}
provides this functionality for arbitrary functions and more efficient versions
\code{\link{rollmean}}, \code{\link{rollmax}}, \code{\link{rollmedian}} are
available for the mean, maximum and median respectively.

The \pkg{zoo} package has an \code{as.Date} \code{numeric} method
which is similar to the one in the core of \R except that the
\code{origin} argument defaults to January 1, 1970 (whereas the one
in the core of \R has no default).

Note that since \code{zoo} uses date/time classes from base \R and 
other packages, it may inherit bugs or problems with those date/time classes.
Currently, there is one such known problem with the \code{c} method for
the \code{POSIXct} class in base \R:
If \code{x} and \code{y} are \code{POSIXct} objects with \code{tzone}
attributes, the attribute will always be dropped in \code{c(x, y)}, even
if it is the same across both \code{x} and \code{y}. Although this is documented
at \code{\link[base]{c.POSIXct}}, one may want to employ a workaround
as shown at \url{https://stat.ethz.ch/pipermail/r-devel/2010-August/058112.html}.
}

\references{
  Achim Zeileis and Gabor Grothendieck (2005).
  \pkg{zoo}: S3 Infrastructure for Regular and Irregular Time Series.
  \emph{Journal of Statistical Software}, \bold{14(6)}, 1-27.
  URL http://www.jstatsoft.org/v14/i06/ and available as 
  \code{vignette("zoo")}.
  
  Ajay Shah, Achim Zeileis and Gabor Grothendieck (2005).
  \pkg{zoo} Quick Reference.
  Package vignette available as \code{vignette("zoo-quickref")}.  
}

\value{
A vector or matrix with an \code{"index"} attribute of the same
dimension (\code{NROW(x)}) by which \code{x} is ordered.
}

\seealso{\code{\link{zooreg}}, \code{\link{plot.zoo}}, \code{\link{index}}, \code{\link{merge.zoo}}}

\examples{
## simple creation and plotting
x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
x <- zoo(rnorm(5), x.Date)
plot(x)
time(x)

## subsetting with numeric indexes
x[c(2, 4)]
## subsetting with index class
x[as.Date("2003-02-01") + c(2, 8)]

## different classes of indexes/times can be used, e.g. numeric vector
x <- zoo(rnorm(5), c(1, 3, 7, 9, 14))
## subsetting with numeric indexes then uses observation numbers
x[c(2, 4)]
## subsetting with index class can be enforced by I()
x[I(c(3, 9))]

## visualization
plot(x)
## or POSIXct
y.POSIXct <- ISOdatetime(2003, 02, c(1, 3, 7, 9, 14), 0, 0, 0)
y <- zoo(rnorm(5), y.POSIXct)
plot(y)

## create a constant series
z <- zoo(1, seq(4)[-2])

## create a 0-dimensional zoo series
z0 <- zoo(, 1:4)

## create a 2-dimensional zoo series
z2 <- zoo(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)

## create a factor zoo object
fz <- zoo(gl(2,5), as.Date("2004-01-01") + 0:9)

## create a zoo series with 0 columns
z20 <- zoo(matrix(nrow = 4, ncol = 0), 1:4)

## arithmetic on zoo objects intersects them first
x1 <- zoo(1:5, 1:5)
x2 <- zoo(2:6, 2:6)
10 * x1 + x2

## $ extractor for multivariate zoo series with column names
z <- zoo(cbind(foo = rnorm(5), bar = rnorm(5)))
z$foo
z$xyz <- zoo(rnorm(3), 2:4)
z

## add comments to a zoo object
comment(x1) <- c("This is a very simple example of a zoo object.",
  "It can be recreated using this R code: example(zoo)")
## comments are not output by default but are still there
x1
comment(x1)

# ifelse does not work with zoo but this works
# to create a zoo object which equals x1 at
# time i if x1[i] > x1[i-1] and 0 otherwise
(diff(x1) > 0) * x1

## zoo series with duplicated indexes
z3 <- zoo(1:8, c(1, 2, 2, 2, 3, 4, 5, 5))
plot(z3)
## remove duplicated indexes by averaging
lines(aggregate(z3, index(z3), mean), col = 2)
## or by using the last observation
lines(aggregate(z3, index(z3), tail, 1), col = 4)

## x1[x1 > 3] is not officially supported since
## x1 > 3 is of class "zoo", not "logical".
## Use one of these instead:
x1[which(x1 > 3)]
x1[coredata(x1 > 3)]
x1[as.logical(x1 > 3)]
subset(x1, x1 > 3)

## any class supporting the methods discussed can be used
## as an index class. Here are examples using complex numbers
## and letters as the time class.

z4 <- zoo(11:15, complex(real = c(1, 3, 4, 5, 6), imag = c(0, 1, 0, 0, 1)))
merge(z4, lag(z4))

z5 <- zoo(11:15, letters[1:5])
merge(z5, lag(z5))

# index values relative to 2001Q1
zz <- zooreg(cbind(a = 1:10, b = 11:20), start = as.yearqtr(2000), freq = 4)
zz[] <- mapply("/", as.data.frame(zz), coredata(zz[as.yearqtr("2001Q1")]))


## even though time index must be unique zoo (and read.zoo)
## will both allow creation of such illegal objects with
## a warning (rather than ana error) to give the user a 
## chance to fix them up.  Extracting and replacing times
## and aggregate.zoo will still work.
\dontrun{
# this gives a warning
# and then creates an illegal zoo object
z6 <- zoo(11:15, c(1, 1, 2, 2, 5))
z6

# fix it up by averaging duplicates
aggregate(z6, identity, mean)

# or, fix it up by taking last in each set of duplicates
aggregate(z6, identity, tail, 1)

# fix it up via interpolation of duplicate times
time(z6) <- na.approx(ifelse(duplicated(time(z6)), NA, time(z6)), na.rm = FALSE)
# if there is a run of equal times at end they
# wind up as NAs and we cannot have NA times
z6 <- z6[!is.na(time(z6))]
z6

x1. <- x1 <- zoo (matrix (1:12, nrow = 3), as.Date("2008-08-01") + 0:2)
colnames (x1) <- c ("A", "B", "C", "D")
x2 <- zoo (matrix (1:12, nrow = 3), as.Date("2008-08-01") + 1:3)
colnames (x2) <- c ("B", "C", "D", "E")

both.dates = as.Date (intersect (index (t1), index (t2)))
both.cols = intersect (colnames (t1), colnames (t2))

x1[both.dates, both.cols]
## there is "[.zoo" but no "[<-.zoo" however four of the following
## five examples work

## wrong
## x1[both.dates, both.cols] <- x2[both.dates, both.cols]

# 4 correct alternatives
# #1
window(x1, both.dates)[, both.cols] <- x2[both.dates, both.cols]

# #2. restore x1 and show a different way
x1 <- x1.
window(x1, both.dates)[, both.cols] <- window(x2, both.dates)[, both.cols]

# #3. restore x1 and show a different way
x1 <- x1.
x1[time(x1) %in% both.dates, both.cols] <- x2[both.dates, both.cols]

# #4. restore x1 and show a different way
x1 <- x1.
x1[time(x1) %in% both.dates, both.cols] <- x2[time(t2) %in% both.dates, both.cols]

}

}
\keyword{ts}

