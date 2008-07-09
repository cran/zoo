\name{read.zoo}
\alias{read.zoo}
\alias{write.zoo}

\title{Reading and Writing zoo Series}
\description{
\code{read.zoo} and \code{write.zoo} are convenience functions for reading
and writing \code{"zoo"} series from/to text files. They are convenience
interfaces to \code{read.table} and \code{write.table}, respectively.
}
\usage{
read.zoo(file, format = "", tz = "", FUN = NULL,
  regular = FALSE, index.column = 1, aggregate = FALSE, \dots)
write.zoo(x, file = "", index.name = "Index", row.names = FALSE, col.names = NULL, \dots)
}
\arguments{
  \item{file}{character giving the name of the file which the data
    are to be read from/written to. See \code{\link{read.table}} and
    \code{\link{write.table}} for more information.}
  \item{format}{date format argument passed to \code{FUN}.}
  \item{tz}{time zone argument passed to \code{\link{as.POSIXct}}.}
  \item{FUN}{a function for computing the index from the first column
    of the data. See details.}
  \item{regular}{logical. Should the series be coerced to class \code{"zooreg"}
    (if the series is regular)?}
  \item{index.column}{integer. The column of the data frame in which the index/time
    is stored.}
  \item{x}{a \code{"zoo"} object.}
  \item{index.name}{character with name of the index column in the written
    data file.}  
  \item{row.names}{logical. Should row names be written? Default is \code{FALSE}
    because the row names are just character representations of the index.}
  \item{col.names}{logical. Should column names be written? Default is to
    write column names only if \code{x} has column names.}
  \item{aggregate}{logical or function. If set to \code{TRUE}, then \code{\link{aggregate.zoo}}
    is applied to the zoo object created to compute the \code{\link{mean}} of all values with
    the same time index. Alternatively, \code{aggregate} can be set to any other
    function that should be used for aggregation.
    If \code{FALSE} (the default), no aggregation is performed and a warning
    is given if there are any duplicated time indexes.  Note that most
    \code{zoo} functions do not accept objects with duplicate time indexes. 
    See \code{\link{aggregate.zoo}}.}
  \item{\dots}{further arguments passed to \code{\link{read.table}} or
    \code{\link{write.table}}, respectively.}
}

\details{
\code{read.zoo} is a convenience function which should make it easier
to read data from a text file and turn it into a \code{"zoo"} series 
immediately. \code{read.zoo} reads the data file via \code{read.table(file, \dots)}.
The column \code{index.column} (by default the first) of the resulting data is
interpreted to be the index/time, the remaining columns the corresponding data.
(If the file only has only column then that is assumed to be the data column and
\code{1, 2, ...} are used for the index.)  To assign the appropriate class
to the index, \code{FUN} can be specified and is applied to the first column.

To process the index, \code{read.zoo} uses the first of the following that is
true:  1. If \code{FUN} is specified then \code{read.zoo} calls \code{FUN} with
the index as the first argument.  2. If \code{tz} is specified then the index 
column is converted to \code{POSIXct}.  3. If \code{format} is specified
then the index column is converted to \code{Date}.  4. A heuristic
attempts to decide among \code{"numeric"}, \code{"Date"} and \code{"POSIXct"}.
If \code{format} and/or \code{tz} is specified 
then it is passed to the conversion function as well.


If \code{regular} is set to \code{TRUE} and the resulting series has an 
underlying regularity, it is coerced to a \code{"zooreg"} series.

\code{write.zoo} is a convenience function for writing \code{"zoo"} series
to text files. It first coerces its argument to a \code{"data.frame"}, adds
a column with the index and then calls \code{\link{write.table}}.
}

\value{
\code{read.zoo} returns an object of class \code{"zoo"} (or \code{"zooreg"}).
}
\note{\code{read.zoo} works by first reading the data in using \code{read.table}
and then processing it.  This implies that 
if the index field is entirely numeric the default is to pass it to \code{FUN}
or the builtin date conversion routine
a number, rather than a character string. 
Thus a date field such as code{09122007} intended
to represent December 12, 2007 would be seen as \code{9122007}
and interpreted as the 91st day 
thereby generating an error.  

This comment also applies to trailing decimals so that if 
\code{2000.10} were intended to represent the 10th month of 2000 in fact
it would receive
\code{2000.1} and regard it as the first month of 2000
unless similar precautions were taken.

In the above cases the index field should be specified to be
\code{"character"} so that leading or trailing zeros
are not dropped.  This can be done by specifying a \code{"character"}
index column in the 
\code{"colClasses"} argument, which is passed to \code{read.table}, 
as shown in the examples below.
}

\seealso{\code{\link{zoo}}}

\examples{
\dontrun{
## turn *numeric* first column into yearmon index
## where number is year + fraction of year represented by month
z <- read.zoo("foo.csv", sep = ",", FUN = as.yearmon)

## first column is of form yyyy.mm
## (Here we use format in place of as.character so that final zero 
## is not dropped in dates like 2001.10 which as.character would do.)
f <- function(x) as.yearmon(format(x, nsmall = 2), "\%Y.\%m")
z <- read.zoo("foo.csv", header = TRUE, FUN = f)

## turn *character* first column into "Date" index
## Assume lines look like: 12/22/2007 1 2
z <- read.zoo("foo.tab", format = "\%m/\%d/\%Y")

# ensure that first column is interpreted as character so leading 0 not dropped
# Suppose links look like: 09112007 1 2 and there is no header
z <- read.zoo("foo.txt", format = "\%d\%m\%Y", colClasses = c(V1 = "character"))

## csv file with first column of form YYYY-mm-dd HH:MM:SS
## Read in times as "chron" class. Requires chron 2.3-22 or later.
z <- read.zoo("foo.csv", header = TRUE, sep = ",", FUN = as.chron)

## same file format but read it in times as "POSIXct" class.
z <- read.zoo("foo.csv", header = TRUE, sep = ",", tz = "")

## csv file with first column mm-dd-yyyy. Read times as "Date" class.
z <- read.zoo("foo.csv", header = TRUE, sep = ",", format = "\%m-\%d-\%Y")

## whitespace separated file with first column of form YYYY-mm-ddTHH:MM:SS
## and no headers.  T appears literally.  Requires chron 2.3-22 or later.
z <- read.zoo("foo.csv", FUN = as.chron)
 
}
}
\keyword{ts}

