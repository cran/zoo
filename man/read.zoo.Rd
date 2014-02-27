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
  regular = FALSE, index.column = 1, drop = TRUE, FUN2 = NULL,
  split = NULL, aggregate = FALSE, \dots, text)
write.zoo(x, file = "", index.name = "Index", row.names = FALSE, col.names = NULL, \dots)
}
\arguments{
  \item{file}{character string or strings giving the name of the file(s) 
    which the data
    are to be read from/written to. See \code{\link{read.table}} and
    \code{\link{write.table}} for more information. Alternatively,
    in \code{read.zoo}, \code{file} can be a \code{connection} or a
	\code{data.frame} (e.g.,
    resulting from a previous \code{read.table} call) that
    is subsequently processed to a \code{"zoo"} series.}
  \item{format}{date format argument passed to \code{FUN}.}
  \item{tz}{time zone argument passed to \code{\link{as.POSIXct}}.}
  \item{FUN}{a function for computing the index from the first column
    of the data. See details.}
  \item{regular}{logical. Should the series be coerced to class \code{"zooreg"}
    (if the series is regular)?}
  \item{index.column}{numeric vector or list. The column names or numbers of the data frame 
    in which the index/time is stored.  If the \code{read.table} \code{colClasses} 
    argument is used and \code{"NULL"} is among its componennts then 
    \code{index.column} refers to the column numbers after the columns 
    corresponding to \code{"NULL"} in \code{colClasses} have been removed.  
    If specified as a list then one argument will be passed to argument \code{FUN} per component so that, 
    for example, \code{index.column = list(1, 2)} will cause 
    \code{FUN(x[,1], x[,2], ...)} to be called whereas 
    \code{index.column = list(1:2)} will cause 
    \code{FUN(x[,1:2], ...)} to be called where \code{x} is a data frame of 
    characters data.  Here \code{...} refers to \code{format}
    and/or \code{tz}, if they specified as arguments.  \code{index.column = 0} can
    be used to specify that the row names be used as the index.  In the case that
    no row names were input sequential numbering is used.
    If \code{index.column} is specified as an ordinary vector then if it has the
    same length as the number of arguments of \code{FUN} (or \code{FUN2} in the
    event that \code{FUN2} is specified and \code{FUN} is not) 
    then \code{index.column} is converted to a
    list.  Also it is always converted to a list if it has length 1.}
  \item{drop}{logical. If the data frame contains just a single data column, should
    the second dimension be dropped?}
  \item{x}{a \code{"zoo"} object.}
  \item{index.name}{character with name of the index column in the written
    data file.}  
  \item{row.names}{logical. Should row names be written? Default is \code{FALSE}
    because the row names are just character representations of the index.}
  \item{col.names}{logical. Should column names be written? Default is to
    write column names only if \code{x} has column names.}
  \item{FUN2}{function. It is applied to the time index after 
    \code{FUN} and before \code{aggregate}. If \code{FUN} is not specified
    but \code{FUN2} is specified then only \code{FUN2} is applied.}
  \item{split}{NULL or column number or name or vector of numbers or 
    names. If not NULL then the data is assumed to be in long format and is 
    split according to the indicated columns.  See the \R 
    \code{\link[stats]{reshape}} command for description of long data.
    If \code{split = Inf} then the first of each run among the times are made into
    a separate series, the second of each run and so on.  If \code{split= -Inf} then
    the last of each run is made into a separate series, the second last
    and so on.}
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
  \item{text}{character. If \code{file} is not supplied and this is, then
    data are read from the value of \code{text} via a text connection.
    See below for an example.}
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

To process the index, \code{read.zoo} calls \code{FUN} with the index as the
first argument.  If \code{FUN} is not specified then if there are multiple
index columns they are pasted together with a space between each.  Using the
index column or pasted index column: 1. If \code{tz} is specified then the
index column is converted to \code{POSIXct}.  2. If \code{format} is specified
then the index column is converted to \code{Date}.  3. Otherwise, a heuristic
attempts to decide among \code{"numeric"}, \code{"Date"} and \code{"POSIXct"}.
If \code{format} and/or \code{tz} is specified then they are passed to the
conversion function as well.

If \code{regular} is set to \code{TRUE} and the resulting series has an 
underlying regularity, it is coerced to a \code{"zooreg"} series.

\code{write.zoo} is a convenience function for writing \code{"zoo"} series
to text files. It first coerces its argument to a \code{"data.frame"}, adds
a column with the index and then calls \code{\link{write.table}}.

See also \code{vignette("zoo-read", package = "zoo")} for detailed examples.
}

\value{
\code{read.zoo} returns an object of class \code{"zoo"} (or \code{"zooreg"}).
}
\note{\code{read.zoo} works by first reading the data in using \code{read.table}
and then processing it.  This implies that 
if the index field is entirely numeric the default is to pass it to \code{FUN}
or the built-in date conversion routine
a number, rather than a character string. 
Thus, a date field such as \code{09122007} intended
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
## See also vignette("zoo-read", package = "zoo") for more examples

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

# Suppose lines look like: 09112007 1 2 and there is no header
z <- read.zoo("foo.txt", format = "\%d\%m\%Y")

## csv file with first column of form YYYY-mm-dd HH:MM:SS
## Read in times as "chron" class. Requires chron 2.3-22 or later.
z <- read.zoo("foo.csv", header = TRUE, sep = ",", FUN = as.chron)

## same but with custom format.  Note as.chron uses POSIXt-style % formats
## Read in times as "chron" class. Requires chron 2.3-24 or later.
z <- read.zoo("foo.csv", header = TRUE, sep = ",", FUN = as.chron, 
	format = "%Y%m%d")

## same file format but read it in times as "POSIXct" class.
z <- read.zoo("foo.csv", header = TRUE, sep = ",", tz = "")

## csv file with first column mm-dd-yyyy. Read times as "Date" class.
z <- read.zoo("foo.csv", header = TRUE, sep = ",", format = "\%m-\%d-\%Y")

## whitespace separated file with first column of form YYYY-mm-ddTHH:MM:SS
## and no headers.  T appears literally.  Requires chron 2.3-22 or later.
z <- read.zoo("foo.csv", FUN = as.chron)

# We use "NULL" in colClasses for those columns we don't need but in 
# col.names we still have to include dummy names for them.  Of what 
# is left the index is the first three columns (1:3) which we convert 
# to chron class times in FUN and then truncate to 5 seconds in FUN2.  
# Finally we use aggregate = mean to average over the 5 second intervals.
library("chron")

Lines <- "CVX 20070201 9 30 51 73.25 81400 0
CVX 20070201 9 30 51 73.25 100 0
CVX 20070201 9 30 51 73.25 100 0
CVX 20070201 9 30 51 73.25 300 0
CVX 20070201 9 30 51 73.25 81400 0
CVX 20070201 9 40 51 73.25 100 0
CVX 20070201 9 40 52 73.25 100 0
CVX 20070201 9 40 53 73.25 300 0"

z <- read.zoo(text = Lines, 
  colClasses = c("NULL", "NULL", "numeric", "numeric", "numeric",
    "numeric", "numeric", "NULL"),
  col.names = c("Symbol", "Date", "Hour", "Minute", "Second", "Price", "Volume", "junk"),
  index = 1:3,  # do not count columns that are "NULL" in colClasses
  FUN = function(h, m, s) times(paste(h, m, s, sep = ":")),
  FUN2 = function(tt) trunc(tt, "00:00:05"),
  aggregate = mean)

## omit the read.table() phase and directly supply a data.frame
dat <- data.frame(date = paste("2000-01-", 10:15, sep = ""), a = rnorm(6), b = 1:6)
z <- read.zoo(dat)

## using built-in data frame BOD
read.zoo(BOD)

read.zoo(BOD, FUN = as.Date)

read.zoo(BOD[c(1:6, 1), ], aggregate = mean)

# read in all csv files in the current directory and merge them
read.zoo(Sys.glob("*.csv"), header = TRUE, sep = ",")

}
}

\keyword{ts}
