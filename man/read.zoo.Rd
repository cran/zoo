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
read.zoo(file, format = "", tz = "", FUN = NULL, regular = FALSE, index.column = 1, \dots)
write.zoo(x, file = "", index.name = "Index", row.names = FALSE, col.names = NULL, \dots)
}
\arguments{
  \item{file}{character giving the name of the file which the data
    are to be read from/written to. See \code{\link{read.table}} and
    \code{\link{write.table}} for more information.}
  \item{format}{date format argument passed to \code{\link{as.Date.character}}.}
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
By default, \code{read.zoo} uses \code{as.Date(as.character(x), format = format)} if \code{format} is 
specified and \code{as.POSIXct(as.character(x), tz = tz)} if \code{tz}
is specified; otherwise it tries to guess between \code{"numeric"},
\code{"Date"} and \code{"POSIXct"}.

If \code{regular} is set to \code{TRUE} and the resulting series has an underlying
regularity, it is coerced to a \code{"zooreg"} series.

\code{write.zoo} is a convenience function for writing \code{"zoo"} series
to text files. It first coerces its argument to a \code{"data.frame"}, adds
a column with the index and then calls \code{\link{write.table}}.
}

\value{
\code{read.zoo} returns an object of class \code{"zoo"} (or \code{"zooreg"}).
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
f <- function(x) as.yearmon(format(x, nsmall = 2), "%Y.%m")
z <- read.zoo("foo.csv", header = TRUE, FUN = f)

## turn *character* first column into Date index
z <- read.zoo("foo.tab", format = "\%m/\%d/\%Y")
}
}
\keyword{ts}

