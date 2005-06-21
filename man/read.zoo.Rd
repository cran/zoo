\name{read.zoo}
\alias{read.zoo}

\title{Read zoo Series via read.table}
\description{
\code{read.zoo} is a convenience function for reading
\code{"zoo"} series from text files.
}
\usage{
read.zoo(file, format = "", tz = "", FUN = NULL, \dots)
}
\arguments{
  \item{file}{character giving the name of the file which the data
    are to be read from. See \code{\link{read.table}} for more information.}
  \item{format}{date format argument passed to \code{\link{as.Date.character}}.}
  \item{tz}{time zone argument passed to \code{\link{as.POSIXct}}.}
  \item{FUN}{a function for computing the index from the first column
    of the data. See details.}
  \item{\dots}{further arguments passed to \code{\link{read.table}}.}
}

\details{
\code{read.zoo} is a convenience function which should make it easier
to read data from a text file and turn it into a \code{"zoo"} series 
immediately. \code{read.zoo} reads the data file via \code{read.table(file, \dots)}.
The first column of the resulting data is interpreted to be the index, the
remaining columns the corresponding data. To assign the appropriate class
to the index, \code{FUN} can be specified and is applied to the first column.
By default, \code{read.zoo} uses \code{as.Date(as.character(x), format = format)} if \code{format} is 
specified and \code{as.POSIXct(as.character(x), tz = tz)} if \code{tz}
is specified; otherwise it tries to guess between \code{"numeric"},
\code{"Date"} and \code{"POSIXct"}.

If the resulting series has an underlying regularity, it is coerced to 
a \code{"zooreg"} series.
}

\value{
An object of class \code{"zoo"} (or \code{"zooreg"}).
}

\seealso{\code{\link{zoo}}}

\examples{
## not run
\dontrun{
## turn (numeric) first column into yearmon index
z <- read.zoo("foo.csv", sep = ",", FUN = as.yearmon)

## turn (character) first column into Date index
z <- read.zoo("foo.tab", format = "%m/%d/%Y")
}
}
\keyword{ts}

