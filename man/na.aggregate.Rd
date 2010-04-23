\name{na.aggregate}
\alias{na.aggregate}
\alias{na.aggregate.default}
\title{Replace NA by Aggregation}
\description{
Generic function for replacing each \code{NA} with aggregated
values. This allows imputing by the overall mean, by monthly means,
etc. 
}
\usage{
na.aggregate(object, \dots)
\method{na.aggregate}{default}(object, by = 1, \dots, FUN = mean,
             na.rm = FALSE, maxgap = Inf)

}
\arguments{
  \item{object}{an object.}
  \item{by}{a grouping variable corresponding to \code{object}, or a
    function to be applied to \code{time(object)} to generate the
    groups.}
  \item{\dots}{further arguments passed to \code{by} if \code{by} is a
    function.}
  \item{FUN}{function to apply to the non-missing values in each group
    defined by \code{by}.} 
  \item{na.rm}{logical. Should any remaining \code{NA}s be removed?}
  \item{maxgap}{maximum number of consecutive \code{NA}s to
    fill. Any longer gaps will be left unchanged.}
}

\value{
An object in which each \code{NA} in the input object is replaced
by the mean (or other function) of its group, defined by
\code{by}. This is done for each series in a multi-column object. Common
choices for the aggregation group are a year, a month, all calendar
months, etc.  

If a group has no non-missing values, the default aggregation function
\code{mean} will return \code{NaN}. Specify \code{na.rm = TRUE} to
omit such remaining missing values.
}

\seealso{\code{\link{zoo}}}

\examples{
z <- zoo(c(1, NA, 3:9),
         c(as.Date("2010-01-01") + 0:2,
           as.Date("2010-02-01") + 0:2,
           as.Date("2011-01-01") + 0:2))
## overall mean
na.aggregate(z)
## group by months
na.aggregate(z, as.yearmon)
## group by calendar months
na.aggregate(z, months)
## group by years
na.aggregate(z, format, "\%Y")
}
\keyword{ts}
