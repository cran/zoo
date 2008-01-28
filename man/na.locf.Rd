\name{na.locf}
\alias{na.locf}
\alias{na.locf.data.frame}
\alias{na.locf.list}
\alias{na.locf.default}
\title{Last Observation Carried Forward}
\description{
Generic function for replacing each \code{NA} with the most recent
non-\code{NA} prior to it.
}
\usage{
na.locf(object, na.rm = TRUE, \dots)
\method{na.locf}{default}(object, na.rm = TRUE, fromLast, rev, \dots)
}
\arguments{
  \item{object}{an object.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{fromLast}{logical. Causes observations to be carried backward rather
    than forward.  Default is \code{FALSE}.
    This corresponds to NOCB (next observation carried backward).}
  \item{rev}{Use fromLast instead.  This argument will
    be eliminated in the future in favor of fromLast.}
  \item{\dots}{further arguments passed to methods.}
}

\value{
An object in which each \code{NA} in the input object is replaced
by the most recent non-\code{NA} prior to it.  If there are no earlier non-\code{NA}s then 
the \code{NA} is omitted (if \code{na.rm = TRUE}) or it is not replaced (if \code{na.rm = FALSE}).  
}

\seealso{\code{\link{zoo}}}

\examples{
az <- zoo(1:6)

bz <- zoo(c(2,NA,1,4,5,2))
na.locf(bz)
na.locf(bz, fromLast = TRUE)

cz <- zoo(c(NA,9,3,2,3,2))
na.locf(cz)

# generate and fill in missing dates
# by merging with a zero width series having those dates
# and then applying na.locf
z <- zoo(c(0.007306621, 0.007659046, 0.007681013,
	0.007817548, 0.007847579, 0.007867313),
	as.Date(c("1993-01-01", "1993-01-09", "1993-01-16",
	"1993-01-23", "1993-01-30", "1993-02-06")))
dd <- seq(start(z), end(z), "day")
na.locf(merge(z, zoo(, dd)))

}
\keyword{ts}
