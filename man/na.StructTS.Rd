\name{na.StructTS}
\alias{na.StructTS}
\alias{na.StructTS.zoo}
\alias{na.StructTS.ts}
\title{Fill NA or specified positions.}
\description{
Generic function for filling \code{NA} values using seasonal Kalman filter.}
\usage{
na.StructTS(object, \dots)
\method{na.StructTS}{ts}(object, ..., na.rm = FALSE, maxgap = Inf)
\method{na.StructTS}{zoo}(object, ..., na.rm = FALSE, maxgap = Inf)
}
\arguments{
  \item{object}{an object.}
  \item{\dots}{other arguments passed to methods.}
  \item{na.rm}{logical.  Whether to remove end portions or fill them with NA.}
  \item{maxgap}{Runs of more than \code{maxgap} 
\code{NA}s
are retained,
other \code{NA}s are removed and the last occurrence in the resulting series
prior to each time point in \code{xout} is used as that time point's
output value.
}
}

\details{Interpolate with seasonal Kalman filter.  The input object should
have a frequency. It is assumed the cycle length is 1.}

\seealso{\code{\link{na.approx}}}

\examples{

z <- zooreg(rep(10 * seq(4), each = 4) + rep(c(3, 1, 2, 4), times = 4), 
	start = as.yearqtr(2000), freq = 4)
z[10] <- NA

zout <- na.StructTS(z)

plot(cbind(z, zout), screen = 1, col = 1:2, type = c("l", "p"), pch = 20)


}
\keyword{ts}
