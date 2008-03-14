\name{na.approx}
\alias{na.approx}
\alias{na.approx.default}
\alias{na.spline}
\alias{na.spline.default}
\title{Replace NA by Interpolation}
\description{
Generic functions for replacing each \code{NA} with interpolated
values.
}
\usage{
na.approx(object, \dots) 
\method{na.approx}{default}(object, along = index(object), na.rm = TRUE, \dots) 

na.spline(object, \dots) 
\method{na.spline}{default}(object, along = index(object), na.rm = TRUE, \dots) 
}
\arguments{
  \item{object}{object in which \code{NA}s are to be replaced}
  \item{along}{variable to use for interpolation. Has to be numeric, is
    otherwise coerced to numeric.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{\dots}{further arguments passed to methods.}
}

\details{
  Missing values (\code{NA}s) are replaced by linear interpolation via
  \code{\link{approx}} or cubic spline interpolation via \code{\link{spline}},
  respectively.  

  By default the index associated with \code{object} is used
  for interpolation. Note, that if this calls \code{index.default}
  this gives an equidistant spacing \code{1:NROW(object)}. If \code{object}
  is a matrix or data.frame, the interpolation is done separately for
  each column.
}

\value{
An object in which each \code{NA} in the input object is replaced
by interpolating the non-\code{NA} values before and after it.
Leading \code{NA}s are omitted (if \code{na.rm = TRUE}) or not replaced (if \code{na.rm = FALSE}). 
}

\seealso{\code{\link{zoo}}, \code{\link{approx}}, \code{\link{na.contiguous}}, \code{link{na.locf}}, \code{\link{na.omit}}, \code{\link{na.trim}}, \code{\link{spline}}, \code{\link[stinepack]{stinterp}}} 

\examples{

z <- zoo(c(2,NA,1,4,5,2), c(1,3,4,6,7,8))

## use underlying time scale for interpolation
na.approx(z) 
## use equidistant spacing
na.approx(z, 1:6)

# with and without na.rm = FALSE
zz <- c(NA,9,3,NA,3,2)
na.approx(zz, na.rm = FALSE)
na.approx(zz)

}
\keyword{ts}
