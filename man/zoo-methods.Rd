\name{zoo-methods}
\alias{is.zoo}
\alias{median.zoo}
\alias{quantile.zoo}
\alias{rev.zoo}
\alias{ifelse.zoo}
\alias{xtfrm.zoo}

\title{Fully Exported zoo Methods}
\description{
Various standard methods for \code{\link{zoo}} time series
that are fully exported and documented for historical reasons.
}
\usage{
is.zoo(object)

\method{median}{zoo}(x, na.rm = FALSE, \dots)
\method{quantile}{zoo}(x, \dots)
\method{ifelse}{zoo}(test, yes, no)
\method{rev}{zoo}(x)
\method{xtfrm}{zoo}(x)
}
\arguments{
  \item{object, x, test}{a \code{\link{zoo}} object.}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be
    stripped before the computation proceeds.}
  \item{yes, no}{return values for true and false elements of \code{test}, respectively.}
  \item{\dots}{further arguments passed to methods.}
}

\seealso{\code{\link{zoo}}}

\keyword{ts}
