\name{ORDER}
\alias{ORDER}
\alias{ORDER.default}
\title{Ordering Permutation}
\description{
\code{ORDER} is a generic function for computing ordering
permutations.
}

\usage{
ORDER(x, \dots)
\method{ORDER}{default}(x, \dots, na.last = TRUE, decreasing = FALSE)
}
\arguments{
  \item{x}{an object.}
  \item{\dots}{further arguments to be passed to methods.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.
  }
  \item{decreasing}{logical. Should the sort order be increasing or
    decreasing?}
}

\details{
\code{ORDER} is a new generic function which aims at providing
the functionality of the non-generic base function \code{\link[base]{order}}
for arbitrary objects. Currently, there is only a default method which
simply calls \code{\link[base]{order}}. For objects (more precisely if
\code{\link[base]{is.object}} is \code{TRUE}) \code{order}
leverages the generic \code{xtfrm}. Thus, to assure ordering
works, one can supply either a method to \code{xtfrm} or to \code{ORDER}
(or both).
}


\seealso{\code{\link[base]{order}}}

\examples{
ORDER(rnorm(5))
}
\keyword{manip}
