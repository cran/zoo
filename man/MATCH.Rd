\name{MATCH}
\alias{MATCH}
\alias{MATCH.default}
\title{Value Matching}
\description{
\code{MATCH} is a generic function for value matching.
}

\usage{
MATCH(x, table, nomatch = NA, \dots)
}
\arguments{
  \item{x}{an object.}
  \item{table}{the values to be matched against.}
  \item{nomatch}{the value to be returned in the case when no match is
    found.  Note that it is coerced to \code{integer}.}
  \item{\dots}{further arguments to be passed to methods.}
}

\details{
\code{MATCH} is a new generic function which aims at providing
the functionality of the non-generic base function \code{\link[base]{match}}
for arbitrary objects. Currently, there is only a default method which
simply calls \code{\link[base]{match}}.
}


\seealso{\code{\link[base]{match}}}

\examples{
MATCH(1:5, 2:3)
}
\keyword{manip}
