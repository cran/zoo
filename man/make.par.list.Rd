\name{make.par.list}
\alias{make.par.list}

\title{Make a List from a Parameter Specification}

\description{
  Process parameters so that a list of parameter
  specifications is returned (used by \code{plot.zoo} and
  \code{xyplot.zoo}).
}

\usage{
make.par.list(nams, x, n, m, def, recycle = sum(unnamed) > 0)
}

\arguments{
  \item{nams}{character vector with names of variables.}
  \item{x}{list or vector of parameter specifications, see details.}
  \item{n}{numeric, number of rows.}
  \item{m}{numeric, number of columns. (Only determines whether \code{m}
    is 1 or greater than 1.}
  \item{def}{default parameter value.}
  \item{recycle}{logical. If \code{TRUE} recycle columns to provide
    unspecified ones. If \code{FALSE} use \code{def} to provide unspecified
    ones. This only applies to entire columns. Within columns recycling is
    always done regardless of how \code{recycle} is set. Defaults to
    \code{TRUE} if there is at least one unnamed variable and defaults to
    \code{FALSE} if there are only named variables in \code{x}.}
}

\details{
  This function is currently intended for internal use.  It is currently 
  used by 
  \code{plot.zoo} and \code{xyplot.zoo} but might also be used in the future
  to create additional new plotting routines.
  It creates a new list which uses the named variables from \code{x}
  and then assigns the unnamed in order.  For the remaining variables
  assign them the default value if \code{!recycle} or recycle the
  unnamed variables if \code{recycle}.
}

\value{
  A list of parameters, see details.
}

\examples{
make.par.list(letters[1:5], 1:5, 3, 5)
suppressWarnings( make.par.list(letters[1:5], 1:4, 3, 5, 99) )
make.par.list(letters[1:5], c(d=3), 3, 5, 99)
make.par.list(letters[1:5], list(d=1:2, 99), 3, 5)
make.par.list(letters[1:5], list(d=1:2, 99, 100), 3, 5)
}

\keyword{ts}
