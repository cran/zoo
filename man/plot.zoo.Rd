\name{plot.zoo}
\alias{plot.zoo}
\alias{lines.zoo}
\title{Plotting zoo Objects}
\description{
Plotting method for objects of class \code{"zoo"}.
}
\usage{
\method{plot}{zoo}(x, plot.type = c("multiple", "single"),
  panel = lines, xlab = "Index", ylab = NULL, main = NULL,
  ylim = NULL, oma = c(6, 0, 5, 0), col = 1, lty = 1, nc,
  \dots)
\method{lines}{zoo}(x, type = "l", \dots)
}
\arguments{
  \item{x}{an object of class \code{"zoo"}.}
  \item{plot.type}{for multivariate zoo objects, should the observations
     be plotted separately (with a common index axis) or on a single plot?}
  \item{panel}{a 'function(x, y, col, lty, \dots)'  which gives the
     action to be carried out in each panel of the display for
    'plot.type="multiple"'.  The default is 'lines'.}
  \item{xlab, ylab, main, ylim, oma, type}{graphical arguments, see \code{\link{par}}.}
  \item{col, lty}{graphical arguments that can be vectorized for
     'plot.type = "multiple"'.}
  \item{nc}{the number of columns to use when 'plot.type="multiple"'.
    Defaults to 1 for up to 4 series, otherwise to 2.}
  \item{\dots}{additional graphical arguments.}
}

\details{
The methods for \code{plot} and \code{lines} are very similar
to the corresponding \code{ts} methods.
}

\seealso{\code{\link{zoo}}, \code{\link{plot.ts}}}

\examples{
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)
x2 <- zoo(rnorm(5, sd = 0.2), x.date)
plot(x)
lines(x2, col = 2)
}
\keyword{ts}
