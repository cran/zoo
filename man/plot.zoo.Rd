\name{plot.zoo}
\alias{plot.zoo}
\alias{barplot.zoo}
\alias{lines.zoo}
\title{Plotting zoo Objects}
\description{
Plotting method for objects of class \code{"zoo"}.
}
\usage{
\method{plot}{zoo}(x, y = NULL, screens = 1, plot.type = c("multiple", "single"),
  panel = lines, xlab = "Index", ylab = NULL, main = NULL,
  xlim = NULL, ylim = NULL, xy.labels = FALSE, xy.lines = NULL,
  oma = c(6, 0, 5, 0), mar = c(0, 5.1, 0, 2.1), 
  col = 1, lty = 1, pch = 1, type = "l", nc, widths = 1, heights = 1, \dots)
\method{lines}{zoo}(x, type = "l", \dots)
}
\arguments{
  \item{x}{an object of class \code{"zoo"}.}
  \item{y}{an object of class \code{"zoo"}. If \code{y} is \code{NULL}
    (the default) a time series plot of \code{x} is produced, otherwise
    if both \code{x} and \code{y} are univariate \code{"zoo"} series, a
    scatter plot of \code{y} versus \code{x} is produced.}
  \item{screens}{factor (or coerced to factor) whose levels specify which
    graph each series is to be plotted in.  \code{screens=c(1,2,1)}
    would plot series 1, 2 and 3 in graphs 1, 2 and 1.}
  \item{plot.type}{for multivariate zoo objects, "multiple" plots the
    series on multiple plots and "single" superimposes them on a single
    plot}
  \item{panel}{a \code{function(x, y, col, lty, \dots)}  which gives the
    action to be carried out in each panel of the display for
   \code{plot.type = "multiple"}.}
  \item{ylim}{if \code{plot.type = "multiple"} then it can be a list of
    y axis limits.  If not a list each graph has the same limits.
    If any list element is not a pair then its range is used instead. If 
    \code{plot.type = "single"} then it is as in \code{plot}.}
  \item{xy.labels}{logical, indicating if \code{\link{text}} labels should be
    used in the scatter plot, or character, supplying a vector of labels to be used.}
  \item{xy.lines}{logical, indicating if \code{\link{lines}} should be drawn in
    the scatter plot. Defaults to the value of \code{xy.labels} if that is
    logical, otherwise to \code{FALSE}.}
  \item{xlab, ylab, main, xlim, oma, mar}{graphical arguments, see \code{\link{par}}.}
  \item{col, lty, pch, type}{graphical arguments that can be vectors or 
    (named) lists. See the details for more information.}
  \item{nc}{the number of columns to use when \code{plot.type = "multiple"}.
    Defaults to \code{1} for up to \code{4} series, otherwise to \code{2}.}
  \item{widths, heights}{widths and heights for individual graphs, see
    \code{\link{layout}}.}
  \item{\dots}{additional graphical arguments.}
}

\details{
The methods for \code{plot} and \code{lines} are very similar
to the corresponding \code{ts} methods. However, the handling of
graphical parameters \code{col}, \code{pch} and \code{lty} is more
flexible for multivariate series. These parameters can be
vectors of the same length as the number of series plotted or
are recycled if shorter. They can also be (partially) named list, e.g.,
\code{list(A = c(1,2), c(3,4))} in which \code{c(3, 4)} is the
default value and \code{c(1, 2)} the value only  for series \code{A}.
The \code{screens} argument can be specified in a similar way.
If \code{plot.type} and \code{screens} conflict then multiple plots
will be assumed. Also see the examples.

In addition to classical time series line plots, there is also a
simple \code{\link{barplot}} method for \code{"zoo"} series.
}

\seealso{\code{\link{zoo}}, \code{\link{plot.ts}}, \code{\link{barplot}}}

\examples{
x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))

## univariate plotting
x <- zoo(rnorm(5), x.Date)
x2 <- zoo(rnorm(5, sd = 0.2), x.Date)
plot(x)
lines(x2, col = 2)

## multivariate plotting
z <- cbind(x, x2, zoo(rnorm(5, sd = 0.5), x.Date))
colnames(z) <- LETTERS[1:3]
plot(z, plot.type = "single", col = list(B = 2))
plot(z, type = "b", pch = 1:3, col = 1:3)
plot(z, type = "b", pch = list(A = 1:5, B = 3), col = list(C = 4, 2))
plot(z, type = "b", screen = c(1,2,1), col = 1:3)

## plot one zoo series against the other.
plot(x, x2)
plot(x, x2, xy.labels = TRUE)
plot(x, x2, xy.labels = 1:5, xy.lines = FALSE)

## barplot
x <- zoo(cbind(rpois(5, 2), rpois(5, 3)), x.Date)
barplot(x, beside = TRUE)
}
\keyword{ts}
