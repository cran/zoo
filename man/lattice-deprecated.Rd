\name{lattice-deprecated}
\alias{panel.plot.default}
\alias{panel.plot.custom}
\alias{panel.lines.zoo}
\alias{panel.lines.ts}
\alias{panel.lines.its}
\alias{panel.lines.tis}
\alias{panel.points.zoo}
\alias{panel.points.ts}
\alias{panel.points.its}
\alias{panel.points.tis}
\alias{panel.text.zoo}
\alias{panel.text.ts}
\alias{panel.text.its}
\alias{panel.text.tis}
\alias{panel.segments.ts}
\alias{panel.segments.its}
\alias{panel.segments.tis}
\alias{panel.rect.ts}
\alias{panel.rect.its}
\alias{panel.rect.tis}
\alias{panel.polygon.ts}
\alias{panel.polygon.its}
\alias{panel.polygon.tis}

\title{Deprecated Time Series Panel Methods for Lattice}

\description{
Deprecated panel methods for time series objects (of class \code{"zoo"},
\code{"ts"}, \code{"its"}, or \code{"tis"}). 
}

\usage{
panel.plot.default(x, y, subscripts, groups, panel = panel.xyplot,
  col = 1, type = "p", pch = 20, lty = 1, lwd = 1, \dots)

panel.plot.custom(\dots)

panel.lines.zoo(x, \dots)
panel.lines.ts(x, \dots)
panel.lines.its(x, \dots)
panel.lines.tis(x, \dots)

panel.points.zoo(x, \dots)
panel.points.ts(x, \dots)
panel.points.its(x, \dots)
panel.points.tis(x, \dots)

panel.text.zoo(x, \dots)
panel.text.ts(x, \dots)
panel.text.its(x, \dots)
panel.text.tis(x, \dots)

panel.segments.ts(x0, x1, \dots)
panel.segments.its(x0, x1, \dots)
panel.segments.tis(x0, x1, \dots)

panel.rect.ts(x0, x1, \dots)
panel.rect.its(x0, x1, \dots)
panel.rect.tis(x0, x1, \dots)

panel.polygon.ts(x, \dots)
panel.polygon.its(x, \dots)
panel.polygon.tis(x, \dots)
}

\arguments{
  \item{x, x0, x1}{time series object of class \code{"zoo"}, \code{"ts"}, \code{"its"} or 
  \code{"tis"}.  For \code{panel.plot.default} it should be a numeric
  vector.}
  \item{y}{numeric vector or matrix.}
  \item{subscripts, groups, panel, col, type, pch, lty, lwd}{lattice graphical arguments.}
  \item{\dots}{arguments passed on to and from methods.}
}

\details{
  Since \pkg{zoo} 1.6-3 \code{plot.panel.default} and
  \code{plot.panel.custom} are no longer necessary, as normal panel
  functions (\code{\link[lattice]{panel.xyplot}} by default) will work.

  Similarly, there are now methods for the generic \pkg{lattice} drawing
  functions \code{\link[lattice]{llines}},
  \code{\link[lattice:llines]{lpoints}}, and
  \code{\link[lattice:llines]{ltext}}. These can also be called as 
  \code{panel.lines}, \code{panel.points}, and \code{panel.text},
  respectively. The old interfaces (\code{panel.lines.zoo},
  \code{panel.points.zoo}, and \code{panel.text.zoo}), will be
  removed in future versions. \code{panel.polygon.zoo} may also be
  removed. 
}

\seealso{\code{\link{xyplot.zoo}}}

\keyword{hplot}
\keyword{ts}
