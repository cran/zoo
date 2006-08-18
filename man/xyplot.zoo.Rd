\name{xyplot.zoo}
\alias{xyplot.zoo}
\alias{xyplot.ts}
\alias{xyplot.its}
\alias{panel.lines.zoo}
\alias{panel.lines.ts}
\alias{panel.lines.its}
\alias{panel.points.zoo}
\alias{panel.points.ts}
\alias{panel.points.its}
\alias{panel.segments.zoo}
\alias{panel.segments.ts}
\alias{panel.segments.its}
\alias{panel.text.zoo}
\alias{panel.text.ts}
\alias{panel.text.its}
\alias{panel.rect.zoo}
\alias{panel.rect.ts}
\alias{panel.rect.its}
\alias{panel.arrows.zoo}
\alias{panel.arrows.ts}
\alias{panel.arrows.its}
\alias{panel.polygon.zoo}
\alias{panel.polygon.ts}
\alias{panel.polygon.its}
\alias{panel.plot.default}
\alias{panel.plot.custom}

\title{Plot zoo Series with Lattice}

\description{
\code{xyplot} methods for time series objects (of class \code{"zoo"}
or \code{"ts"}, \code{"its"}). These functions are still under development
and the interface and functionality might be modified/extended in future
releases.
}

\usage{

\method{xyplot}{zoo}(x, data, screens = seq(length = NCOL(x)), 
    default.scales = list(y = list(relation = "free")), 
    layout = NULL, xlab = "Index", ylab = NULL, 
    lty = trellis.par.get("plot.line")$lty,
    lwd = trellis.par.get("plot.line")$lwd,
    pch = trellis.par.get("plot.symbol")$pch, 
    type = "l", 
    col = trellis.par.get("plot.line")$col,
    strip = TRUE, panel = panel.plot.default, ...) 

panel.plot.default(x, y, subscripts, groups, 
    panel = panel.xyplot,
    col = 1, type = "p", pch = 20, lty = 1, lwd = 1, ...) 

panel.plot.custom(...) 

panel.lines.zoo(x, ...)
panel.points.zoo(x, ...)
panel.segments.zoo(x0, x1, ...) 
panel.text.zoo(x, ...)
panel.rect.zoo(x0, x1, ...)
panel.arrows.zoo(x0, x1, ...)
panel.polygon.zoo(x, ...)

}
\arguments{
  \item{x, x0, x1}{time series object of class \code{"zoo"}, \code{"ts"} or 
  \code{"its"}.  For \code{panel.plot.default} it should be a numeric
  vector.}
  \item{y}{numeric vector or matrix.}
  \item{subscripts, groups, panel}{arguments for panel functions, see
   description of \code{panel} argument in \code{\link[lattice]{xyplot}}.}
  \item{data}{currently not used.}
  \item{screens}{factor (or coerced to factor) whose levels specify which
    graph each series is to be plotted in.  \code{screens = c(1, 2, 1)}
    would plot series 1, 2 and 3 in graphs 1, 2 and 1.}
  \item{default.scales}{\code{scales} specification. The default is set so that all
    series have the \code{"same"} X axis but \code{"free"} Y axis. 
    See \code{\link[lattice]{xyplot}} in the \pkg{lattice} package for more
    information on \code{scales}. For users, it is recommended to set the \code{scales}
    argument instead of \code{default.scales}.}
  \item{layout}{numeric vector of length 2 specifying number of columns
    and rows in the plot, see \code{\link[lattice]{xyplot}} for more details.
    The default is to fill columns with up to 5 rows.}
  \item{xlab}{character string used as the X axis label.}
  \item{ylab}{character string used as the Y axis label. If there
    are multiple panels it may be a character vector the same length
    as the number of panels.}
  \item{lty, lwd, pch, type, col}{graphical arguments passed to \code{\link[lattice]{xyplot}}.
    These arguments can also be vectors or (named) lists, see details
    for more information.}
  \item{strip}{logical, character or function specifying headings used for 
     panels.  If character, should be a vector the same length as the 
     number of panels.  If \code{TRUE} column names are used for headers.
     If \code{FALSE}, no headings are produced.  See 
     \code{\link[lattice]{xyplot}} for the case in 
     which \code{strip} is a function.}
  \item{\dots}{additional arguments passed to \code{\link[lattice]{xyplot}}. }
}
\details{
  \code{xyplot.zoo} plots 
  a \code{"zoo"}, \code{"ts"} or \code{"its"} object using
  \code{\link[lattice]{xyplot}} from \pkg{lattice}. Series of other classes
  are coerced to \code{"zoo"} first.
  
  The handling of several graphical parameters is more
  flexible for multivariate series. These parameters can be
  vectors of the same length as the number of series plotted or
  are recycled if shorter. They can also be (partially) named list, e.g.,
  \code{list(A = c(1,2), c(3,4))} in which \code{c(3, 4)} is the
  default value and \code{c(1, 2)} the value only  for series \code{A}.
  The \code{screens} argument can be specified in a similar way.

  \code{plot.panel.default} is the default panel function.  
  \code{plot.panel.custom} facilitates the development of 
  custom panels.   Usually it has one argument \code{"panel"}
  which specifies the custom panel.  That panel typically calls
  \code{plot.panel.default} and uses the \code{panel.number}
  argument to take additional special action for particular panels.  
  See the examples.
}
\value{
  Invisibly returns a \code{"trellis"} class object. Printing this
  object using \code{print} will display it.
}
\examples{
library("lattice")
library("grid")

# change strip background to levels of grey
# If you like the defaults, this can be omitted.
strip.background <- trellis.par.get("strip.background")
trellis.par.set(strip.background = list(col = grey(7:1/8)))

set.seed(1)
z <- zoo(cbind(a = 1:5, b = 11:15, c = 21:25) + rnorm(5))
 
# plot a blue running mean on the panel of b.
# Also add a grid.
# We show two ways to do it. 

# Number 1.  Using trellis.focus.
print(xyplot(z))
trellis.focus("panel", 1, 2, highlight = FALSE)
z.mean <- rollmean(z, 3)
# uncomment next line and remove line after that when
# lattice makes panel.lines generic
# print(panel.lines(time(z.mean), z.mean[,2], col = "blue"))
print(panel.lines.zoo(z.mean[,2], col = "blue"))
print(panel.grid(h = 10, v = 10, col = "grey", lty = 3))
trellis.unfocus()

# Number 2. Using a custom panel routine.
p <- function(x, y, groups = NULL, ..., panel.number) {
  panel.xyplot(x, y, groups = groups, ..., panel.number)
  if (panel.number == 2) {
    panel.lines.zoo(rollmean(zoo(y, x), 3), col = "blue")
    panel.grid(h = 10, v = 10, col = "grey", lty = 3)
  }
}
print(xyplot(z, panel = panel.plot.custom(panel = p)))


# plot a light grey rectangle "behind" panel b
trellis.focus("panel", 1, 2)
grid.rect(x = 2, w = 1, default.units = "native", 
  gp = gpar(fill = "light grey"))
do.call("panel.plot.default", trellis.panelArgs())
trellis.unfocus()

# same but make first (i.e. bottom) panel twice as large as others
print(xyplot(z), heights = list(c(2,1,1), units = "null"))
# add a grid - this method does not confine grid to frames
# To do that see prior example.
panel.grid()

# Plot all in one panel.
print(xyplot(z, screens = 1))

# Plot first two columns in first panel and third column in second panel.
# Plot first series using points, second series using lines and third
#  series via overprinting both lines and points
# Use colors 1, 2 and 3 for the three series (1=black, 2=red, 3=green)
# Make 2nd (upper) panel 3x the height of the 1st (lower) panel
# Also make the strip background orange.
p <- xyplot(z, screens = c(1,1,2), type = c("p", "l", "o"), col = 1:3,
  par.setting = list(strip.background = list(col = "orange")))
print(p, panel.height = list(y = c(1, 3), units = "null"))

# Example of using a custom axis
# Months are labelled with smaller ticks for weeks and even smaller
# ticks for days.
Days <- seq(from = as.Date("2006-1-1"), to = as.Date("2006-8-8"), by = "day")
z <- zoo(seq(length(Days))^2, Days)
Months <- Days[format(Days, "\%d") == "01"]
Weeks <- Days[format(Days, "\%w") == "0"]
xyplot(z, scales = list(x = list(at = Months)))
trellis.focus("panel", 1, 1, clip.off = TRUE)
panel.axis("bottom", check.overlap = TRUE, outside = TRUE, labels = FALSE, 
  tck = .7, at = as.numeric(Weeks))
panel.axis("bottom", check.overlap = TRUE, outside = TRUE, labels = FALSE, 
  tck = .4, at = as.numeric(Days))
trellis.unfocus()

trellis.par.set(strip.background = strip.background)

}
\keyword{ts}
