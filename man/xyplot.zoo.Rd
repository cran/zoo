\name{xyplot.zoo}
\alias{xyplot.zoo}
\alias{xyplot.its}
\alias{xyplot.tis}
\alias{llines.zoo}
\alias{llines.its}
\alias{llines.tis}
\alias{lpoints.zoo}
\alias{lpoints.its}
\alias{lpoints.tis}
\alias{ltext.zoo}
\alias{ltext.its}
\alias{ltext.tis}
%% Deprecated:
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
\alias{panel.plot.default}
\alias{panel.plot.custom}
%% Unnecessary?:
\alias{panel.segments.zoo}
\alias{panel.segments.ts}
\alias{panel.segments.its}
\alias{panel.segments.tis}
\alias{panel.rect.zoo}
\alias{panel.rect.ts}
\alias{panel.rect.its}
\alias{panel.rect.tis}
\alias{panel.polygon.zoo}
\alias{panel.polygon.ts}
\alias{panel.polygon.its}
\alias{panel.polygon.tis}

\title{Plot zoo Series with Lattice}

\description{
\code{xyplot} methods for time series objects (of class \code{"zoo"},
\code{"its"}, or \code{"tis"}). 
}

\usage{
\method{xyplot}{zoo}(x, data, ...)

\method{llines}{zoo}(x, y = NULL, ...)
\method{lpoints}{zoo}(x, y = NULL, ...)
\method{ltext}{zoo}(x, y = NULL, ...)

panel.segments.zoo(x0, x1, ...)
panel.rect.zoo(x0, x1, ...)
panel.polygon.zoo(x, ...)
}

\arguments{
  \item{x, x0, x1}{time series object of class \code{"zoo"}, \code{"its"} or 
  \code{"tis"}.  For \code{panel.plot.default} it should be a numeric
  vector.}
  \item{y}{numeric vector or matrix.}
  \item{data}{not used.}
  \item{\dots}{
    arguments are passed to \code{\link[lattice]{xyplot.ts}}, and may be
    passed through to \code{\link[lattice]{xyplot}} and
    \code{\link[lattice]{panel.xyplot}}.

    Some of the commonly used arguments are:
  \describe{
  \item{\code{screens}}{factor (or coerced to factor) whose levels specify which
    graph each series is to be plotted in.  \code{screens = c(1, 2, 1)}
    would plot series 1, 2 and 3 in graphs 1, 2 and 1. This also defines
    the strip text in multi-panel plots.}
  \item{\code{scales}}{ the default is set so that all
    series have the \code{"same"} X axis but \code{"free"} Y axis. 
    See \code{\link[lattice]{xyplot}} in the \pkg{lattice} package for more
    information on \code{scales}. }
  \item{\code{layout}}{numeric vector of length 2 specifying number of columns
    and rows in the plot, see \code{\link[lattice]{xyplot}} for more details.
    The default is to fill columns with up to 6 rows.}
  \item{\code{xlab}}{character string used as the X axis label.}
  \item{\code{ylab}}{character string used as the Y axis label. If there
    are multiple panels it may be a character vector the same length
    as the number of panels, but \emph{NOTE} in this case the vector
    should be reversed OR the argument \code{as.table} set to
    \code{FALSE}.}
  \item{\code{lty, lwd, pch, type, col}}{
    graphical arguments passed to \code{\link[lattice]{panel.xyplot}}.
    These arguments can also be vectors or (named) lists, see details
    for more information.}
 }
 }
}

\details{
  \code{xyplot.zoo} plots a \code{"zoo"}, \code{"its"} or \code{"tis"}
  object using \code{\link[lattice]{xyplot.ts}} from
  \pkg{lattice}. Series of other classes are coerced to \code{"zoo"}
  first. 
  
  The handling of several graphical parameters is more
  flexible for multivariate series. These parameters can be
  vectors of the same length as the number of series plotted or
  are recycled if shorter. They can also be (partially) named list, e.g.,
  \code{list(A = c(1,2), c(3,4))} in which \code{c(3, 4)} is the
  default value and \code{c(1, 2)} the value only  for series \code{A}.
  The \code{screens} argument can be specified in a similar way.

  Note that since \pkg{zoo} 1.6-3 \code{plot.panel.default} and
  \code{plot.panel.custom} are no longer necessary, as normal panel
  functions (\code{\link[lattice]{panel.xyplot}} by default) will work.

  Similarly, there are now methods for the generic \pkg{lattice} drawing
  functions \code{\link[lattice]{llines}},
  \code{\link[lattice]{lpoints}}, and
  \code{\link[lattice]{ltext}}. These can also be called as 
  \code{panel.lines}, \code{panel.points}, and \code{panel.text},
  respectively. The old interfaces (\code{panel.lines.zoo},
  \code{panel.points.zoo}, and \code{panel.text.zoo}), will be
  removed in future versions. \code{panel.polygon.zoo} may also be
  removed. 
}

\value{
  Invisibly returns a \code{"trellis"} class object. Printing this
  object using \code{print} will display it.
}

\seealso{\code{\link[lattice]{xyplot.ts}}, \code{\link{zoo}},
  \code{\link{plot.ts}}, \code{\link{barplot}}, \code{\link{plot.zoo}}}

\examples{
if(require("lattice") & require("grid")) {

set.seed(1)
z <- zoo(cbind(a = 1:5, b = 11:15, c = 21:25) + rnorm(5))

# plot z using same Y axis on all plots
xyplot(z, scales = list(y = list(relation = "same", alternating = FALSE)))
 
# plot a double-line-width running mean on the panel of b.
# Also add a grid.
# We show two ways to do it. 

# change strip background to levels of grey
# If you like the defaults, this can be omitted.
strip.background <- trellis.par.get("strip.background")
trellis.par.set(strip.background = list(col = grey(7:1/8)))


# Number 1.  Using trellis.focus.
print( xyplot(z) )
trellis.focus("panel", 1, 2, highlight = FALSE)
# (or just trellis.focus() for interactive use)
z.mean <- rollmean(z, 3)
panel.lines(z.mean[,2], lwd = 2)
panel.grid(h = 10, v = 10, col = "grey", lty = 3)
trellis.unfocus()

# Number 2. Using a custom panel routine.
xyplot(z, panel = function(x, y, ...) {
    if (packet.number() == 2) {
        panel.grid(h = 10, v = 10, col = "grey", lty = 3)
        panel.lines(rollmean(zoo(y, x), 3), lwd = 2)
    }
    panel.xyplot(x, y, ...)
})

# plot a light grey rectangle "behind" panel b
trellis.focus("panel", 1, 2)
grid.rect(x = 2, w = 1, default.units = "native", 
  gp = gpar(fill = "light grey"))
# do.call("panel.xyplot", trellis.panelArgs())
do.call("panel.lines", trellis.panelArgs()[1:2])
trellis.unfocus()
# a better method is to use a custom panel function.
# see also panel.xblocks() and layer() in the latticeExtra package.

# same but make first panel twice as large as others
lopt <- list(layout.heights = list(panel = list(x = c(2,1,1))))
xyplot(z, lattice.options = lopt)
# add a grid
update(trellis.last.object(), type = c("l", "g"))

# Plot all in one panel.
xyplot(z, screens = 1)
# Same with default styles and auto.key:
xyplot(z, superpose = TRUE)

# Plot first two columns in first panel and third column in second panel.
# Plot first series using points, second series using lines and third
#  series via overprinting both lines and points
# Use colors 1, 2 and 3 for the three series (1=black, 2=red, 3=green)
# Make 2nd (lower) panel 3x the height of the 1st (upper) panel
# Also make the strip background orange.
p <- xyplot(z, screens = c(1,1,2), type = c("p", "l", "o"), col = 1:3,
  par.settings = list(strip.background = list(col = "orange")))
print(p, panel.height = list(y = c(1, 3), units = "null"))

# Example of using a custom axis
# Months are labelled with smaller ticks for weeks and even smaller
# ticks for days.
Days <- seq(from = as.Date("2006-1-1"), to = as.Date("2006-8-8"), by = "day")
z1 <- zoo(seq(length(Days))^2, Days)
Months <- Days[format(Days, "\%d") == "01"]
Weeks <- Days[format(Days, "\%w") == "0"]
print( xyplot(z1, scales = list(x = list(at = Months))) )
trellis.focus("panel", 1, 1, clip.off = TRUE)
panel.axis("bottom", check.overlap = TRUE, outside = TRUE, labels = FALSE, 
  tck = .7, at = as.numeric(Weeks))
panel.axis("bottom", check.overlap = TRUE, outside = TRUE, labels = FALSE, 
  tck = .4, at = as.numeric(Days))
trellis.unfocus()

trellis.par.set(strip.background = strip.background)

# separate the panels and suppress the ticks on very top
xyplot(z, between = list(y = 1), scales = list(tck = c(1,0)))

# left strips but no top strips
xyplot(z, screens = colnames(z), strip = FALSE, strip.left = TRUE)

# plot list of zoo objects using different x scales
z.l <- list(
	zoo(cbind(a = rnorm(10), b = rnorm(10)), as.Date("2006-01-01") + 0:9),
	zoo(cbind(c = rnorm(10), d = rnorm(10)), as.Date("2006-12-01") + 0:9)
)
zm <- do.call(merge, z.l)
xlim <- lapply(zm, function(x) range(time(na.omit(x))))
xyplot(zm, xlim = xlim, scale = list(relation = "free"))
# to avoid merging see xyplot.list() in the latticeExtra package.

}

\dontrun{
## playwith (>= 0.9)
library("playwith")

z3 <- zoo(cbind(a = rnorm(100), b = rnorm(100) + 1), as.Date(1:100))
playwith(xyplot(z3), time.mode = TRUE)
# hold down Shift key and drag to zoom in to a time period.
# then use the horizontal scroll bar.

# set custom labels; right click on points to view or add labels
labs <- paste(round(z3,1), index(z3), sep = "@")
trellis.par.set(user.text = list(cex = 0.7))
playwith(xyplot(z3, type = "o"), labels = labs)

# this returns indexes into times of clicked points 
ids <- playGetIDs()
z3[ids,]

## another example of using playwith with zoo
# set up data
dat <- zoo(matrix(rnorm(100*100),ncol=100), Sys.Date()+1:100)
colnames(dat) <- paste("Series", 1:100)

# This will give you a spin button to choose the column to plot,
# and a button to print out the current series number.
playwith(xyplot(dat[,c(1,i)]),  parameters = list(i = 1:100,
   do_something = function(playState) print(playState$env$i))

}

}

\keyword{hplot}
\keyword{ts}
