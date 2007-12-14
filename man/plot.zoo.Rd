\name{plot.zoo}
\alias{plot.zoo}
\alias{barplot.zoo}
\alias{lines.zoo}
\alias{points.zoo}
\title{Plotting zoo Objects}
\description{
Plotting method for objects of class \code{"zoo"}.
}
\usage{
\method{plot}{zoo}(x, y = NULL, screens, plot.type,
  panel = lines, xlab = "Index", ylab = NULL, main = NULL,
  xlim = NULL, ylim = NULL, xy.labels = FALSE, xy.lines = NULL,
  oma = c(6, 0, 5, 0), mar = c(0, 5.1, 0, 2.1), 
  col = 1, lty = 1, pch = 1, type = "l", nc, widths = 1, heights = 1, \dots)
\method{lines}{zoo}(x, y = NULL, type = "l", \dots)
\method{points}{zoo}(x, y = NULL, type = "p", \dots)
}
\arguments{
  \item{x}{an object of class \code{"zoo"}.}
  \item{y}{an object of class \code{"zoo"}. If \code{y} is \code{NULL}
    (the default) a time series plot of \code{x} is produced, otherwise
    if both \code{x} and \code{y} are univariate \code{"zoo"} series, a
    scatter plot of \code{y} versus \code{x} is produced.}
  \item{screens}{factor (or coerced to factor) whose levels specify which
    graph each series is to be plotted in.  \code{screens=c(1,2,1)}
    would plot series 1, 2 and 3 in graphs 1, 2 and 1.  If not specified
    then 1 is used if \code{plot.type="single"} and \code{seq_len(ncol(x))}
    otherwise.}
  \item{plot.type}{for multivariate zoo objects, "multiple" plots the
    series on multiple plots and "single" superimposes them on a single
    plot.  Default is "single" if \code{screens} has only one level and
    \code{"multiple"} otherwise. If neither \code{screens} nor
    \code{plot.type} is specified then \code{"single"}
    is used if there is one series and \code{"mulitple"} otherwise.  This
    option is provided for back compatibility.  Usually \code{screens} is
    used instead.}
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
several graphical parameters is more flexible for multivariate series.
These parameters can be vectors of the same length as the number of
series plotted or are recycled if shorter. They can also be (partially)
named list, e.g., \code{list(A = c(1,2), c(3,4))} in which \code{c(3, 4)}
is the default value and \code{c(1, 2)} the value only  for series \code{A}.
The \code{screens} argument can be specified in a similar way.
If \code{plot.type} and \code{screens} conflict then multiple plots
will be assumed. Also see the examples.

In the case of a custom panel the panel can reference
\code{parent.frame$panel.number} in order to determine which
frame the panel is being called from.  See examples.

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
plot(z, type = "b", pch = 1:3, col = 1:3, ylab = list(expression(mu), "b", "c"))
colnames(z) <- LETTERS[1:3]
plot(z, screens = 1, col = list(B = 2))
plot(z, type = "b", pch = 1:3, col = 1:3)
plot(z, type = "b", pch = list(A = 1:5, B = 3), col = list(C = 4, 2))
plot(z, type = "b", screen = c(1,2,1), col = 1:3)
# right axis is for broken lines
plot(x)
opar <- par(usr = c(par("usr")[1:2], range(x2)))
lines(x2, lty = 2)
axis(4)
par(opar)


## Custom x axis labelling using a custom panel.
# 1. test data
z <- zoo(c(21, 34, 33, 41, 39, 38, 37, 28, 33, 40), 
     as.Date(c("1992-01-10", "1992-01-17", "1992-01-24", "1992-01-31", 
       "1992-02-07", "1992-02-14", "1992-02-21", "1992-02-28", "1992-03-06", 
       "1992-03-13")))
zz <- merge(a = z, b = z+10)
# 2. axis tick for every point. Also every 3rd point labelled.
my.panel <- function(...) {
   fmt <- "\%b-\%d" # format for axis labels
   lines(...)
   panel.number <- parent.frame()$panel.number
   # if bottom panel
   if (!length(panel.number) || panel.number == NCOL(zz)) { 
      # next line only if non-labelled ticks wanted for each point
      axis(1, at = time(zz), lab = FALSE)
      ix <- seq(1, length(zz), 3)
      labs <- format(time(zz), fmt)
      axis(1, at = time(zz)[ix], lab = labs[ix], tcl = -0.7, cex.axis = 0.7)
   }
}
# 3. plot
plot(zz, panel = my.panel, xaxt = "n")


# plot a mulitple ts series with nice X axis using panel function

tab <- ts(cbind(A = 1:24, B = 24:1), start = c(2006, 1), freq = 12)
pnl.xaxis <- function(...) {
     lines(...)
     panel.number <- parent.frame()$panel.number
     nser <- parent.frame()$nser
     # if bottom panel
     if (!length(panel.number) || panel.number == nser) { 
           tt <- list(...)[[1]]
           ym <- as.yearmon(tt)
	   mon <- as.numeric(format(ym, "\%m"))
	   yy <- format(ym, "\%y")
	   mm <- substring(month.abb[mon], 1, 1)
	   axis(1, tt[mon == 1], yy[mon == 1], cex.axis = 0.7)
	   axis(1, tt[mon > 1], mm[mon > 1], cex.axis = 0.5, tcl = -0.3)
     }
}
plot(as.zoo(tab), panel = pnl.xaxis, xaxt = "n")

## plot with left and right axes
## modified from http://www.mayin.org/ajayshah/KB/R/html/g6.html
set.seed(1)
z <- zoo(cbind(A = cumsum(rnorm(100)), B = cumsum(rnorm(100, mean = 0.2))))
opar <- par(mai = c(.8, .8, .2, .8))
plot(z[,1], type = "l", 
   xlab = "X axis label", ylab = colnames(z)[1])
par(new = TRUE)
plot(z[,2], type = "l", ann = FALSE, yaxt = "n", col = "blue")
axis(4)
legend(x = "topleft", bty = "n", lty = c(1,1), col = c("black", "blue"),
       legend = paste(colnames(z), c("(left scale)", "(right scale)")))
usr <- par("usr")
# if you don't care about srt= in text then mtext is shorter:
#  mtext(colnames(z)[2], 4, 2, col = "blue")
text(usr[2] + .1 * diff(usr[1:2]), mean(usr[3:4]), colnames(z)[2],
   srt = -90, xpd = TRUE, col = "blue")
par(opar)



## plot one zoo series against the other.
plot(x, x2)
plot(x, x2, xy.labels = TRUE)
plot(x, x2, xy.labels = 1:5, xy.lines = FALSE)

## barplot
x <- zoo(cbind(rpois(5, 2), rpois(5, 3)), x.Date)
barplot(x, beside = TRUE)
}
\keyword{ts}
