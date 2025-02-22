\name{tinyplot.zoo}
\alias{tinyplot.zoo}

\title{Plotting zoo Objects with tinyplot}

\details{
  Convenience interface for visualizing zoo objects with tinyplot.
}

\usage{
  \method{tinyplot}{zoo}(object, facet, type = "l",
    facet.args = list(free = TRUE), ylab = "", \dots)
}

\arguments{
  \item{object}{an object of class \code{"zoo"}.}
  \item{facet}{specification of \code{facet} for \code{\link[tinyplot]{tinyplot.formula}}. The
    default in the \code{tinyplot} method is to use \code{facet = NULL} for univariate
    series and \code{facet = ~ Series} for multivariate series.}
  \item{type, facet.args, ylab, \dots}{further arguments passed to \code{tinyplot}.}
}

\description{
  The approach is similar to that in \code{\link{autoplot.zoo}}
  (based on ggplot2) and uses \code{\link{fortify.zoo}} (with \code{melt = TRUE})
  to convert the zoo object into a data frame and then uses a suitable
  formula to visiualize the series.
}

\seealso{
  \code{\link[tinyplot]{tinyplot}}, \code{\link{autoplot.zoo}}
}

\examples{
if(require("tinyplot")) {
suppressWarnings(RNGversion("3.5.0"))
set.seed(1)

## example data
x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
x <- zoo(rnorm(5), x.Date)
xlow <- x - runif(5)
xhigh <- x + runif(5)
z <- cbind(x, xlow, xhigh)

## univariate plotting
tinyplot(x)

## multivariate plotting in multiple or single panels
tinyplot(z)                    ## multiple without color/linetype with free scales
tinyplot(z, facet.args = NULL) ## multiple without color/linetype with same scale
tinyplot(z, facet = ~ Series)  ## multiple with series-dependent color/linetype
tinyplot(z, facet = NULL)      ## single with series-dependent color/linetype

## by hand with color/linetype and with/without facets
d <- fortify.zoo(z, melt = TRUE)
tinyplot(Value ~ Index | Series, data = d, type = "l")
tinyplot(Value ~ Index | Series, facet = "by", data = d, type = "l")
tinyplot(Value ~ Index | Series, facet = "by", data = d, type = "l", facet.args = list(free = TRUE))

## EuStockMarkets data (coerced from "ts")
eusm <- as.zoo(EuStockMarkets)
tinyplot(eusm)
tinytheme("clean2")
tinyplot(eusm, facet = NULL)
tinyplot(eusm, facet = ~ Series)
tinyplot(eusm, facet = ~ Series, facet.args = NULL)
tinytheme() ## reset

}
}

