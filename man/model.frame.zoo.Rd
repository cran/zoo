\name{model.frame.zoo}
\alias{model.frame.AsIs}
\alias{model.frame.zoo}
\alias{model.frame.ts}
\concept{dynamic regression}
\title{Model Frame for zoo Series}
\description{
  \code{model.frame.AsIs} is used for dispatching \code{model.frame} to
  \code{model.frame.zoo} or \code{model.frame.ts} which create model frames
  from \code{"zoo"} and \code{"ts"} series respectively.
}
\usage{

\method{model.frame}{AsIs}(formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, \dots)
\method{model.frame}{zoo}(formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, \dots)
\method{model.frame}{ts}(formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, \dots)

}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{data}{optional object from which the data are taken.
    In the case of \code{model.frame.zoo} this is either a
    single \code{"zoo"} object or a data frame or list of \code{"zoo"}
    objects.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s. The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset. The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}. Other possible values include
    \code{\link{na.approx}}, \code{\link{na.contiguous}} and \code{\link{na.locf}}. }

  \item{drop.unused.levels}{should factors have unused levels dropped?
    Defaults to \code{FALSE}.}

  \item{xlev}{a named list of character vectors giving the full set of levels
    to be assumed for each factor.}

  \item{\dots}{additional arguments to be passed to the low level
    regression fitting functions (see below).}
}
\details{
  Regression functions such as \code{lm} typically specify the data
  to be used based on a \code{formula} (and optional further arguments)
  as in \code{lm(formula, \dots)}. The regression function then typically
  calls the generic function \code{model.frame} to convert the formula  
  (and related arguments) to a data frame which forms the basic data used
  by the regression.
  In such a situation, the \code{formula} argument of the regression
  function is passed to \code{model.frame} so the class of the
  \code{formula} determines which method of \code{model.frame} is used for
  data handling by the regression function.  

  Normally the class of \code{formula} is
  \code{"formula"} which causes \code{model.frame.formula} to be called. In time
  series regression (and potentially in other situations as well), a more
  specialized \code{model.frame} method should be called depending on the
  class of the dependent variable. For this alternate form of dispatch, 
  \code{model.frame.AsIs} is introduced: by insulating the \code{formula}
  argument in \code{I(formula)} the class is changed to \code{"AsIs"}
  (leaving \code{formula} unchanged) so that \code{model.frame.AsIs}
  is dispatched. \code{mode.frame.AsIs} does no processing of its own other than
  to examine the dependent variable of the formula and redispatch according
  to its class.
  Thus, if the dependent variable specified in \code{I(formula)} is of
  class \code{"foo"} the method \code{model.frame.foo} will be called for
  handling the data.

  If the dependent variable in such a model is of class \code{"zoo"} then
  \code{model.frame.zoo} will be called. Its  key role is to inspect a formula
  that may contain only \code{zoo} objects as variables and transform
  it to a model frame that can be used in various regression functions
  appropriately aligning the various series. If the \code{"zoo"} series
  should be specified using the \code{data} argument, such argument can be 
  a list of \code{"zoo"} objects, a single zoo object, or
  a data frame of \code{"zoo"} objects. Similarly, a \code{model.frame.ts}
  method is provided for \code{"ts"} objects. Note, that despite their names
  these methods do \emph{not} expect a normal \code{"zoo"} or \code{"ts"} object
  as their respective first argument but rather they expect a \code{formula}
  (whose dependent variable is of class 
  \code{"zoo"} or \code{"ts"} respectively).
  Their behaviour is essentially the same as in the default 
  \code{\link{model.frame}}
  method, but they retain the index/time information. Furthermore, they
  enable the user to use \code{\link{diff}} and \code{\link{lag}} in the
  model specification.
  
  As many regression functions in R use the same steps to extract the 
  data from a specified \code{formula}, this approach modularizes the
  data management and regression based on \code{"zoo"} objects making it
  available in various regression functions. Hence, the user will
  usually not have to call any of the \code{model.frame} functions explicitly
  but only has to insulate the \code{formula} with \code{I()}. 
  See the examples for an illustration.
  The regression functions for which this approach is known to work 
  includes \code{\link[stats]{lm}},
  \code{\link[stats]{glm}}, \code{\link[Design]{lrm}}, \code{\link[MASS]{lqs}},
  \code{\link[nnet]{nnet}}, \code{\link[e1071]{svm}}, 
  \code{\link[quantreg]{rq}},
  \code{\link[randomForest]{randomForest}} and possibly many others.

   IMPORTANT: Note, that this feature is under development and might
   change in future versions.
}

\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula} (plus those specified in the remaining arguments).
}
\seealso{\code{\link{model.frame}}}


\examples{
yz <- zoo(1:12)^2
xz <- zoo(1:9)^2
fz <- zoo(gl(2,5))

# example of dispatching on response, using diff and
# lag, using a factor and returning residuals and fitted 
# zoo objects
yz.lm <- lm(I(lag(yz) ~ diff(xz) + fz))
residuals(yz.lm)
fitted(yz.lm)

# examples of using list in data argument
lm(I(lag(y) ~ diff(x) + f), list(y = yz, x = xz, f = fz))
lm(I(y ~ x+f), list(y=lag(yz), x=diff(xz), f=fz))

# examples of using a zoo object in the data argument
lm(I(lag(y) ~ diff(x)), merge(y=yz, x=xz))
lm(I(lag(yz) ~ diff(xz)), merge(yz, xz))
lm(I(y ~ x), merge(y = lag(yz), x = diff(xz)))

# using a variety of other modelling functions
# note that residuals and fitted may or may not
# work with these and if they do work may not
# return zoo objects.

# resistant regression
if(require(MASS))
lqs(I(lag(yz) ~ diff(xz)+fz))

# neural network
if(require(nnet))
nnet(I(lag(yz) ~ diff(xz)+fz), size = 1)

# quantile regression
if(require(quantreg))
rq(I(lag(yz) ~ fz), tau = 0.25)

# random forest
set.seed(1)
if(require(randomForest))
randomForest(I(lag(yz) ~ diff(xz) + fz))

# support vector machine
if(require(e1071))
svm(I(lag(yz) ~ diff(xz) + fz))

}
\keyword{regression}

