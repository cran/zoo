## ----preliminaries, include=FALSE---------------------------------------------
library("zoo")
library("tseries")
online <- FALSE ## if set to FALSE the local copy of
                ## is used instead of get.hist.quote()
options(prompt = "R> ")
Sys.setenv(TZ = "GMT")
suppressWarnings(RNGversion("3.5.0"))

## ----setwd, eval=FALSE--------------------------------------------------------
# library("zoo")
# setwd(file.path(system.file(package = "zoo"), "doc"))

## ----read.zoo-----------------------------------------------------------------
Sys.setlocale("LC_TIME", "C")
inrusd <- read.zoo("demo1.txt", sep = "|", format="%d %b %Y")

## ----read.table---------------------------------------------------------------
tmp <- read.table("demo2.txt", sep = ",")
z <- zoo(tmp[, 3:4], as.Date(as.character(tmp[, 2]), format="%d %b %Y"))
colnames(z) <- c("Nifty", "Junior")

## ----extract dates------------------------------------------------------------
time(z)

## ----start and end------------------------------------------------------------
start(z)
end(inrusd)

## ----convert to plain matrix--------------------------------------------------
plain <- coredata(z)
str(plain)

## ----intersection-------------------------------------------------------------
m <- merge(inrusd, z, all = FALSE)

## ----union--------------------------------------------------------------------
m <- merge(inrusd, z)

## ----merge with lag-----------------------------------------------------------
merge(inrusd, lag(inrusd, -1))

## ----plotting1,fig.height=8,fig.width=6---------------------------------------
plot(m)

## ----plotting2,fig.height=4,fig.width=6---------------------------------------
plot(m[, 2:3], plot.type = "single", col = c("red", "blue"), lwd = 2)

## ----tinyplot, eval=requireNamespace("tinyplot", quietly = TRUE), fig.height=6, fig.width=6----
library("tinyplot")
tinyplot(z, facet = Series ~ 1, theme = "clean2",
  palette = "Dark 3", lwd = 2, legend = FALSE)

## ----select range of dates----------------------------------------------------
window(z, start = as.Date("2005-02-15"), end = as.Date("2005-02-28"))

## ----select one date----------------------------------------------------------
m[as.Date("2005-03-10")]

## ----impute NAs by interpolation----------------------------------------------
interpolated <- na.approx(m)

## ----impute NAs by LOCF-------------------------------------------------------
m <- na.locf(m)
m

## ----compute returns function-------------------------------------------------
prices2returns <- function(x) 100*diff(log(x))

## ----column-wise returns------------------------------------------------------
r <- prices2returns(m)

## ----rolling standard deviations----------------------------------------------
rollapply(r, 10, sd)

## ----last day of month--------------------------------------------------------
prices2returns(aggregate(m, as.yearmon, tail, 1))

## ----last day of week---------------------------------------------------------
nextfri <- function(x) 7 * ceiling(as.numeric(x-5+4) / 7) + as.Date(5-4)
prices2returns(aggregate(na.locf(m), nextfri, tail, 1))

## ----four second mark---------------------------------------------------------
zsec <- structure(1:10, index = structure(c(1234760403.968, 1234760403.969, 
1234760403.969, 1234760405.029, 1234760405.029, 1234760405.03, 
1234760405.03, 1234760405.072, 1234760405.073, 1234760405.073
), class = c("POSIXt", "POSIXct"), tzone = ""), class = "zoo")

to4sec <- function(x) as.POSIXct(4*ceiling(as.numeric(x)/4), origin = "1970-01-01")
aggregate(zsec, to4sec, tail, 1)

## ----one second grid----------------------------------------------------------
# tmp is zsec with time discretized into one second bins
tmp <- zsec
st <- start(tmp)
Epoch <- st - as.numeric(st)
time(tmp) <- as.integer(time(tmp) + 1e-7) + Epoch

# find index of last value in each one second interval
ix <- !duplicated(time(tmp), fromLast = TRUE)

# merge with grid 
merge(tmp[ix], zoo(, seq(start(tmp), end(tmp), "sec")))

# Here is a function which generalizes the above:

intraday.discretise <- function(b, Nsec) {
 st <- start(b)
 time(b) <- Nsec * as.integer(time(b)+1e-7) %/% Nsec + st -
 as.numeric(st)
 ix <- !duplicated(time(b), fromLast = TRUE)
 merge(b[ix], zoo(, seq(start(b), end(b), paste(Nsec, "sec"))))
}

intraday.discretise(zsec, 1)


## ----tseries------------------------------------------------------------------
library("tseries")

## ----data handling if offline, includ=FALSE-----------------------------------
if(online) {
  msft <- get.hist.quote(instrument = "MSFT", start = "2004-01-01", end = "2004-12-31")
  msft2 <- get.hist.quote(instrument = "MSFT", start = "2004-01-01", end = "2004-12-31",
    compression = "m", quote = "Close")
  save(msft, msft2, file = "msft2004.rda")
} else {
  load("msft2004.rda")
}

## ----get.hist.quote daily series, eval=FALSE----------------------------------
# msft <- get.hist.quote(instrument = "MSFT", start = "2004-01-01", end = "2004-12-31")

## ----get.hist.quote monthly series, eval=FALSE--------------------------------
# msft2 <- get.hist.quote(instrument = "MSFT", start = "2004-01-01", end = "2004-12-31",
#   compression = "m", quote = "Close")

## ----change index to yearmon--------------------------------------------------
time(msft2) <- as.yearmon(time(msft2))

## ----compute same series via aggregate----------------------------------------
msft3 <- aggregate(msft[, "Close"], as.yearmon, tail, 1)

## ----compute returns----------------------------------------------------------
r <- prices2returns(msft3)

## ----summaries----------------------------------------------------------------
date1 <- seq(as.Date("2001-01-01"), as.Date("2002-12-1"), by = "day")
len1 <- length(date1)
set.seed(1) # to make it reproducible
data1 <- zoo(rnorm(len1), date1)

# quarterly summary

data1q.mean <- aggregate(data1, as.yearqtr, mean)
data1q.sd <- aggregate(data1, as.yearqtr, sd)
head(cbind(mean = data1q.mean, sd = data1q.sd), main = "Quarterly")

# weekly summary - week ends on tuesday

# Given a date find the next Tuesday.
# Based on formula in Prices and Returns section.
nexttue <- function(x) 7 * ceiling(as.numeric(x - 2 + 4)/7) + as.Date(2 - 4)

data1w <- cbind(
       mean = aggregate(data1, nexttue, mean),
       sd = aggregate(data1, nexttue, sd)
)
head(data1w)

### ALTERNATIVE ###

# Create function ag like aggregate but takes vector of
# function names.

FUNs <- c(mean, sd)
ag <- function(z, by, FUNs) {
       f <- function(f) aggregate(z, by, f)
       do.call(cbind, sapply(FUNs, f, simplify = FALSE))
}

data1q <- ag(data1, as.yearqtr, c("mean", "sd"))
data1w <- ag(data1, nexttue, c("mean", "sd"))

head(data1q)
head(data1w)

## ----is.weekend convenience function------------------------------------------
is.weekend <- function(x) ((as.numeric(x)-2) %% 7) < 2

## ----is.weekend based on POSIXlt----------------------------------------------
is.weekend <- function(x) {
  x <- as.POSIXlt(x)
  x$wday > 5 | x$wday < 1
}

