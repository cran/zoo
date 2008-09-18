###################################################
### chunk number 1: preliminaries
###################################################
library("zoo")


###################################################
### chunk number 2: duplicates1
###################################################
z <- suppressWarnings(zoo(1:8, c(1, 2, 2, 2, 3, 4, 5, 5)))
z


###################################################
### chunk number 3: duplicates2
###################################################
aggregate(z, force, mean)


###################################################
### chunk number 4: duplicates3
###################################################
aggregate(z, force, tail, 1)


###################################################
### chunk number 5: duplicates4
###################################################
time(z) <- na.approx(ifelse(duplicated(time(z)), NA, time(z)), na.rm = FALSE)


###################################################
### chunk number 6: duplicates5
###################################################
z[!is.na(time(z))]


###################################################
### chunk number 7: duplicates
###################################################
Lines <- "1|BHARTIARTL|EQ|18:15:05|600|1
2|BHARTIARTL|EQ|18:15:05|600|99
3|GLENMARK|EQ|18:15:05|238.1|5
4|HINDALCO|EQ|18:15:05|43.75|100
5|BHARTIARTL|EQ|18:15:05|600|1
6|BHEL|EQ|18:15:05|1100|11
7|HINDALCO|EQ|18:15:06|43.2|1
8|CHAMBLFERT|EQ|18:15:06|46|10
9|CHAMBLFERT|EQ|18:15:06|46|90
10|BAJAUTOFIN|EQ|18:15:06|80|100"

library(zoo)
library(chron)

tail1 <- function(x) tail(x, 1)
cls <- c("NULL", "NULL", "NULL", "character", "numeric", "numeric")
nms <- c("", "", "", "time", "value", "volume")

z <- read.zoo(textConnection(Lines), aggregate = tail1,
       FUN = times, sep = "|", colClasses = cls, col.names = nms)

# re-read using sum
z2 <- read.zoo(textConnection(Lines), aggregate = sum,
       FUN = times, sep = "|", colClasses = cls, col.names = nms)

z$volume <- z2$volume
z


###################################################
### chunk number 8: log-plot
###################################################
z <- zoo(1:100)
plot(z, log = "y", panel = function(..., log) lines(...))


###################################################
### chunk number 9: plot-axes eval=FALSE
###################################################
## set.seed(1)
## z.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
## z <- zoo(cbind(left = rnorm(5), right = rnorm(5, sd = 0.2)), z.Date)
## 
## plot(z[,1], xlab = "Time", ylab = "")
## opar <- par(usr = c(par("usr")[1:2], range(z[,2])))
## lines(z[,2], lty = 2)
## 
## axis(side = 4)
## legend("bottomright", lty = 1:2, legend = colnames(z), bty="n")
## par(opar)


###################################################
### chunk number 10: plot-axes1
###################################################
set.seed(1)
z.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
z <- zoo(cbind(left = rnorm(5), right = rnorm(5, sd = 0.2)), z.Date)

plot(z[,1], xlab = "Time", ylab = "")
opar <- par(usr = c(par("usr")[1:2], range(z[,2])))
lines(z[,2], lty = 2)

axis(side = 4)
legend("bottomright", lty = 1:2, legend = colnames(z), bty="n")
par(opar)


###################################################
### chunk number 11: factor1
###################################################
DF <- data.frame(time = 1:4, x = 1:4, f = factor(letters[c(1, 1, 2, 2)]))
zx <- zoo(DF$x, DF$time)
zf <- zoo(DF$f, DF$time)


###################################################
### chunk number 12: factor2
###################################################
DF2 <- data.frame(x = zx, f = zf)


###################################################
### chunk number 13: factor3
###################################################
z <- zoo(data.matrix(DF[-1]), DF$time)


###################################################
### chunk number 14: lags
###################################################
z <- zoo(11:15, as.Date("2008-01-01") + c(-4, 1, 2, 3, 6))
zr <- as.zooreg(z)

lag(z)
lag(zr)

diff(log(z))
diff(log(zr))


###################################################
### chunk number 15: subtract-monthly-means
###################################################
set.seed(123)
z <- zoo(rnorm(100), as.Date("2007-01-01") + seq(0, by = 10, length = 100))
z.demean1 <- z - ave(z, as.yearmon(time(z)))


###################################################
### chunk number 16: subtract-monthly-means2
###################################################
z.demean2 <- z - ave(z, format(time(z), "%m"))


###################################################
### chunk number 17: yearmon2
###################################################
# as.yearmon2 generic and as.yearmon2.Date method
as.yearmon2 <- function(x, ...) UseMethod("as.yearmon2")
as.yearmon2.Date <- function(x, ...) {
  y <- as.yearmon(with(as.POSIXlt(x, tz = "GMT"), 1900 + year + mon/12))
  names(y) <- x
  structure(y, class = c("yearmon2", class(y)))
}


###################################################
### chunk number 18: yearmon2-inverse
###################################################
as.Date.yearmon2 <- function(x, frac = 0, ...) {
     if (!is.null(names(x))) return(as.Date(names(x)))
     x <- unclass(x)
     year <- floor(x + .001)
     month <- floor(12 * (x - year) + 1 + .5 + .001)
     dd.start <- as.Date(paste(year, month, 1, sep = "-")) 
     dd.end <- dd.start + 32 - as.numeric(format(dd.start + 32, "%d"))
     as.Date((1-frac) * as.numeric(dd.start) + frac * as.numeric(dd.end), origin = "1970-01-01")
}


###################################################
### chunk number 19: yearmon2-example
###################################################
dd <- seq(as.Date("2000-01-01"), length = 5, by = 32)
z <- zoo(1:5, as.yearmon2(dd))
z
aggregate(z, as.Date, force) 


###################################################
### chunk number 20: single-panel
###################################################
z <- zoo(0:500, as.Date(0:500))
plot(z, xaxt = "n")
tt <- time(z)
m <- unique(as.Date(as.yearmon(tt)))
jan <- format(m, "%m") == "01"
mlab <- substr(months(m[!jan]), 1, 1)
axis(side = 1, at = m[!jan], labels = mlab, tcl = -0.3, cex.axis = 0.7)
axis(side = 1, at = m[jan], labels = format(m[jan], "%y"), tcl = -0.7)
axis(side = 1, at = unique(as.Date(as.yearqtr(tt))), labels = FALSE)

# vertical grid lines
abline(v = m, col = grey(0.8), lty = 2)


###################################################
### chunk number 21: multiplesingleplot
###################################################
z3 <- cbind(z1 = z, z2 = 2*z, z3 = 3*z)
opar <- par(mfrow = c(2, 2))
tt <- time(z)
m <- unique(as.Date(as.yearmon(tt)))
jan <- format(m, "%m") == "01"
mlab <- substr(months(m[!jan]), 1, 1)
for(i in 1:ncol(z3)) {
    plot(z3[,i], xaxt = "n", ylab = colnames(z3)[i], ylim = range(z3))
    axis(side = 1, at = m[!jan], labels = mlab, tcl = -0.3, cex.axis = 0.7)
    axis(side = 1, at = m[jan], labels = format(m[jan], "%y"), tcl = -0.7)
    axis(side = 1, at = unique(as.Date(as.yearqtr(tt))), labels = FALSE)
}
par(opar)


###################################################
### chunk number 22: multipanelplot
###################################################
plot(z3, screen = 1:3, xaxt = "n", nc = 2, ylim = range(z3),
  panel = function(...) {
   lines(...)
   panel.number <- parent.frame()$panel.number
   nser <- parent.frame()$nser
   # place axis on bottom panel of each column only
   if (panel.number %% 2 == 0 || panel.number == nser) { 
         tt <- list(...)[[1]]
         m <- unique(as.Date(as.yearmon(tt)))
         jan <- format(m, "%m") == "01"
         mlab <- substr(months(m[!jan]), 1, 1)
         axis(side = 1, at = m[!jan], labels = mlab, tcl = -0.3, cex.axis = 0.7)
         axis(side = 1, at = m[jan], labels = format(m[jan], "%y"), tcl = -0.7)
         axis(side = 1, at = unique(as.Date(as.yearqtr(tt))), labels = FALSE)
    }
})


###################################################
### chunk number 23: plot-with-na
###################################################
z <- zoo(c(1, NA, 2, NA, 3))
plot(z)


###################################################
### chunk number 24: plot-with-na1
###################################################
plot(z, type = "p") 


###################################################
### chunk number 25: plot-with-na2
###################################################
plot(na.omit(z))


###################################################
### chunk number 26: plot-with-na3
###################################################
plot(na.approx(z))


###################################################
### chunk number 27: plot-with-na4
###################################################
plot(z, type = "p")
lines(na.omit(z))


###################################################
### chunk number 28: Rmetrics
###################################################
library("timeDate")
dts <- c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
tms <- c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
td <- timeDate(paste(dts, tms), format = "%Y-%m-%d %H:%M:%S")

library("zoo")
z <- zoo(1:4, td)
zz <- merge(z, lag(z))
plot(zz)

library("timeSeries")
zz
as.timeSeries(zz)
as.zoo(as.timeSeries(zz))


###################################################
### chunk number 29: Rmetrics-detach
###################################################
detach("package:timeDate")
detach("package:timeSeries")


###################################################
### chunk number 30: ifelse
###################################################
z <- zoo(c(1, 5, 10, 15))
# wrong !!!
ifelse(diff(z) > 4, -z, z)

# ok
ifelse.zoo(diff(z) > 4, -z, z)

# or if we merge first we can use ordinary ifelse
xm <- merge(z, dif = diff(z))
with(xm, ifelse(dif > 4, -z, z))

# or in this case we could also use orindary ifelse if we 
# use na.pad = TRUE to ensure all three have same index
ifelse(diff(z, na.pad = TRUE) > 4, -z, z)


