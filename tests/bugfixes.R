## packages
library("zoo")
library("timeDate")

## aggregate() with "timeDate" index
z <- zoo(1:3, timeDate(c("2011-09-19 12:00", "2011-09-19 12:00", "2011-09-19 13:00")))
aggregate(z, identity, mean)

## assignment and preservation of column names in merge()
x <- zoo(cbind(a = 3:4, b = 5:6))
y <- zoo(1:2)
merge(x, zoo(, time(x)))
merge(y, x)
