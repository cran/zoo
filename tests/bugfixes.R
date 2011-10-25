## packages
library("zoo")
library("timeDate")

## aggregate() with "timeDate" index
z <- zoo(1:3, timeDate(c("2011-09-19 12:00", "2011-09-19 12:00", "2011-09-19 13:00")))
aggregate(z, identity, mean)
