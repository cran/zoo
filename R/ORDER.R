ORDER <- function(x, ...)
  UseMethod("ORDER")

ORDER.default <- function(x, ..., na.last = TRUE, decreasing = FALSE)
  order(x, ..., na.last = na.last, decreasing = decreasing)

ORDER.timeDate <- function(x, ...) {
  stopifnot(require("fCalendar"))
  order(as.POSIXct(x), ...)
}
