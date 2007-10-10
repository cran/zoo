panel.plot.default <- function(x, y, subscripts, groups, panel = panel.xyplot,
  col = 1, type = "p", pch = 20, lty = 1, lwd = 1, ...)
{
  col <- rep(as.list(col), length = nlevels(groups))
  type <- rep(as.list(type), length = nlevels(groups))
  pch <- rep(as.list(pch), length = nlevels(groups))
  lty <- rep(as.list(lty), length = nlevels(groups))
  lwd <- rep(as.list(lwd), length = nlevels(groups))

  for(g in 1:nlevels(groups)) {
    idx <- g == groups[subscripts]
    if (any(idx)) panel(x[idx], y[idx], ...,
      col = col[[g]], type = type[[g]], pch = pch[[g]],
      lty = lty[[g]], lwd = lwd[[g]])
  }
}

panel.plot.custom <- function(...) {
  args <- list(...)
  function(...) {
    dots <- list(...)
    # do.call("panel.plot.default", lattice:::updateList(dots, args))
    do.call("panel.plot.default", modifyList(dots, args))
  }
}

xyplot.its <-
xyplot.ts <-
xyplot.zoo <- function(x, data,
  screens = seq(length = NCOL(x)),
  default.scales = list(y = list(relation = "free")),
  layout = NULL, xlab = "Index", ylab = NULL,
  lty = trellis.par.get("plot.line")$lty,
  lwd = trellis.par.get("plot.line")$lwd,
  pch = trellis.par.get("plot.symbol")$pch, 
  type = "l", 
  col = trellis.par.get("plot.line")$col, 
  strip = TRUE,
  panel = panel.plot.default, ...)
{
  x <- as.zoo(x)
  if (length(dim(x)) < 2) x <- zoo(matrix(coredata(x),,1), time(x))

  cn <- if (is.null(colnames(x))) paste("V", seq(length = NCOL(x)), sep = "")
          else colnames(x)
  screens <- make.par.list(cn, screens, NROW(x), NCOL(x), 1)
  screens <- as.factor(unlist(screens))[drop = TRUE]
  lty <- make.par.list(cn, lty, NROW(x), NCOL(x), trellis.par.get("plot.line")$lty)
  lwd <- make.par.list(cn, lwd, NROW(x), NCOL(x), trellis.par.get("plot.line")$lwd)
  pch <- make.par.list(cn, pch, NROW(x), NCOL(x), trellis.par.get("plot.symbol")$pch)
  type <- make.par.list(cn, type, NROW(x), NCOL(x), "l")
  col <- make.par.list(cn, col, NROW(x), NCOL(x), trellis.par.get("plot.line")$col)

  tt <- rep(time(x), NCOL(x))
  x <- coredata(x)
  screens <- rep(screens, length = NCOL(x))
  fac <- factor(rep(screens, each = NROW(x)))
  if(is.null(layout)) {
    nc <- ceiling(nlevels(fac)/5)
    nr <- ceiling(nlevels(fac)/nc)
    layout <- c(nc, nr)
  }

  fo <- if(NCOL(x) == 1) x ~ tt else x ~ tt | fac

  if (isTRUE(strip)) {
    isnotdup <- !duplicated(screens)
    strip <- cn[isnotdup][order(screens[isnotdup])]
  }
  if (is.character(strip))
    strip <- strip.custom(factor.levels = rep(strip, length(unique(screens))))

  if(is.null(ylab) || length(ylab) == 1) {
    xyplot(fo, panel = panel, groups = factor(col(x)),  
           type = type, default.scales = default.scales, 
           layout = layout, xlab = xlab, ylab = ylab, pch = pch, 
           col = col, lty = lty, lwd = lwd, strip = strip, ...)
  } else {
    ylab <- rep(ylab, length = length(unique(screens)))
    xyplot(fo, panel = panel, groups = factor(col(x)),  
  	   type = type, default.scales = default.scales, 
  	   layout = layout, xlab = xlab, ylab = "", pch = pch, 
  	   col = col, lty = lty, lwd = lwd, outer = TRUE,
  	   strip.left = strip.custom(horizontal = FALSE, 
  	   factor.levels = ylab), strip = TRUE, ...)
  }
}

panel.lines.ts <- 
panel.lines.its <-
panel.lines.zoo <- function(x, ...) {
  x <- as.zoo(x)
  panel.lines(time(x), coredata(x), ...)
}

panel.points.ts <- 
panel.points.its <-
panel.points.zoo <- function(x, ...) {
  x <- as.zoo(x)
  panel.points(time(x), coredata(x), ...)
}

panel.segments.ts <- 
panel.segments.its <-
panel.segments.zoo <- function(x0, x1, ...) {
  x0 <- as.zoo(x0)
  x1 <- as.zoo(x1)
  panel.segments(time(x0), coredata(x0), time(x1), coredata(x1), ...)
}

panel.text.ts <- 
panel.text.its <-
panel.text.zoo <- function(x, ...) {
  x <- as.zoo(x)
  panel.text(time(x), coredata(x), ...)
}

panel.rect.ts <- 
panel.rect.its <-
panel.rect.zoo <- function(x0, x1, ...) {
  x0 <- as.zoo(x0)
  x1 <- as.zoo(x1)
  panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), ...)
}

panel.arrows.ts <- 
panel.arrows.its <-
panel.arrows.zoo <- function(x0, x1, ...) {
  x0 <- as.zoo(x0)
  x1 <- as.zoo(x1)
  panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), ...)
}

panel.polygon.ts <- 
panel.polygon.its <-
panel.polygon.zoo <- function(x, ...) {
  x <- as.zoo(x)
  panel.polygon(time(x), coredata(x), ...)
}
