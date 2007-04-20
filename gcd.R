gcd <- function(x) {	
  gcd0 <- function(a, b) ifelse(b==0 | a==b, a, gcd0(b, a %% b))
  if(length(x) < 2) x <- c(x, as.integer(0))
  if(length(x) < 3) {
    return(gcd0(x[1], x[2]))
  } else {
    x <- sapply(1:(length(x) - 1), function(i) gcd0(x[i], x[i+1]))
    gcd(x)
  }
}


  d <- try(diff(as.numeric(order.by)), silent = TRUE)
  if(class(d) == "try-error" || length(d) < 1 || any(is.na(d))) FALSE
  else {      
      deltat <- min(d)
      dd <- d/deltat
      if(identical(all.equal(dd, round(dd)), TRUE)) {	      
	  freq <- 1/deltat
	  if(freq > 1 && identical(all.equal(freq, round(freq)), TRUE)) freq <- round(freq)
  	  identical(all.equal(frequency %% freq, 0), TRUE)
      } else if(identical(all.equal(d, round(d)), TRUE)) {
  	  freq <- 1/gcd(cumsum(d))
      } else {
  	  FALSE
      }
  }


  delta <- suppressWarnings(try(diff(as.numeric(index(x))), silent = TRUE))
  if(class(delta) == "try-error" || any(is.na(delta))) FALSE
  else if(length(delta) < 1) FALSE
  else if(strict) identical(all.equal(delta, rep.int(delta[1], length(delta))), TRUE)
  else identical(all.equal(delta/min(delta), round(delta/min(delta))), TRUE)

  d <- suppressWarnings(try(diff(as.numeric(index(x))), silent = TRUE))
  reg <- if(class(d) == "try-error" || any(is.na(d))) FALSE
    else identical(all.equal(d/min(d), round(d/min(d))), TRUE)
  if(!reg) return(NULL)

  deltat <- min(d)
  freq <- 1/deltat
  if(freq > 1 && identical(all.equal(freq, round(freq)), TRUE)) freq <- round(freq)
  return(freq)

  ix <- suppressWarnings(try(as.numeric(index(x)), silent = TRUE))
  freqOK <- if(class(ix) == "try-error" || any(is.na(ix))) FALSE
    else if(length(ix) < 2) TRUE
    else identical(all.equal(ix*value, round(ix*value)), TRUE)

  ix <- suppressWarnings(try(as.numeric(index(x)), silent = TRUE))
  d <- diff(ix)

  1/min(d)
