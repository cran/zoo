aggregate.zoo <- function(x, by, FUN, ...)
{
	my.unique <- function(x) x[MATCH(x,x) == seq(length = length(x))]
	if (!is.list(by)) by <- list(by)
	stopifnot( length(time(x)) == length(by[[1]]) )
	df <- aggregate(unclass(x), by, FUN, ...)
	row.names(df) <- df[,1]
	df <- df[,-1]
	if (is.matrix(x)) df <- as.matrix(df)
	zoo(df, my.unique(by[[1]]))
}
