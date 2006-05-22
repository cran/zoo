aggregate.zoo <- function(x, by, FUN, ...)
{
    my.unique <- function(x) x[MATCH(x, x) == seq(length = length(x))]
    my.sort <- function(x) x[order(x)]
    if (is.function(by)) 
        by <- by(index(x))
    if (!is.list(by)) 
        by <- list(by)
    stopifnot(length(time(x)) == length(by[[1]]))
    df <- aggregate(coredata(x), by, FUN, ...)
    row.names(df) <- df[, 1]
    df <- df[, -1]
    if (is.matrix(x)) 
        df <- as.matrix(df)
    rval <- zoo(df, my.sort(my.unique(by[[1]])))
    if (is.regular(rval)) {
        rval <- zoo(df, my.sort(my.unique(by[[1]])), frequency(rval))
    }
    return(rval)
}
