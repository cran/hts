## Function to make hts
## y contains series at lowest level
## g contains group information

hts <- function(y, g)
{
    tsp.y <- tsp(y)
    if(is.null(tsp.y))
        stop("Not time series data")
    y <- as.matrix(y)
    if(ncol(g) == 1)
        g <- matrix(1, ncol = 1, nrow = nrow(g))
    else
        g <- make.groups(g)
    g <- as.matrix(g)
    levels <- nrow(g)
    m <- apply(g,1,function(x){length(unique(x))})
    gma <- ts(as.matrix(rowSums(y)),start=start(y),f=frequency(y))
    return(structure(list(y=y,g=g,levels=levels,m=m,gma=gma),class="hts"))
}
