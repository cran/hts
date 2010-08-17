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
    m <- apply(g, 1, function(x){
               length(unique(x))}
               )
    gma <- ts(as.matrix(rowSums(y)), start = tsp.y[1], frequency = tsp.y[3])
    y <- ts(y, start = tsp.y[1], frequency = tsp.y[3])
    colnames(g) <- colnames(y)
    rownames(g) <- paste("Level", 0:(nrow(g) - 1))
    return(structure(list(y = y, g = g, m = m, gma = gma), class = "hts"))
}
