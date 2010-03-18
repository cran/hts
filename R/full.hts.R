full.hts <- function(obj)
{
    tsp.y <- tsp(obj$y)
    if(is.null(tsp.y))
    {
        browser()
        stop("Not time series data")
    }
    # Compute series at all levels
    S <- Scsr(hts(y=obj$y,g=obj$g))
    gma <- ts(as.matrix(as.matrix.csr(obj$y) %*% t(S)),start=tsp.y[1],frequency=tsp.y[3])
    # hier.names function can generate 26 letters for naming. If number of split is greater than 26 we have
    #to pass following two line of the function as comment
    g.names <- hier.names(list(y=obj$y,g=obj$g))
    dimnames(gma)[[2]] <- g.names
    return(structure(list(y=obj$y,g=obj$g,m=obj$m, gma=gma),class="hts"))
}
