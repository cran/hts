# Construct hierarchical time series
# data gives the value of the factors for the hierarchy in order.
make.groups <- function(data)
{
    # Check if already created group matrix
    if(is.element("gmatrix",class(data)))
        return(data)
    vars <- rownames(data)
    nv <- length(vars)
    g <- matrix(1,nrow=nv+1,ncol=ncol(data))
    fac <- ""
    for(i in 1:nv)
    {
        fac <- paste(fac,data[vars[i],])
        g[i+1,] <- as.numeric(as.factor(fac))
    }
    return(structure(g,class=c("gmatrix","matrix")))
}

