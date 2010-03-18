# Fix group matrix so it has the lowest possible integers
fix.groups <- function(g)
{
    if(class(g)=="numeric")
        g <- matrix(g,ncol=1)
    for(i in 1:nrow(g))
        g[i,] <- as.numeric(as.factor(g[i,]))
    return(g)
}
