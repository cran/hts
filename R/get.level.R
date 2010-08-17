get.level <- function(data,level)
{
    x <- allts(data)
    j <- rep(1:length(x$m),x$m)
    return(x[,j==level])
}
