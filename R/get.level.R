get.level <- function(data,level)
{
    x <- full.hts(data)
    j <- rep(1:length(x$m),x$m)
    return(x$gma[,j==level])
}
