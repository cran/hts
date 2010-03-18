####################################
### Making HTS of desired levels ###
####################################

cut.hts <- function(obj, dl = length(obj$m))
{
    if (class(obj) != "hts")
        stop("Inappropriate class for data")
    if (dl <= 0 | dl > length(obj$m))
        stop("Inappropriate number of levels")
    if (dl == length(obj$m))
        return(obj)
    else if(dl == 1)
        return(hts(y = obj$gma, g = matrix(obj$g[1,], nrow = 1)))
    else
        tmpg <- obj$g[1:dl,]
    obj2 <- hts(y = obj$y, g = tmpg)
    tmpS2 <- Scsr(obj2)
    cum <- cumsum(obj2$m)
    indtmpS2 <- tmpS2[(cum[length(cum) - 1] + 1):cum[length(obj2$m)],]
    gdl <- unique(tmpg, MARGIN = 2)
    y <- ts(as.matrix(obj$y) %*% t(as.matrix(indtmpS2)))
    tsp(y) <- tsp(obj$y)
    return(hts(y = y, g = gdl))
}
