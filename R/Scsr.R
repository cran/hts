#################################
### Making S (summing) matrix ###
#################################

Scsr <- function(x)
{
    require(SparseM)
    ns <- ncol(x$y)  # number of series
    nl <- nrow(x$g)  # number of levels
    l <- sum(x$m)    # total number of series
    ra <- rep(1,nl*ns)
    ja <- jaslot(x)
    ia <- cumsum(c(1,unlist(apply(x$g,1,table))))
    return(new("matrix.csr", ra=as.numeric(ra), ja=as.integer(ja), ia=as.integer(ia),
        dimension=as.integer(c(l,ns))))
}
