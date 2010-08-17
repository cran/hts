##############################################################################
### Conventional Top-down Forecast based on Method "A" from Gross and Sohl ###
##############################################################################

td.gsA <- function(data,h,p.f.mat,c.sum,index,scoef.l) 
{ 
    # Method "A" from Gross and Sohl implemented here
    # Construct in-sample proportion matrix
    fulldata <- allts(data)
    insamp.porps <- matrix(NA,nr=nrow(fulldata), nc=ncol(fulldata))
    for(i in 1: nrow(insamp.porps))
    {
        insamp.porps[i,1] <- 1
        if(length(data$m)>1)
            for (j in 2:data$levels)
            {
                insamp.porps[i,((index[j-1]+1):(index[j]))] <- split(fulldata[i,],rep(1:length(data$m),data$m))[[j]]/
                                                            split(fulldata[i,],rep(1:length(data$m),data$m))[[j]]%*%scoef.l[[j-1]]
            }
    }
    propmat <- matrix(rep(colMeans(insamp.porps),h),h, byrow=T)
    tdmA.l <- list(matrix(p.f.mat[,1],ncol=1))
    if(length(data$m)>1)
    {
        for (j in 2:length(data$m))
        {
            tmp.mat <- matrix(NA,h,data$m[[j]])
            for (i in 1:(length(c.sum[[j-1]])-1))
                tmp.mat[,(c.sum[[j-1]][i]+1):c.sum[[j-1]][i+1]] <- tdmA.l[[j-1]][,i]
            ppl.conA <- propmat[,((index[j-1]+1):(index[j]))]
            tdmA.l[[j]] <- tmp.mat * ppl.conA
        }
    }
    tdmA.l <- ts(tdmA.l[[length(data$m)]],start=tsp(data$y)[2]+1/frequency(data$y),,f=frequency(data$y))
    colnames(tdmA.l) <- colnames(data$y)
    return(hts(y=tdmA.l, g=data$g))
}
