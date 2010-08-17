##############################################################################
### Conventional Top-down Forecast based on Method "F" from Gross and Sohl ###
##############################################################################

td.gsF <- function(data,h,p.f.mat,c.sum,scoef.l) 
{ 
    # Method "F" from Gross and Sohl implemented here
    level.ave <- split(matrix(colMeans(allts(data)),nrow=1),rep(1:length(data$m),data$m))
    tdm2.l <- list(matrix(p.f.mat[,1],ncol=1))
    if(length(data$m)>1)
    {
        for (j in 2:length(data$m))
        {
            tmp.mat <- matrix(NA,h,data$m[[j]])
            for (i in 1:(length(c.sum[[j-1]])-1))
                tmp.mat[,(c.sum[[j-1]][i]+1):c.sum[[j-1]][i+1]] <- tdm2.l[[j-1]][,i]
            ppl.con <- matrix(level.ave[[j]]/(level.ave[[j]]%*%scoef.l[[j-1]]),h,data$m[[j]],byrow=T)
            tdm2.l[[j]] <- tmp.mat * ppl.con
        }
    }
    tdm2.l <- ts(tdm2.l[[length(data$m)]],start=tsp(data$y)[2]+1/frequency(data$y),f=frequency(data$y))
    colnames(tdm2.l) <- colnames(data$y)
    return(hts(y=tdm2.l, g=data$g))
} 

