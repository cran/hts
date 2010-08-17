#################################################################
### Top-down forecast by Roman's Forecast Proportion Approach ###
#################################################################

td.rFP <- function(data,h,p.f.mat,c.sum,scoef.l,index) 
{ 
    # For calculating proportion we need the point forecast matrix at each level (p.f.mat.l)
    # and the proportion per level (ppl) of the hierarchy
    # and the top-down matrix per level (tdm.l) of the hierarchy
    tdm.l <- list(matrix(p.f.mat[,index[1]],h,1))
    if(length(data$m)>1)
    {
        for (j in 2:length(data$m))
        {
            tmp.mat <- matrix(NA,h,data$m[j])
            for (i in 1:ncol(tdm.l[[j-1]]))
                tmp.mat[,(c.sum[[j-1]][i]+1):(c.sum[[j-1]][i+1])] <- tdm.l[[j-1]][,i]
            p.f.mat.l <- p.f.mat[,(index[j-1]+1):index[j]]
            ppl.gs <- p.f.mat.l/(p.f.mat.l%*%scoef.l[[j-1]])
            tdm.l[[j]] <- tmp.mat*ppl.gs
        }
    }
    # The top-down matrix according to the proportion calculated by Gross and Sohl's approach
    tdm.l <- ts(tdm.l[[length(data$m)]],start=tsp(data$y)[2]+1/frequency(data$y),
		f=frequency(data$y))
    tdm.l[is.na(tdm.l)] <- 0
    colnames(tdm.l) <- colnames(data$y)
    return(hts(y=tdm.l,g=data$g))
} 

