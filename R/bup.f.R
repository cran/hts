############################
###### Bottom-up Code ######
############################


bup.f <- function(data,h,p.f.mat,index)
{
    a <- length(index)
    p.f.m.bu <- ts(p.f.mat[,(index[a-1]+1):index[a]],
                    start=end(data$y)[1]+1,f=frequency(data$y))
    colnames(p.f.m.bu) <- colnames(data$y)
    return(hts(y=p.f.m.bu,g=data$g))
}

