###### Creating ja slot for Sparse S matrix ##########

jaslot <- function(hts)
{
    g <- hts$g
    ja.index <- list()
    for (k in 1:nrow(g))
    {
        un <- unique(g[k,])
        lu <- length(un)
        cl <- ncol(g)
        str <- list()
        for(i in 1:lu)
        {
            tmp <- c(NA)
            for (j in 1:ncol(hts$g))
                if(g[k,j]==un[i]) 
                    tmp[j] <- j
            str[[i]] <- as.vector(na.exclude(tmp))
        }
        ja.index[[k]] <- unlist(str)
    }
    ja <- unlist(ja.index)
    return(ja)
}




   
