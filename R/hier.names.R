# Function to generate group names for all levels
hier.names <- function(x)
{
    nl <- nrow(x$g)
    ns <- ncol(x$g)
    if(nl==1)
        return("Total")

    LETNUM <- c(LETTERS,1:100000)

    names <- rep("A",ns)
    for(i in 2:nl)
    {
        groups <- unique(x$g[i-1,])
        current.names <- numeric(0)
        for(j in c(groups))
        {
            tmp <- x$g[i,x$g[i-1,]==j]
            current.names <- c(current.names,LETNUM[tmp-min(tmp)+1])
        }
        names <- paste(names,current.names,sep="")
    }
    g.names <- "Total"
    for(i in 2:nl)
        g.names <- c(g.names,unique(substr(names,2,i)))

    return(g.names)
}
