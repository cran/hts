# Function to generate group names for all levels
hier.names <- function (x) 
{
    nl <- nrow(x$g)
    ns <- ncol(x$g)
    if (nl == 1) 
        return("Total")
    LETNUM <- c(LETTERS, 1:1e+05)
    names <- rep("A", ns)
    for (i in 2:nl) 
	{
        groups <- unique(x$g[i - 1, ])
        current.names <- numeric(length(names))
        for (j in c(groups)) 
		{
			k <- (x$g[i - 1, ] == j)
            tmp <- x$g[i, k]
            current.names[k] <- LETNUM[tmp - min(tmp) + 1]
        }
        names <- paste(names, current.names, sep = "")
    }
    g.names <- "Total"
    for (i in 2:nl) 
		g.names <- c(g.names, unique(substr(names, 2, i)))
    return(g.names)
}