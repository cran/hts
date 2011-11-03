# Compute series at all levels.
allts <- function(obj)
{
    tsp.y <- tsp(obj$y)
    if(is.null(tsp.y))
    {
        browser()
        stop("Not time series data")
    }
    # Compute series at all levels
    S <- Scsr(obj)
    gma <- ts(as.matrix(as.matrix.csr(obj$y) %*% t(S)),start=tsp.y[1],frequency=tsp.y[3])
    dimnames(gma)[[2]] <- group.names(obj)
    return(gma)
}

# Function to generate group names for all levels

group.names <- function(x)
{
  if(is.hts(x))
    return(hier.names(x))
  nl <- nrow(x$g)
  ns <- ncol(x$g)
  gnames <- c("Total")
  if(nl==1)
    return("Total")
  LETNUM <- c(LETTERS,1:1e+05)
  for(i in 2:nl)
    gnames <- c(gnames, paste(LETNUM[i-1],seq(1,x$m[i],by=1), sep=""))
    return(gnames)
}

# Function to generate group names for all levels for hts
hier.names <- function (x) 
{
  nl <- nrow(x$g)
  ns <- ncol(x$g)
  if (nl == 1) 
    return("Total")
  LETNUM <- c(LETTERS, 1:1e+05)
  names <- rep("/", ns)
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
    names <- paste(names, current.names, "/", sep = "")
  }
  splits <- matrix(unlist(gregexpr("/",names)),nrow=ns,byrow=TRUE)
  g.names <- "Total"
  for (i in 2:nl) 
  {
    lnames <- substr(names,2,splits[,i]-1)
    g.names <- c(g.names, unique(lnames))
  }
  return(g.names)
}