#################################
### Making S (summing) matrix ###
#################################


Smatrix <- function(object)
{
  return(as.matrix(Scsr(object)))
}


Scsr <- function(x)
{
  require(SparseM)
  ns <- ncol(x$y)  # number of series
  nl <- nrow(x$g)  # number of levels
  l <- sum(x$m)    # total number of series
  ra <- rep(1,nl*ns)
  ja <- jaslot(x)
  ia <- iaslot(x)
  return(new("matrix.csr", ra=as.numeric(ra), ja=as.integer(ja), ia=as.integer(ia),
      dimension=as.integer(c(l,ns))))
}


iaslot <- function(x)
{
  g <- apply(x$g,1,table)
  for(i in 1:length(g))
    g[[i]] <- g[[i]][unique(x$g[i,])]
  return(cumsum(c(1,unlist(g))))
}


###### Creating ja slot for Sparse S matrix ##########

jaslot <- function(x)
{
  g <- x$g
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
      for (j in 1:ncol(x$g))
      {
        if(g[k,j]==un[i]) 
          tmp[j] <- j
      }
      str[[i]] <- as.vector(na.exclude(tmp))
    }
    ja.index[[k]] <- unlist(str)
  }
  ja <- unlist(ja.index)
  return(ja)
}


groupsfromS <- function(S)
{
  j <- k <- 1 # i=level, j=row of S, k=group within level
  i <- 0
  g <- NULL # group matrix
  tmp <- rep(1,ncol(S)) # sum of S rows for current level. Should be a 1-vector when level complete.
  while(j <= nrow(S))
  {
    if(sum(abs(tmp-1)) < 1e-10) # Last level complete
    {
      g <- rbind(g, S[j,])
      tmp <- S[j,]
      k <- 2
      i <- i + 1
    }
    else # Still constructing incomplete level
    {
      g[i,] <- g[i,] + S[j,]*k
      k <- k+1
      tmp <- tmp + S[j,]
    }
    j <- j+1
  }
  #class(g) <- c("gmatrix","matrix")
  rownames(g) <- paste("Level",(1:nrow(g))-1)
  return(g)
}