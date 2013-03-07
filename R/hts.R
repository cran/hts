## Function to make grouped time series
## y contains series at completely disaggregated level
## g contains group information (not necessarily hierarchical)

gts <- function(y, g, hierarchical=FALSE)
{
  y <- as.ts(y)
  tsp.y <- tsp(y)
  if(is.null(tsp.y))
    stop("Not time series data")
  y <- as.matrix(y)
  g <- as.matrix(g)
  if(ncol(g) == 1)
    g <- matrix(g, nrow = 1)
  # Check top row
  if(!(sum(g[1,])==ncol(g) & sum(abs(g[1,]-1)) < 1e-8))
    g <- rbind(rep(1,ncol(g)),g)
  # Check last row complete
  bottom <- unique(g[nrow(g),])
  if(length(bottom) != ncol(g))
    g <- rbind(g,1:ncol(g))
  # Remove duplicate rows
  z <- rnorm(ncol(g),0,1)
  z <- g %*% z
  g <- g[!duplicated(z[,1]),]
  
  # Count groups at each level
  m <- apply(g, 1, function(x){length(unique(x))})
  # Add attributes to y
  y <- ts(y, start = tsp.y[1], frequency = tsp.y[3])
  colnames(g) <- colnames(y)
  rownames(g) <- paste("Level", 0:(nrow(g) - 1))
  class(g) <- "gmatrix"
  # Return gts object
  return(structure(list(y = y, g = g, m = m, hierarchical=hierarchical), class = "gts"))
}

## Function to make hierarchical time series
## y contains series at lowest level
## g contains group information assumed to be hierarchical
## This retained for backwards compatibility only.
hts <- function(y, g)
{
  g <- as.matrix(g)
  if(ncol(g) == 1)
    g <- matrix(g, nrow=1)
  g <- make.groups(g)
  return(gts(y, g, hierarchical=TRUE))
}

# Construct hierarchical time series
# data gives the value of the factors for the hierarchy in order.
make.groups <- function(data)
{
  # Check if already created group matrix
  if(is.element("gmatrix",class(data)))
      return(data)
  nv <- nrow(data)
  g <- matrix(1,nrow=nv+1,ncol=ncol(data))
  vars <- rownames(data)
  if (length(vars) == 0)
    vars = 1:nrow(g)
  else
  {
    j <- vars==""
    if(sum(j)>0)
      rownames(data)[j] <- vars[j] <- (1:nrow(g))[j]
  }
  fac <- ""
  for(i in 1:nv)
  {
    fac <- paste(fac,data[vars[i],])
    g[i+1,] <- as.numeric(as.factor(fac))
  }
  return(structure(g,class=c("gmatrix","matrix")))
}

is.gts <- function(x)
{
  is.element("gts",class(x))
}

print.gts <- function(x,...)
{
  fcasts <- !is.null(x$oldy)
  if(x$hierarchical)
    cat("Hierarchical Time Series")
  else
    cat("Grouped Time Series")
  if(fcasts)
    cat(" Forecasts")

  cat("\n  ",length(x$m),"Levels")
  cat("\n   Number of groups at each level:",x$m)
  cat("\n   Total number of series:",sum(x$m))
  if(!fcasts)
    cat("\n   Number of observations per series:",nrow(x$y),"\n")
  else
    cat("\n   Number of observations in each historical series", nrow(x$oldy),"\n")

  cat("\n   Top level series:\n")
  print(toplevel(x,forecasts=FALSE))

  if(fcasts)
  {
    cat("\n   Number of forecasts per series:", nrow(x$y), "\n")    
    cat("\n   Top level series of forecasts:\n")
    print(toplevel(x))
  }


}

 # Construct top level series
 # If forecasts=FALSE, return historical values.
toplevel <- function(object, forecasts=TRUE)
{ 
  if(!forecasts & !is.null(object$oldy))
    y <- object$oldy
  else
    y <- object$y
  tsp.y <- tsp(y)
  return(ts(rowSums(y), start = tsp.y[1], frequency = tsp.y[3]))
}