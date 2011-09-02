## Function to make grouped time series
## y contains series at completely disaggregated level
## g contains group information (not necessarily hierarchical)

gts <- function(y, g)
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
  # Construct top level series
  gma <- ts(rowSums(y), start = tsp.y[1], frequency = tsp.y[3])
  # Add attributes to y
  y <- ts(y, start = tsp.y[1], frequency = tsp.y[3])
  colnames(g) <- colnames(y)
  rownames(g) <- paste("Level", 0:(nrow(g) - 1))
  class(g) <- "gmatrix"
  # Return gts object
  return(structure(list(y = y, g = g, m = m, gma = gma), class = "gts"))
}

## Function to make hierarchical time series
## y contains series at lowest level
## g contains group information assumed to be hierarchical

hts <- function(y, g)
{
  g <- as.matrix(g)
  if(ncol(g) == 1)
    g <- matrix(g, nrow=1)
  g <- make.groups(g)
  y <- gts(y, g)
  class(y) <- c("hts","gts")
  return(y)
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

is.hts <- function(x)
{
  is.element("hts",class(x))
}

is.gts <- function(x)
{
  is.element("gts",class(x))
}

print.hts <- function(x,...)
{
  cat("Hierarchical Time Series")
  cat("\n  ",length(x$m),"Levels")
  cat("\n   Number of series at each level:",x$m)
  cat("\n   Total number of series:",sum(x$m))
  cat("\n   Number of observations per series:",nrow(x$y),"\n")
  cat("\n   Top level series:\n")
  print(x$gma)
}

print.gts <- function(x,...)
{
  cat("Grouped Time Series")
  cat("\n  ",length(x$m),"Levels")
  cat("\n   Number of groups at each level:",x$m)
  cat("\n   Total number of series:",sum(x$m))
  cat("\n   Number of observations per series:",nrow(x$y),"\n")
  cat("\n   Top level series:\n")
  print(x$gma)
}

####################################
### Making HTS of desired levels ###
####################################

# cut.hts <- function(obj, dl = length(obj$m))
# {
    # if (!is.hts("hts"))
        # stop("Inappropriate class for data")
    # if (dl <= 0 | dl > length(obj$m))
        # stop("Inappropriate number of levels")
    # if (dl == length(obj$m))
        # return(obj)
    # else if(dl == 1)
        # return(hts(y = obj$gma, g = matrix(obj$g[1,], nrow = 1)))
    # else
        # tmpg <- obj$g[1:dl,]
    # obj2 <- hts(y = obj$y, g = tmpg)
    # tmpS2 <- Scsr(obj2)
    # cum <- cumsum(obj2$m)
    # indtmpS2 <- tmpS2[(cum[length(cum) - 1] + 1):cum[length(obj2$m)],]
    # gdl <- unique(tmpg, MARGIN = 2)
    # y <- ts(as.matrix(obj$y) %*% t(as.matrix(indtmpS2)))
    # tsp(y) <- tsp(obj$y)
    # return(hts(y = y, g = gdl))

# }

# get.level <- function(data,level)
# {
    # x <- allts(data)
    # j <- rep(1:length(x$m),x$m)
    # return(x[,j==level])
# }
