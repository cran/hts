###################################
### Combined Forecast using OLS ###
###################################

# This version returns a gts or hts object
comb.f <- function(data, h, fcasts)
{
  y <- combinef(fcasts, as.matrix(Scsr(data)), return="bottomlevelonly")
  colnames(y) <- colnames(data$y)
  data$oldy <- data$y
  data$y <- y
  return(data)
}

# This version returns the full matrix of forecasts
# fcasts = matrix of forecasts
# S = summing matrix

combinef <- function(fcasts, S, return=c("gts","matrix","bottomlevelonly"), hierarchical=FALSE)
{
  return <- match.arg(return)
  y <- combinebf(fcasts, S)
  if(return == "bottomlevelonly")
    return(y)
  else if(return == "gts")
    return(gts(y,groupsfromS(S),hierarchical=hierarchical))
  else # return==matrix
  {
    ytilde <- y %*% t(S)
    colnames(ytilde) <- colnames(fcasts)
    if(is.ts(fcasts))
    {
      ytilde <- ts(ytilde)
      tsp(ytilde) <- tsp(fcasts)
    }
    return(ytilde)
  }
}

# This version returns the bottom level forecasts

combinebf <- function(fcasts, S)
{
  ytild <- as.matrix(t(qr.fitted(qr(S), t(fcasts))))
  bottom <- nrow(S)  - (ncol(S):1) + 1
  y <- matrix(ytild[, bottom], nrow = nrow(ytild))
  if(is.ts(fcasts))
  {
    y <- ts(y)
    tsp(y) <- tsp(fcasts)
  }
  return(y)
}

