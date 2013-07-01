middleout <- function (data, h, level, positive = FALSE, fmethod, ...) 
{
  # Simple special cases at top and bottom levels
  if (level == length(data$m)) 
    return(forecast.gts(data, h = h, positive = positive, method = "bu", fmethod = fmethod, ...))
  else if (level == 1) 
    return(forecast.gts(data, h = h, positive = positive, method = "tdfp", fmethod = fmethod, ...))
  else if (level < 1 | level > length(data$m))
    stop("level outside the allowable range")

# Proceed if genuinely at middle level
  ntrees <- data$m[level]
  fcast <- NULL
  if(h == 1)
  {
    for (i in 1:ntrees) 
    {
      j <- data$g[level,] == i
      x <- hts(data$y[,j], fix.groups(data$g[level:nrow(data$g),j]))
      fcast <- matrix(c(fcast, forecast(x, h = h, positive = positive, 
         method = "tdfp", fmethod = fmethod, ...)$y), nrow = 1)
    }
  }
  if(h>1)
  {
    for (i in 1:ntrees) 
    {
      j <- data$g[level, ] == i
      x <- hts(data$y[, j], fix.groups(data$g[level:nrow(data$g),j]))            
      fcast <- cbind(fcast, forecast(x, h = h, positive = positive, 
          method = "tdfp", fmethod = fmethod, ...)$y)
    }
  }

# Construct result
  fcast <- ts(fcast, frequency = frequency(data$y), start = tsp(data$y)[2] + 1/frequency(data$y))
  colnames(fcast) <- colnames(data$y)
  data$oldy <- data$y
  data$y <- fcast
  return(data)
}


# Fix group matrix so it has the lowest possible integers
fix.groups <- function(g)
{
  if(class(g) == "numeric")
    g <- matrix(g, ncol=1)
  for(i in 1:nrow(g))
    g[i,] <- as.numeric(as.factor(g[i,]))
  return(g)
}
