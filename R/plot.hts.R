# Function to plot grouped and hierarchical time series & forecasts
# Inputs:
#   x = grouped time series 
#   levels = number of levels to plot (default is all)
#   labels indicates whether to label each series plotted.
#   include = number of historical observations to include in the plot

plot.gts <- function (x, include, levels = length(x$m), labels = TRUE, ...) 
{
  if (missing(include)) 
  {
    if(is.null(x$oldy)) # Not forecasts
      include <- end(x$y)[1] - start(x$y)[1] + 1
    else
      include <- end(x$oldy)[1] - start(x$oldy)[1] + 1
  }
  if (include < 0)
    stop("The number of historical time series is negative")
  if (!is.gts(x)) 
    stop("Inappropriate class for data")
  if (levels < 1) 
    stop("levels must be at least 1")
  else if (levels > length(x$m)) 
    stop("levels too large")
#  if (length(x$m) == 1) 
#  {
#    plot.ts(x$gma, main = "Level 0", ylab = "", ...)
#    return(invisible())
#  }

  fcasts <- !is.null(x$oldy) & (include > 0)
  if(fcasts)
  {
    gma <- allts(x, forecasts=FALSE)
    fgma <- allts(x, forecasts=TRUE)
    h <- nrow(fgma)
  }
  else
    gma <- allts(x)
  if(include > 0)
    gma <- window(gma, start=end(gma)[1] - include + 1) 

  cm <- c(0, cumsum(x$m))
  oldpar <- par(mfrow = c(levels, 1), mar = c(3,4,4,2))
  on.exit(par(oldpar))

  for (i in 1:levels) 
  {
    if (x$m[i] > 1) 
      cols <- rainbow(min(1024, x$m[i]))
    else
      cols <- 1
    series <- cm[i] + (1:x$m[i])
    ns <- length(series)
    if (fcasts) 
    {
      ylim <- range(gma[, series], fgma[, series])
      xlim <- range(time(gma),time(fgma))
    }
    else
    {
      ylim <- range(gma[, series])
      xlim <- range(time(gma))
    }
    plot(gma[, series,drop=FALSE], xlim=xlim, ylim=ylim, col = cols, ylab="",
       main = paste("Level", i - 1), plot.type = "single", type=ifelse(include==1,"p","l"),...)
    if(fcasts)
    {
      for (j in 1:ns) 
        lines(fgma[, series[j], drop=FALSE], col = cols[j], lty = 2, type=ifelse(h==1,"p","l"))
    }
    if (i > 1 & labels) 
    {
      text(rep(tsp(gma)[1], x$m[i]) - 0.1/tsp(gma)[3], 
      gma[1, series], dimnames(gma)[[2]][series], cex = 0.7, adj = 1)
    }
  }
}
