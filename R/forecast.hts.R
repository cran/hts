forecast.gts <- function (object, h, 
    method=c("comb","bu", "mo", "tdgsf", "tdgsa", "tdfp","all"), 
    fmethod=c("ets","rw","arima"),  
    level, positive=FALSE, xreg = NULL, newxreg = NULL, ...) 
{ 
  require(forecast)
  method <- match.arg(method)
  fmethod <- match.arg(fmethod)

  # Check arguments
  if(!is.gts(object))
    stop("Data not hierarchical or grouped time series")
  if(!object$hierarchical & is.element(method,c("mo", "tdgsf", "tdgsa", "tdfp")))
    stop("Method not appropriate for a non-hierarchical time series")
  if(method == "mo" & missing(level))
    stop("Please specify level for middle-out method")
  if(h <= 0)
    stop("Forecast horizon must be positive")

  levels <- length(object$m)
  if(levels == 1)
    method <- "comb"
              
  if(method == "mo")
    return(middleout(data=object,h=h,level=level,positive=positive,fmethod=fmethod,...))

  # Create a matrix of point forecast (p.f.mat) for each series of the hierarchy.
  n.s <- sum(object$m)         # Total number of series in the hierarchy
  tspx <- tsp(object$y)
  p.f.mat <- ts(matrix(NA,h,n.s),frequency=tspx[3],start=tspx[2]+1/tspx[3])   # Point forecast matrix
  fulldata <- allts(object)
  
  if(!positive)
  {
    for (i in 1:n.s)
    {
      if(fmethod == "ets")
        p.f.mat[,i] <- forecast(ets(fulldata[,i]), h=h, additive.only=TRUE,...)$mean
      else if(fmethod == "rw")
        p.f.mat[,i] <- rwf(fulldata[,i],h=h)$mean
      else if(fmethod == "arima")
        p.f.mat[,i] <- forecast(auto.arima(fulldata[,i],xreg=xreg,...),h=h,xreg=newxreg)$mean
      else
        stop(paste(fmethod,"method not yet implemented"))
    }
          
  }
  else # Force forecasts to be positive
  {
    if(min(fulldata,na.rm=TRUE) < 0)
      stop("Some data negative")
    for (i in 1:n.s)
    {
      if(fmethod == "ets")
          p.f.mat[,i] <- exp(forecast(ets(log(fulldata[,i]+1)), h=h,...)$mean)-1
      else if(fmethod == "rw")
          p.f.mat[,i] <- exp(rwf(log(fulldata[,i]+1), h=h)$mean)-1
      else if(fmethod == "arima")
          p.f.mat[,i] <- exp(forecast(auto.arima(log(fulldata[,i]+1),xreg=xreg,...),h=h,xreg=newxreg)$mean)-1
      else
          stop(paste(fmethod,"method not yet implemented"))
    }
  }      

  # Set up information for top down forecasts
  if(substr(method,1,2) == "td" | method == "all")
  {
    c.sum <- scoef.l <- list()
    for (i in 1:(levels-1))
    {
      n.unique <- lapply(split(object$g[i+1,],object$g[i,]),function(x){length(unique(x))})
      c.sum[[i]] <- c(0,cumsum(n.unique))
      scoef.l[[i]] <- matrix(NA,length(unique(object$g[i+1,])),length(unique(object$g[i+1,])))
      for (j in 1:(length(c.sum[[i]])-1))
      {
        scoef.l[[i]][,(c.sum[[i]][j]+1):(c.sum[[i]][j+1])] <- c(rep(0,c.sum[[i]][j]),
                  rep(1,c.sum[[i]][j+1]-c.sum[[i]][j]),
                  rep(0,object$m[[i+1]]-c.sum[[i]][j+1]))
      }
    }   
  }
      
  index <- cumsum(object$m)
  
  if(method == "tdgsf")
    return(td.gsF(object,h,p.f.mat,c.sum,scoef.l))  # Return forecast by Gross and Sohl's Approach "F"
  else if(method == "tdgsa")
    return(td.gsA(object,h,p.f.mat,c.sum,index,scoef.l))  # Return forecast by Gross and Sohl's Approach "A"
  else if(method == "tdfp")
    return(td.rFP(object,h,p.f.mat,c.sum,scoef.l,index)) # Return forecast by Roman's forecasted proportion Approach
  else if(method == "bu") # Bottom up
    return(bup.f(object,h,p.f.mat,index))
  else if(method == "comb") # Regression combination
    return(comb.f(object,h,p.f.mat))
  else if(method == "all" & object$hierarchical) # Return all
    return(list(bu=bup.f(object,h,p.f.mat,index),comb=comb.f(object,h,p.f.mat),
	    		mo = middleout(data=object,h=h,level=level,positive=positive,fmethod=fmethod,...),
		    	tdgsf=td.gsF(object,h,p.f.mat,c.sum,scoef.l),
        		tdgsa=td.gsA(object,h,p.f.mat,c.sum,index,scoef.l),tdfp=td.rFP(object,h,p.f.mat,c.sum,scoef.l,index)))
  else if(method == "all" & !object$hierarchical) # Return all possible
    return(list(bu=bup.f(object,h,p.f.mat,index),comb=comb.f(object,h,p.f.mat)))
}

accuracy.gts <- function(f, x, criterion=c("all", "RMSE", "MAE", "MAPE", "MASE"))
{	
 	if(!is.gts(f) | !is.gts(x))
    stop("f and x should be a grouped time series.")
  if(is.null(f$oldy))
    stop("f should be a forecast grouped time series.")

  criterion = match.arg(criterion)
  if(criterion == "all")
  {
    criterion=c("RMSE", "MAE", "MAPE", "MASE")
  }
  ns <- sum(f$m)
  out <- matrix(NA, nrow = length(criterion), ncol = ns)  
  
  allx <- allts(x)
  allf <- allts(f)
  res <- allx - allf
  if(is.element("MAPE",criterion))
    pe <- res/allx * 100
  if(is.element("MASE",criterion))
  {
    m <- frequency(f$y)
    if(m <= 1)
      scale <- colMeans(diff(allx))
    else
      scale <- colMeans(diff(allx,lag=m))
    q <- sweep(res,2,scale,"/")
  }

  if(length(criterion) == 1)
  {
    out <- switch(criterion,
      RMSE = sqrt(colMeans(res^2,na.rm=TRUE)),
      MAE = colMeans(abs(res),na.rm=TRUE),
      MAPE = colMeans(abs(pe),na.rm=TRUE),
      MASE = colMeans(abs(q), na.rm=TRUE))
    names(out) <- colnames(allf)
  }
  else
 	{
  	out <- rbind(
      RMSE = sqrt(colMeans(res^2,na.rm=TRUE)),
      MAE = colMeans(abs(res),na.rm=TRUE),
      MAPE = colMeans(abs(pe),na.rm=TRUE),
      MASE = colMeans(abs(q), na.rm=TRUE))
    colnames(out) <- colnames(allf)
  }
  return(out)
}

window.gts <- function(x, ...)
{
	x$y <- window(x$y,...)
  x$oldy <- NULL # Just in case this contains forecasts
  x$gma <- ts(rowSums(x$y))
  tsp(x$gma) <- tsp(x$y)
	return(x)
}







