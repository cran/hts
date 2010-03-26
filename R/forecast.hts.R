forecast.hts <- function (object, h, fmethod=c("ets","rw","arima"), 
    method=c("comb","bu", "mo", "tdgsf", "tdgsa", "tdfp","all"), 
    level, positive=FALSE, xreg = NULL, newxreg = NULL, ...) 
{ 

    require(forecast)
    if(method=="mo" & missing(level))
        stop("Please specify level for middle-out method")
    
    # Check arguments
    if(class(object) != "hts")
        stop("Data not hierarchical time series")
    if(h <= 0)
        stop("Forecast horizon must be positive")
    method <- match.arg(method)
    fmethod <- match.arg(fmethod)
    levels <- length(object$m)
    if(levels == 1)
        method <- "comb"
                
    if(method=="mo")
        return(middleout(data=object,h=h,level=level,positive=positive,fmethod=fmethod,...))

    # Create a matrix of point forecast (p.f.mat) for each series of the hierarchy.
    n.s <- sum(object$m)         # Total number of series in the hierarchy
    p.f.mat <- matrix(NA,h,n.s)   # Point forecast matrix
    fulldata <- full.hts(object)
    
    if(!positive)
    {
        for (i in 1:n.s)
        {
            if(fmethod=="ets")
                p.f.mat[,i] <- forecast(ets(fulldata$gma[,i]), h=h, additive.only=TRUE,...)$mean
            else if(fmethod=="rw")
                p.f.mat[,i] <- rwf(fulldata$gma[,i],h=h)
            else if(fmethod=="arima")
                p.f.mat[,i] <- forecast(auto.arima(fulldata$gma[,i],xreg=xreg,...),h=h,xreg=newxreg)$mean
            else
                stop(paste(fmethod,"method not yet implemented"))
        }
            
    }
    else # Force forecasts to be positive
    {
        if(min(fulldata$gma,na.rm=TRUE) < 0)
            stop("Some data negative")
        for (i in 1:n.s)
        {
            if(fmethod=="ets")
                p.f.mat[,i] <- exp(forecast(ets(log(fulldata$gma[,i]+1)), h=h,...)$mean)-1
            else if(fmethod=="rw")
                p.f.mat[,i] <- exp(rwf(log(fulldata$gma[,i]+1), h=h)$mean)-1
            else if(fmethod=="arima")
                p.f.mat[,i] <- exp(forecast(auto.arima(log(fulldata$gma[,i]+1),xreg=xreg,...),h=h,xreg=newxreg)$mean)-1
            else
                stop(paste(fmethod,"method not yet implemented"))
        }
    }      

    # Set up information for top down forecasts
    if(substr(method,1,2)=="td" | method=="all")
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
    
    if(method=="tdgsf")
        return(td.gsF(object,h,p.f.mat,c.sum,scoef.l))  # Return forecast by Gross and Sohl's Approach "F"
    else if(method=="tdgsa")
        return(td.gsA(object,h,p.f.mat,c.sum,index,scoef.l))  # Return forecast by Gross and Sohl's Approach "A"
    else if(method=="tdfp")
        return(td.rFP(object,h,p.f.mat,c.sum,scoef.l,index)) # Return forecast by Roman's forecasted proportion Approach
    else if(method=="bu") # Bottom up
        return(bup.f(object,h,p.f.mat,index))
    else if(method=="comb") # Regression combination
        return(comb.f(object,h,p.f.mat))
    else if(method=="all") # Return all
        return(list(bu=bup.f(object,h,p.f.mat,index),comb=comb.f(object,h,p.f.mat),tdgsf=td.gsF(object,h,p.f.mat,c.sum,scoef.l),
            tdgsa=td.gsA(object,h,p.f.mat,c.sum,index,scoef.l),tdfp=td.rFP(object,h,p.f.mat,c.sum,scoef.l,index)))
}
