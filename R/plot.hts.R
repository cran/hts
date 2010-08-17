# Function to plot hierarchical time series & forecasts
# Inputs:
#   x = hierarchical time series or output from heir.fcast
#   levels = number of levels to plot (default is all)
#   labels indicates whether to label each series plotted.
plot.hts <- function(x,levels=length(x$m),labels=TRUE,...)
{
    if(class(x)!="hts")
        stop("Inappropriate class for data")
    fcasts <- !is.null(x$hfcast)
    if(levels < 1)
        stop("levels must be at least 1")
    else if(levels > length(x$m))
        stop("levels too large")
        
    if(length(x$m)==1)
    {  
        par(mfrow=c(1,1))
        plot.ts(x$gma,main="Level 0",ylab="",...)
        return(invisible())
    }

    gma <- allts(x)

    cm <- c(0,cumsum(x$m))
    par(mfrow=c(levels,1))
    par(mar=c(3,4,4,2))    
    for(i in 1:levels)
    {
        if(x$m[i]>1)
            palette(rainbow(x$m[i]))
        if(fcasts)
            addon <- matrix(NA,nrow=nrow(x$hfcast),ncol=x$m[i])
        else
            addon <- numeric(0)
        series <- cm[i]+(1:x$m[i])
        ylim <- range(gma[,series])
        if(fcasts)
            ylim <- range(ylim,x$hfcast[,series],x$rawfcast[,series])
        plot.ts(ts(rbind(as.matrix(gma[,series]),addon),start=start(x$y),frequency=frequency(x$y)),
            col=1:x$m[i],main=paste("Level",i-1),plot.type="single",ylim=ylim,ylab="",...)
        if(fcasts)
        {
            ns <- length(series)
            for(j in 1:ns)
            {
                lines(x$hfcast[,series[j]],col=j,lty=2,lwd=2)
                lines(x$rawfcast[,series[j]],col=j,lty=2)
            }
        }
        if(i>1 & labels)
            text(rep(tsp(gma)[1],x$m[i])-.1/tsp(gma)[3],
                gma[1,series],dimnames(gma)[[2]][series],cex=0.7,adj=1)
    }
    palette("default")
}

