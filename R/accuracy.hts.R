accuracy.hts <- function(f, x)
{
    if(!is.hts(f) | !is.hts(x))
        stop("f and x should both be hierarchical time series")
    ns <- ncol(f$y)
    out <- matrix(NA, nrow = 8, ncol = ns)
    xx <- window(x$y, start = tsp(f$y)[1], end = tsp(f$y)[2])
    for(i in 1:ns)
    {
        ff <- list(mean = f$y[,i], x = x$y[,i])
        out[,i] <- accuracy(ff, xx[,i])
    }
    colnames(out) <- colnames(f$y)
    rownames(out) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "Theil's U")
    return(out)
}
