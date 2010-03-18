###################################
### Combined Forecast using OLS ###
###################################

comb.f <- function(data,h,p.f.mat)
{
#    require(Matrix)
    ytild <- ts(as.matrix(t(qr.fitted(qr(as.matrix(Scsr(data))),t(p.f.mat)))),start=end(data$y)[1]+1,f=frequency(data$y)) 
    y <- ts(matrix(ytild[,(cumsum(data$m)[length(data$m)-1]+1):cumsum(data$m)[length(data$m)]],nr=nrow(ytild)),start=start(ytild)[1], f=frequency(ytild))
    colnames(y) <- colnames(data)
    return(hts(y=y,g=data$g))
}
