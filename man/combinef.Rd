\name{combinef}
\Rdversion{1.1}
\alias{combinef}
\title{Optimally combine forecasts from a hierarchical time series}
\description{
Using the method of Hyndman et al. (2011), this function optimally combines the forecasts at all levels of a hierarchical time series. The \code{\link{forecast.gts}} calls this function when the \code{comb} method is selected.
}
\usage{
combinef(fcasts, S, return=c("gts","matrix","bottomlevelonly"), hierarchical=FALSE)
}
\arguments{
  \item{fcasts}{Matrix of forecasts for all levels of the hierarchical time series. Each row represents one forecast horizon and each column represents one time series from the hierarchy.}
  \item{S}{The summing matrix defining how the forecasts at the bottom level are summed to obtain each time series in the hierarchy.}
  \item{return}{determines what type and class of object to return.}
  \item{hierarchical}{indicates if the grouped time series should be hierarchical. Ignored if \code{return!="gts"}.}
}
\value{Either an object of class \code{gts} or a matrix of forecasts. If a matrix is returned, it can be the same  size as \code{fcasts} (if \code{return=="matrix"}) or just the bottom-level forecasts (if \code{return=="bottomlevelonly"}).}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}

\author{Rob J Hyndman and Roman A Ahmed}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{forecast.gts}}
}
\keyword{ts}

\examples{
# Construct data
abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
y <- gts(abc, rbind(c(1,1,2,2), c(1,2,1,2)))
h <- 12 
ally <- allts(y)
allf <- matrix(NA,nrow=h,ncol=ncol(ally))
for(i in 1:ncol(ally))
	allf[,i] <- forecast(auto.arima(ally[,i]),h=h)$mean
allf <- ts(allf, start=51)
y.f <- combinef(allf,Smatrix(y), return="gts")
plot(y.f)
}
