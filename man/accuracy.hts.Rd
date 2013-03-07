\name{accuracy.gts}
\Rdversion{1.1}
\alias{accuracy.gts}
\title{
Accuracy measures for forecast grouped and hierarchical model
}
\description{
Returns range of summary measures of the forecast accuracy. The function measures 
out-of-sample forecast accuracy based on (holdout data - forecasts). All measures are defined and discussed in Hyndman and Koehler (2006). 
}
\usage{	
accuracy.gts(f, x, criterion = c( "all","RMSE","MAE","MAPE","MASE"))
}
\arguments{
  \item{f}{An object of class \code{\link[hts]{gts}}, containing the forecasted hierarchical time series}
  \item{x}{An object of class \code{\link[hts]{gts}}, containing the holdout hierarchical time series}
  
  \item{criterion}{Forecast error criterion.} 
}
\value{
Vector (or matrix if \code{criterion=="all"}) giving forecast accuracy measures.
\item{RMSE}{Root Mean Square Error}
\item{MAE}{Mean Absolute Error}
\item{MAPE}{Mean Absolute Percentage Error}
\item{MASE}{Mean Absolute Scaled Error}
}
\details{
  MASE calculation is scaled using MAE of in-sample naive forecasts for non-seasonal time series, and in-sample seasonal naive forecasts for seasonal time series.
}
\references{
R. J. Hyndman and A. Koehler (2006), Another look at measures of forecast accuracy, \emph{International Journal of Forecasting}, \bold{22}, 679-688.
}
\author{
Rob J Hyndman
}
\seealso{
\code{\link[hts]{hts}}, \code{\link[hts]{plot.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[forecast]{accuracy}}
}
\examples{
data <- window(htseg1, start = 1992, end = 1999)
test <- window(htseg1, start = 2000, end = 2001)
forecast <- forecast(data, h = 2, method = "bu")
accuracy.gts(f = forecast, x = test)
}
\keyword{error}

