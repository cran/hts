\name{accuracy.hts}
\Rdversion{1.1}
\alias{accuracy.hts}
\title{
Accuracy measures for forecast hierarchical model
}
\description{
Returns range of summary measures of the forecast accuracy. When the holdout data are provided, the function measures 
out-of-sample forecast accuracy based on (holdout data - forecasts). All measures are defined and discussed in Hyndman and Koehler (2006). 
}
\usage{
accuracy.hts(f, x)
}
\arguments{
  \item{f}{An object of class \code{\link[hts]{hts}}, containing the forecasted hierarchical time series}
  \item{x}{An object of class \code{\link[hts]{hts}}, containing the holdout hierarchical time series}
}
\value{
Vector giving forecast accuracy measures.
\item{ME}{Mean Error}
\item{RMSE}{Root Mean Square Error}
\item{MAE}{Mean Absolute Error}
\item{MPE}{Mean Percentage Error}
\item{MAPE}{Mean Absolute Percentage Error}
\item{MASE}{Mean Absolute Scale Error}
\item{ACF1}{Autocorrelation Function at Lag 1}
\item{Theil's U}{Theil's U Inequality Index}
}
\references{
H. Theil (1966), Applied economic forecasting, North-Holland, Amsterdam.

R. Hyndman and A. Koehler (2006), Another look at measures of forecast accuracy, \emph{International Journal of Forecasting}, \bold{22}, 679-688.
}
\author{
Rob J Hyndman
}
\seealso{
\code{\link[hts]{hts}}, \code{\link[hts]{plot.hts}}, \code{\link[hts]{forecast.hts}}
}
\examples{
f = forecast(simulatedata1, h = 10, method = "bu")
accuracy.hts(f, holdoutdata)
}
\keyword{error}

