\name{forecast.hts}
\Rdversion{1.1}
\alias{forecast.hts}
\title{Forecast a hierarchical time series}
\description{
Methods for forecasting hierarchical time series. 
}
\usage{
forecast.hts(object, h, fmethod = c("ets", "rw", "arima"), method = c("comb", "bu", 
 "mo", "tdgsf", "tdgsa", "tdfp", "all"), level, positive = FALSE, xreg = NULL, 
  newxreg = NULL, ...)
}
\arguments{
  \item{object}{Hierarchical time series object of class \code{\link[hts]{hts}}}
  \item{h}{Forecast horizon}
  \item{fmethod}{Forecasting method to use}
  \item{method}{Method for distributing forecasts within the hierarchy. See details}
  \item{level}{Level used for "middle-out" method (only used when \code{method="mo"})}
  \item{positive}{If TRUE, forecasts are forced to be strictly positive}
  \item{xreg}{When \code{fmethod = "arima"}, a vector or matrix of external regressors, which must have the same number of rows as the original univariate time series}
  \item{newxreg}{When \code{fmethod = "arima"}, a vector or matrix of external regressors, which must have the same number of rows as the original univariate time series}
  \item{...}{Other arguments passing to \code{\link[forecast]{ets}} or \code{\link[forecast]{auto.arima}}}
}
\value{A hierarchical time series of class \code{\link[hts]{hts}}.}
\details{
Base methods implemented include ETS, ARIMA and the naive (random walk) models.
Forecasts are distributed in the hierarchy using bottom-up, top-down, middle-out and 
optimal combination methods. Three top-down methods are available: the two Gross-Sohl 
methods and the forecast-proportion approach of Hyndman, Ahmed, and Athanasopoulos (2009).
The "middle-out" method \code{"mo"} uses bottom-up (\code{"bu"}) for levels higher than \code{level}
and top-down forecast proportions (\code{"tdfp"}) for levels lower than \code{level}.
}
\references{
G. Athanasopoulos, R. A. Ahmed and R. J. Hyndman (2009) Hierarchical forecasts for Australian domestic tourism, 
\emph{International Journal of Forecasting}, \bold{25}, 146-166.

R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2010) 
Optimal combination forecasts for hierarchical time series. 
Working paper, Monash University. 
\url{http://robjhyndman.com/papers/hierarchical}

Gross, C. and Sohl, J. (1990) Dissagregation methods to expedite product line forecasting, 
\emph{Journal of Forecasting}, \bold{9}, 233-254.
}

\author{Rob J Hyndman and Roman A Ahmed}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{plot.hts}}, \code{\link[hts]{accuracy.hts}}
}
\examples{
forecast(htseg1, h = 10, method = "bu")
}
\keyword{ts}

