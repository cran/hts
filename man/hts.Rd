\name{hts}
\Rdversion{1.1}
\alias{hts}
\title{Create a hierarchical time series}
\description{
Method for creating hierarchical time series. 
}
\usage{
hts(y, g)
}
\arguments{
  \item{y}{Multivariate time series containing the bottom level series}
  \item{g}{Group matrix indicating the hierarchical structure, 
           with one column for each bottom level series, 
           and one row for each level of the hierarchy}
}
\value{
  \item{y}{Multivariate time series containing the bottom level series}
  \item{g}{Information about the levels and groups of a hierarchical time series}
  \item{m}{Number of time series within each level}
  \item{gma}{Top level series equal to the rowsums of \code{y}}
}
\references{
Hyndman, Rob J., Ahmed, Roman A., and Athanasopoulos, G. (2007) 
Optimal combination forecasts for hierarchical time series. Working paper. 
Monash University Business and Economic Forecasting Unit.
\url{http://robjhyndman.com/papers/hierarchical}
}
\author{Rob J Hyndman}

\seealso{
\code{\link[hts]{accuracy.hts}}, \code{\link[hts]{forecast.hts}}, \code{\link[hts]{plot.hts}}
}
\examples{
abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 100))
g.abc <- rbind(c(1, 1, 2, 2), 1:4)
x <- hts(y = abc, g = g.abc) 
}

\keyword{ts}
