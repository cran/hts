\name{plot.hts}
\Rdversion{1.1}
\alias{plot.hts}
\title{Plot a hierarchical time series}
\description{
Method for plotting hierarchical time series. 
}
\usage{
plot.hts(x, levels = length(x$m), labels = TRUE, ...)
}

\arguments{
  \item{x}{An object of class \code{\link[hts]{hts}}}
  \item{levels}{Levels of the hierarchical structure}
  \item{labels}{Labels of the hierarchical structure}
  \item{\dots}{Other arguments passing to \code{\link[graphics]{plot.default}}}
}
\author{Rob J Hyndman}

\references{
G. Athanasopoulos, R. A. Ahmed and R. J. Hyndman (2009) Hierarchical forecasts for Australian domestic tourism, \emph{International Journal of Forecasting}, \bold{25}, 146-166.

Hyndman, Rob J., Ahmed, Roman A., and Athanasopoulos, G. (2007) 
Optimal combination forecasts for hierarchical time series. Working paper. 
Monash University Business and Economic Forecasting Unit.
\url{http://robjhyndman.com/papers/hierarchical}
}
\examples{
plot(simulatedata1)
}
\keyword{hplot}

