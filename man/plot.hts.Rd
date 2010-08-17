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
  \item{levels}{Number of levels of the hierarchy + to be plotted}
  \item{labels}{If \code{TRUE}, plot the labels next to each series}
  \item{\dots}{Other arguments passing to \code{\link[graphics]{plot.default}}}
}
\author{Rob J Hyndman}

\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2010) 
Optimal combination forecasts for hierarchical time series. 
Working paper, Monash University. 
\url{http://robjhyndman.com/papers/hierarchical}
}
\examples{
plot(htseg1)
}
\keyword{hplot}

