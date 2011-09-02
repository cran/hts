\name{plot.gts}
\Rdversion{1.1}
\alias{plot.gts}
\title{Plot grouped or hierarchical time series}
\description{
Method for plotting grouped or hierarchical time series. 
}
\usage{
\S3method{plot}{gts}(x, levels = length(x$m), labels = TRUE, ...)
}

\arguments{
  \item{x}{An object of class \code{\link[hts]{hts}}}
  \item{levels}{Number of levels of the hierarchy + to be plotted}
  \item{labels}{If \code{TRUE}, plot the labels next to each series}
  \item{\dots}{Other arguments passing to \code{\link[graphics]{plot.default}}}
}
\author{Rob J Hyndman}

\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}
\examples{
plot(htseg1)
}
\keyword{hplot}

