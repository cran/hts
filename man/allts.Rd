\name{allts}
\alias{allts}
\title{Extract all time series from an hts object}
\description{
The time series from all levels of a hierarchical time series are returned as a multivariate time series.
}
\usage{
allts(obj)
}

\arguments{
  \item{obj}{An object of class \code{\link[hts]{hts}}}
}
\author{Rob J Hyndman}

\examples{
allts(htseg1)
}
\keyword{ts}

