\name{Smatrix}
\Rdversion{1.1}
\alias{Smatrix}
\title{Summing matrix for hierarchical time series}
\description{
This function returns the summing matrix for a hierarchical or grouped time series, as defined in Hyndman et al. (2011).
}
\usage{
Smatrix(object)
}
\arguments{
  \item{object}{Hierarchical time series of class \code{hts}.}
}
\value{A numerical matrix.}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}

\author{Rob J Hyndman and Roman A Ahmed}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{combinef}}
}

\examples{
Smatrix(htseg1)
}
\keyword{ts}

