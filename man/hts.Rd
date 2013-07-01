\name{hts}
\Rdversion{1.1}
\alias{hts}
\alias{gts}
\title{Create a hierarchical or grouped time series}
\description{
Method for creating hierarchical or grouped time series.  For a grouped 
time series, there is no need for a hierarchical structure.

}
\usage{
hts(y, g)
gts(y, g, hierarchical=FALSE)
}
\arguments{
  \item{y}{Multivariate time series containing the bottom level series}
  \item{g}{Group matrix indicating the group structure, with one column for each series when completely disaggregated,
           and one row for each grouping of the time series.}
  \item{hierarchical}{Indicates if the grouping matrix should be treated as hierarchical.}
}
\details{\code{hts} is simply a wrapper for \code{gts(y,g,TRUE)}.}
\value{
  \item{y}{Multivariate time series containing the bottom level series}
  \item{g}{Information about the groups of a grouped time series}
  \item{m}{Number of groups within each grouping}
  \item{gma}{Top level series equal to the rowsums of \code{y}}
}
\references{
R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) 
Optimal combination forecasts for hierarchical time series. 
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}
}
\author{Rob J Hyndman}

\seealso{
\code{\link[hts]{accuracy.gts}}, \code{\link[hts]{forecast.gts}}, \code{\link[hts]{plot.gts}}
}

\examples{
abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 100))
x <- hts(abc, c(1,1,2,2))
y <- gts(abc, rbind(c(1,1,2,2), c(1,2,1,2)))
}

\keyword{ts}
