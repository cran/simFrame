\name{DataControl}
\Rdversion{1.1}
\alias{DataControl}

\title{Create objects of class "DataControl"}
\description{Create objects of class \code{"DataControl"}.}
\usage{
DataControl(\dots)
}
\arguments{
    \item{\dots}{arguments passed to \code{new("DataControl", \dots)}.}
}
\value{
  An object of class \code{"DataControl"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\linkS4class{DataControl}}
}
\examples{
require(MASS)
mu <- rep(0, 2)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
dc <- DataControl(size = 10, distribution = mvrnorm, 
    dots = list(mu = mu, Sigma = Sigma))
generate(dc)
}
\keyword{classes}
