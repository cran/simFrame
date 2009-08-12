\name{DARContControl}
\Rdversion{1.1}
\alias{DARContControl}

\title{Create objects of class "DARContControl"}
\description{Create objects of class \code{"DARContControl"}.}
\usage{
DARContControl(\dots)
}
\arguments{
    \item{\dots}{arguments passed to \code{new("DARContControl", \dots)}.}
}
\value{
  An object of class \code{"DARContControl"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\linkS4class{DARContControl}}
}
\examples{
require(MASS)
mu <- rep(0, 2)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
foo <- generate(size = 10, distribution = mvrnorm, 
    dots = list(mu = mu, Sigma = Sigma))
cc <- DARContControl(target = "V2",
    epsilon = 0.2, fun = function(x) x * 100)
contaminate(foo, cc)
}
\keyword{classes}
