\name{ContControl}
\Rdversion{1.1}
\alias{ContControl}

\title{Create contamination control objects}
\description{Create objects of a class inheriting from \code{"ContControl"}.}
\usage{
ContControl(\dots, type = c("DCAR", "DAR"))
}
\arguments{
    \item{\dots}{arguments passed to \code{new("DCARContControl", \dots)} or 
      \code{new("DARContControl", \dots)}, as determined by \code{type}.}
    \item{type}{a character string specifying whether a control object of class 
      \code{"DCARContControl"} or \code{"DARContControl"} should be created.}
}
\value{
  If \code{type = "DCAR"}, an object of class \code{"DCARContControl"}.

  If \code{type = "DAR"}, an object of class \code{"DARContControl"}.
}
\note{
  This constructor exists mainly for back compatibility with early draft 
  versions of \code{simFrame}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\linkS4class{DCARContControl}}, \code{\linkS4class{DARContControl}}, 
    \code{\linkS4class{ContControl}}
}
\examples{
## distributed completely at random
data(eusilc)
sam <- draw(eusilc[, c("id", "eqIncome")], size = 20)
dcarc <- ContControl(target = "eqIncome", epsilon = 0.05, 
    dots = list(mean = 5e+05, sd = 10000), type = "DCAR")
contaminate(sam, dcarc)

## distributed at random
require(MASS)
mu <- rep(0, 2)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
foo <- generate(size = 10, distribution = mvrnorm, 
    dots = list(mu = mu, Sigma = Sigma))
darc <- ContControl(target = "V2", epsilon = 0.2, 
    fun = function(x) x * 100, type = "DAR")
contaminate(foo, darc)
}
\keyword{classes}
