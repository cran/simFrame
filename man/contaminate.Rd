\name{contaminate}
\Rdversion{1.1}
\alias{contaminate}
\alias{contaminate,data.frame,ContControl-method}

\title{Contaminate data}
\description{Generic function for contaminating data.}
\usage{
contaminate(x, control, \dots)

\S4method{contaminate}{data.frame,ContControl}(x, control, i)
}
\arguments{
  \item{x}{the data to be contaminated.}
  \item{control}{a control object of a class inheriting from the virtual class 
    \code{"VirtualContControl"} or a character string specifying such a control 
    class (the default being \code{"DCARContControl"}).}
  \item{i}{an integer giving the element of the slot \code{epsilon} of 
    \code{control} to be used as contamination level.}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.}
}
\details{
  In order to extend the framework by a user-defined control class 
  \code{"MyContControl"} (which must extend \code{"VirtualContControl"}), a 
  method \code{contaminate(x, control, i)} with signature 
  \code{'data.frame, MyContControl'} needs to be implemented.
}
\value{
  A \code{data.frame} containing the contaminated data.  In addition, the 
  column \code{".contaminated"}, which consists of logicals indicating the 
  contaminated observations, is added to the \code{data.frame}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{DCARContControl}}, \code{\linkS4class{DARContControl}},
  \code{\linkS4class{ContControl}}, \code{\linkS4class{VirtualContControl}}
}
\examples{
## distributed completely at random
data(eusilc)
sam <- draw(eusilc[, c("id", "eqIncome")], size = 20)
contaminate(sam, target = "eqIncome", epsilon = 0.05, 
    dots = list(mean = 5e+05, sd = 10000))

## distributed at random
require(MASS)
mu <- rep(0, 2)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
foo <- generate(size = 10, distribution = mvrnorm, 
    dots = list(mu = mu, Sigma = Sigma))
contaminate(foo, "DARContControl", target = "V2",
    epsilon = 0.2, fun = function(x) x * 100)
}
\keyword{manip}
