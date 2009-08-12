\name{generate}
\Rdversion{1.1}
\alias{generate}
\alias{generate,DataControl-method}

\title{Generate data}
\description{
Generic function for generating data based on a (distribution) model.
}
\usage{
generate(control, \dots)

\S4method{generate}{DataControl}(control)
}
\arguments{
  \item{control}{a control object inheriting from the virtual class 
    \code{"VirtualDataControl"} or a character string specifying such a 
    control class (the default being \code{"DataControl"}).}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.}
}
\details{
  The control class \code{"DataControl"} is quite simple but general.  For 
  user-defined data generation, it often suffices to implement a function and 
  use it as the \code{distribution} slot in the \code{"DataControl"} object.  
  See \code{"\linkS4class{DataControl}"} for some requirements for such a 
  function.
  
  However, if more specialized data generation models are required, e.g., for 
  adding contamination already in the data generation process, the framework 
  can be extended by defining a control class \code{"MyDataControl"} extending 
  \code{"VirtualDataControl"} and the corresponding method 
  \code{generate(control)} with signature \code{'MyDataControl'}.
}
\value{
  A \code{data.frame}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{DataControl}}, \code{\linkS4class{VirtualDataControl}}
}
\examples{
require(MASS)
mu <- rep(0, 2)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
generate(size = 10, distribution = mvrnorm, 
    dots = list(mu = mu, Sigma = Sigma))
}
\keyword{distribution}
