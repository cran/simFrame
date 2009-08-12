\name{NAControl}
\Rdversion{1.1}
\alias{NAControl}

\title{Create objects of class "NAControl"}
\description{Create objects of class \code{"NAControl"}.}
\usage{
NAControl(\dots)
}
\arguments{
    \item{\dots}{arguments passed to \code{new("NAControl", \dots)}.}
}
\value{
  An object of class \code{"NAControl"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\linkS4class{NAControl}}
}
\examples{
data(eusilc)
sam <- draw(eusilc[, c("id", "age", "eqIncome")], size = 25)

## missing completely at random
mcarc <- NAControl(target = "eqIncome", NArate = 0.2)
setNA(sam, mcarc)

## missing at random
marc <- NAControl(target = "eqIncome", NArate = 0.2, aux = "age")
setNA(sam, marc)

## missing not at random
mnarc <- NAControl(target = "eqIncome", 
    NArate = 0.2, aux = "eqIncome")
setNA(sam, mnarc)
}
\keyword{classes}
