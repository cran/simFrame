\name{DCARContControl}
\Rdversion{1.1}
\alias{DCARContControl}

\title{Create objects of class "DCARContControl"}
\description{Create objects of class \code{"DCARContControl"}.}
\usage{
DCARContControl(\dots)
}
\arguments{
    \item{\dots}{arguments passed to \code{new("DCARContControl", \dots)}.}
}
\value{
  An object of class \code{"DCARContControl"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\linkS4class{DCARContControl}}
}
\examples{
data(eusilc)
sam <- draw(eusilc[, c("id", "eqIncome")], size = 20)
cc <- DCARContControl(target = "eqIncome", epsilon = 0.05, 
    dots = list(mean = 5e+05, sd = 10000))
contaminate(sam, cc)
}
\keyword{classes}
