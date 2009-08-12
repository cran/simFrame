\name{SampleControl}
\Rdversion{1.1}
\alias{SampleControl}

\title{Create objects of class "SampleControl"}
\description{Create objects of class \code{"SampleControl"}.}
\usage{
SampleControl(\dots)
}
\arguments{
    \item{\dots}{arguments passed to \code{new("SampleControl", \dots)}.}
}
\value{
  An object of class \code{"SampleControl"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\linkS4class{SampleControl}}
}
\examples{
data(eusilc)

## simple random sampling
srsc <- SampleControl(size = 20)
draw(eusilc[, c("id", "eqIncome")], srsc)

## group sampling
gsc <- SampleControl(group = "hid", size = 10)
draw(eusilc[, c("hid", "id", "eqIncome")], gsc)

## stratified sampling
sts <- SampleControl(design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))
draw(eusilc[, c("id", "region", "eqIncome")], sts)
}
\keyword{classes}
