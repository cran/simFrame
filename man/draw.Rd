\name{draw}
\Rdversion{1.1}
\alias{draw}
\alias{draw,data.frame,SampleSetup-method}
\alias{draw,data.frame,VirtualSampleControl-method}

\title{Draw a sample}
\description{
Generic function for drawing a sample.
}
\usage{
draw(x, setup, \dots)

\S4method{draw}{data.frame,SampleSetup}(x, setup, i = 1)

\S4method{draw}{data.frame,VirtualSampleControl}(x, setup)
}
\arguments{
  \item{x}{the data to sample from.}
  \item{setup}{an object of class \code{"SampleSetup"} containing previously 
    set up samples, a control object inheriting from the virtual class 
    \code{"VirtualSampleControl"} or a character string specifying such a 
    control class (the default being \code{"SampleControl"}).}
  \item{i}{an integer specifying which one of the previously set up samples 
    should be drawn.}
  \item{\dots}{if \code{setup} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.}
}
\value{
  A \code{data.frame} containing the sampled observations.  In addition, the 
  column \code{".weight"}, which consists of the sample weights, is added to 
  the \code{data.frame}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{setup}}, \code{\linkS4class{SampleSetup}}, 
  \code{\linkS4class{SampleControl}}, \code{\linkS4class{VirtualSampleControl}}
}
\examples{
data(eusilc)

## simple random sampling
draw(eusilc[, c("id", "eqIncome")], size = 20)

## group sampling
draw(eusilc[, c("hid", "id", "eqIncome")], 
    group = "hid", size = 10)

## stratified sampling
draw(eusilc[, c("id", "region", "eqIncome")], 
    design = "region", size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))
}
\keyword{distribution}
