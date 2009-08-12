\name{setup}
\Rdversion{1.1}
\alias{setup}
\alias{setup,data.frame,SampleControl-method}

\title{Set up multiple samples}
\description{
Generic function for setting up multiple samples.
}
\usage{
setup(x, control, \dots)

\S4method{setup}{data.frame,SampleControl}(x, control)
}
\arguments{
  \item{x}{the data to sample from.}
  \item{control}{a control object inheriting from the virtual class 
    \code{"VirtualSampleControl"} or a character string specifying such a 
    control class (the default being \code{"SampleControl"}).}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.}
}
\details{
  The control class \code{"SampleControl"} is highly flexible and allows 
  stratified sampling as well as sampling of whole groups rather than 
  individuals with a specified sampling method.  Hence it is often sufficient 
  to implement the desired sampling method for the simple non-stratified case 
  to extend the existing framework.  See \code{"\linkS4class{SampleControl}"} 
  for some restrictions on the argument names of such a function, which should 
  return a vector containing the indices of the sampled observations.
  
  Nevertheless, for very complex sampling procedures, it is possible to define 
  a control class \code{"MySampleControl"} extending 
  \code{"VirtualSampleControl"}, and the corresponding method 
  \code{setup(x, control)} with signature \code{'data.frame, MySampleControl'}.  
  In order to optimize computational performance, it is necessary to 
  efficiently set up multiple samples.  Thereby the slot \code{k} of 
  \code{"VirtualSampleControl"} needs to be used to control the number of 
  samples, and the resulting object must be of class \code{SampleSetup}.
}
\value{
  An object of class \code{"SampleSetup"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{simSample}}, \code{\link{draw}}, 
  \code{\linkS4class{SampleControl}}, \code{\linkS4class{VirtualSampleControl}}, 
  \code{\linkS4class{SampleSetup}}
}
\examples{
data(eusilc)

## simple random sampling
srss <- setup(eusilc, size = 20, k = 3)
draw(eusilc[, c("id", "eqIncome")], srss, i = 1)

## group sampling
gss <- setup(eusilc, group = "hid", size = 10, k = 3)
draw(eusilc[, c("hid", "id", "eqIncome")], gss, i = 2)

## stratified sampling
stss <- setup(eusilc, design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), k = 3)
draw(eusilc[, c("id", "region", "eqIncome")], stss, i = 3)
}
\keyword{distribution}
