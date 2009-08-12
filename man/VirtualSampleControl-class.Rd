\name{VirtualSampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualSampleControl-class}
\alias{length,VirtualSampleControl-method}

\title{Class "VirtualSampleControl"}
\description{
  Virtual superclass for controlling the setup of samples.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{k}:}{Object of class \code{"numeric"}, a single positive 
      integer giving the number of samples to be set up.}
  }
}
\section{Methods}{
  \describe{
    \item{clusterRunSimulation}{\code{signature(cl = "ANY", x = "data.frame", 
      setup = "VirtualSampleControl", nrep = "missing", 
      control = "SimControl")}: run a simulation experiment on a \code{snow} 
      cluster.}
    \item{draw}{\code{signature(x = "data.frame", 
      setup = "VirtualSampleControl")}: draw a sample.}
    \item{length}{\code{signature(x = "VirtualSampleControl")}: get the number 
      of samples to be set up.}
    \item{runSimulation}{\code{signature(x = "data.frame", 
      setup = "VirtualSampleControl", nrep = "missing", 
      control = "SimControl")}: run a simulation experiment.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{SampleControl}}, \code{\linkS4class{SampleSetup}}, 
  \code{\link{setup}}, \code{\link{draw}}
}
\examples{
showClass("VirtualSampleControl")
}
\keyword{classes}
