\name{VirtualDataControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualDataControl-class}

\title{Class "VirtualDataControl"}
\description{
  Virtual superclass for controlling model-based generation of data.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Methods}{
  \describe{
    \item{clusterRunSimulation}{\code{signature(cl = "ANY", 
      x = "VirtualDataControl", setup = "missing", nrep = "numeric", 
      control = "SimControl")}: run a simulation experiment on a \code{snow} 
      cluster.}
    \item{runSimulation}{\code{signature(x = "VirtualDataControl", 
      setup = "missing", nrep = "numeric", control = "SimControl")}: run a 
      simulation experiment.}
    \item{runSimulation}{\code{signature(x = "VirtualDataControl", 
      setup = "missing", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{DataControl}}, \code{\link{generate}}
}
\examples{
showClass("VirtualDataControl")
}
\keyword{classes}
