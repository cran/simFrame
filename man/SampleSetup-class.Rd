\name{SampleSetup-class}
\Rdversion{1.1}
\docType{class}
\alias{SampleSetup-class}
\alias{length,SampleSetup-method}

\title{Class "SampleSetup"}
\description{Class for set up samples.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SampleSetup", \dots)} or 
\code{SampleSetup(\dots)}.
}
\section{Slots}{
	 \describe{
    \item{\code{indices}:}{Object of class \code{"list"}; each list element 
        contains the indices of the sampled observations.}
    \item{\code{prob}:}{Object of class \code{"numeric"} giving the 
        inclusion probabilities.}
    \item{\code{design}:}{Object of class \code{"character"} giving the 
        variables (columns) defining the strata of the sample design.}
    \item{\code{group}:}{Object of class \code{"character"} giving the 
        variable (column) used for sampling whole groups rather than 
        individual observations.}
    \item{\code{method}:}{Object of class \code{"function"} used for sampling.}
    \item{\code{call}:}{Object of class \code{"SimCall"}; the function call 
        used to set up the samples, or \code{NULL}.}
  }
}
\section{Methods}{
  \describe{
    \item{clusterRunSimulation}{\code{signature(cl = "ANY", x = "data.frame", 
      setup = "SampleSetup", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment on a \code{snow} cluster.}
    \item{draw}{\code{signature(x = "data.frame", setup = "SampleSetup")}: 
      draw a sample.}
    \item{length}{\code{signature(x = "SampleSetup")}: get the number of set up 
      samples.}
    \item{runSimulation}{\code{signature(x = "data.frame", 
      setup = "SampleSetup", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{SampleControl}}, \code{\linkS4class{VirtualSampleControl}}, 
  \code{\link{setup}}, \code{\link{draw}}
}
\examples{
showClass("SampleSetup")
}
\keyword{classes}
