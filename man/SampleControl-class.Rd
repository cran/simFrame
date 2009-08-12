\name{SampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{SampleControl-class}

\title{Class "SampleControl"}
\description{Class for controlling the setup of samples.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SampleControl", \dots)} 
or \code{SampleControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{design}:}{Object of class \code{"BasicVector"} specifying the 
      variables (columns) to be used for stratified sampling.}
    \item{\code{group}:}{Object of class \code{"BasicVector"} specifying a 
      variable (column) to be used for sampling whole groups rather than 
      individual observations.}
    \item{\code{method}:}{Object of class \code{"function"} to be used for 
      sampling (defaults to \code{\link{srs}}). It should return a vector 
      containing the indices of the sampled items (observations or groups).}
    \item{\code{size}:}{Object of class \code{"OptNumeric"}; an optional 
      non-negative integer giving the number of items (observations or groups) 
      to sample.  For stratified sampling, a vector of non-negative integers, 
      each giving the number of items to sample from the corresponding stratum.}
    \item{\code{prob}:}{Object of class \code{"OptNumeric"}; an optional 
      numeric vector giving the probability weights.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional 
      arguments to be passed to \code{method}.}
    \item{\code{k}:}{Object of class \code{"numeric"}; a single positive 
      integer giving the number of samples to be set up.}
  }
}
\section{Details}{
  There are some restrictions on the argument names of the function 
  supplied to \code{method}.  If it needs population data as input, 
  the corresponding argument should be called \code{x} and should expect 
  a \code{data.frame}.  If the sampling method only needs the population size 
  as input, the argument should be called \code{N}.  Note that \code{method} is 
  not expected to have both \code{x} and \code{N} as arguments, and that the 
  latter is much faster for stratified sampling or group sampling.  
  Furthermore, if the function has arguments for sample size and probability 
  weights, they should be called \code{size} and \code{prob}, respectively.  
  Note that a function with \code{prob} as its only argument is perfectly valid 
  (for probability proportional to size sampling).  Further arguments of 
  \code{method} may be supplied as a list via the slot \code{dots}.
}
\section{Extends}{
Class \code{"\linkS4class{VirtualSampleControl}"}, directly.
}
\section{Methods}{
  \describe{
    \item{clusterSetup}{\code{signature(cl = "ANY", x = "data.frame", 
      control = "SampleControl")}: set up multiple samples on a \code{snow} 
      cluster.}
    \item{setup}{\code{signature(x = "data.frame", 
      control = "SampleControl")}: set up multiple samples.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{VirtualSampleControl}}, \code{\linkS4class{SampleSetup}}, 
  \code{\link{setup}}, \code{\link{draw}}
}
\examples{
showClass("SampleControl")
}
\keyword{classes}
