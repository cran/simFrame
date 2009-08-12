\name{clusterSetup-methods}
\docType{methods}
\alias{clusterSetup-methods}
\alias{clusterSetup,ANY,data.frame,character-method}
\alias{clusterSetup,ANY,data.frame,missing-method}

\title{Methods for setting up multiple samples on a snow cluster}
\description{
Set up multiple samples on a \code{snow} cluster.
}
\section{Methods}{
\describe{

\item{cl = "ANY", x = "data.frame", control = "character"}{set up multiple 
  samples on a \code{snow} cluster using a control class specified by the 
  character string \code{control}.  The slots of the control object may be 
  supplied as additional arguments.}

\item{cl = "ANY", x = "data.frame", control = "missing"}{set up multiple 
  samples on a \code{snow} cluster using a control object of class 
  \code{"SampleControl"}.  Its slots may be supplied as additional arguments.}

\item{cl = "ANY", x = "data.frame", control = "SampleControl"}{set up 
  multiple samples on a \code{snow} cluster as defined by the control object 
  \code{control}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{clusterSetup}}, \code{\link{setup}}, \code{\link{draw}}, 
  \code{\linkS4class{SampleControl}}, \code{\linkS4class{VirtualSampleControl}}, 
  \code{\linkS4class{SampleSetup}}
}
\keyword{methods}
\keyword{distribution}
