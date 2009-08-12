\name{setup-methods}
\docType{methods}
\alias{setup-methods}
\alias{setup,data.frame,character-method}
\alias{setup,data.frame,missing-method}

\title{Methods for setting up multiple samples}
\description{
Set up multiple samples.
}
\section{Methods}{
\describe{

\item{x = "data.frame", control = "character"}{set up multiple samples using 
  a control class specified by the character string \code{control}.  The 
  slots of the control object may be supplied as additional arguments.}

\item{x = "data.frame", control = "missing"}{set up multiple samples using a 
  control object of class \code{"SampleControl"}.  Its slots may be supplied as 
  additional arguments.}

\item{x = "data.frame", control = "SampleControl"}{set up multiple samples as 
  defined by the control object \code{control}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{setup}}, \code{\link{simSample}}, \code{\link{draw}}, 
  \code{\linkS4class{SampleControl}}, \code{\linkS4class{VirtualSampleControl}}, 
  \code{\linkS4class{SampleSetup}}
}
\keyword{methods}
\keyword{distribution}
