\name{draw-methods}
\docType{methods}
\alias{draw-methods}
\alias{draw,data.frame,character-method}
\alias{draw,data.frame,missing-method}

\title{Methods for drawing a sample}
\description{
Draw a sample.
}
\section{Methods}{
\describe{

\item{x = "data.frame", setup = "character"}{draw a sample using a control 
  class specified by the character string \code{setup}.  The slots of the 
  control object may be supplied as additional arguments.}

\item{x = "data.frame", setup = "missing"}{draw a sample using a control 
  object of class \code{"SampleControl"}.  Its slots may be supplied as 
  additional arguments.}

\item{x = "data.frame", setup = "SampleSetup"}{draw a previously set up 
  sample.}

\item{x = "data.frame", setup = "VirtualSampleControl"}{draw a sample using a 
  control object inheriting from the virtual class 
  \code{"VirtualSampleControl"}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{draw}}, \code{\link{setup}}, \code{\linkS4class{SampleSetup}}, 
  \code{\linkS4class{SampleControl}}, \code{\linkS4class{VirtualSampleControl}}
}
\keyword{methods}
\keyword{distribution}
