\name{contaminate-methods}
\docType{methods}
\alias{contaminate-methods}
\alias{contaminate,data.frame,character-method}
\alias{contaminate,data.frame,missing-method}

\title{Methods for contaminating data}
\description{
Contaminate data.
}
\section{Methods}{
\describe{

\item{x = "data.frame", control = "character"}{contaminate data using a 
  control class specified by the character string \code{control}.  The 
  slots of the control object may be supplied as additional arguments.}

\item{x = "data.frame", control = "ContControl"}{contaminate data as defined 
  by the control object \code{control}.}

\item{x = "data.frame", control = "missing"}{contaminate data using a control 
  object of class \code{"ContControl"}.  Its slots may be supplied as 
  additional arguments.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{contaminate}}, \code{\linkS4class{DCARContControl}}, 
  \code{\linkS4class{DARContControl}}, \code{\linkS4class{ContControl}}, 
  \code{\linkS4class{VirtualContControl}}
}
\keyword{methods}
\keyword{manip}
