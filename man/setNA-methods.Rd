\name{setNA-methods}
\docType{methods}
\alias{setNA-methods}
\alias{setNA,data.frame,character-method}
\alias{setNA,data.frame,missing-method}

\title{Methods for setting missing values}
\description{
Insert missing values into data.
}
\section{Methods}{
\describe{

\item{x = "data.frame", control = "character"}{set missing values using a 
  control class specified by the character string \code{control}.  The 
  slots of the control object may be supplied as additional arguments.}

\item{x = "data.frame", control = "missing"}{set missing values using a 
  control object of class \code{"NAControl"}.  Its slots may be supplied as 
  additional arguments.}

\item{x = "data.frame", control = "NAControl"}{set missing values as defined 
  by the control object \code{control}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{setNA}}, \code{\linkS4class{NAControl}}, 
  \code{\linkS4class{VirtualNAControl}}
}
\keyword{methods}
\keyword{manip}
