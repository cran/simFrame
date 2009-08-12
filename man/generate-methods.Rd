\name{generate-methods}
\docType{methods}
\alias{generate-methods}
\alias{generate,character-method}
\alias{generate,missing-method}

\title{Methods for generating data}
\description{
Generate data based on a (distribution) model.
}
\section{Methods}{
\describe{

\item{control = "character"}{generate data using a control class specified by 
  the character string \code{control}.  The slots of the control object may be 
  supplied as additional arguments.}

\item{control = "missing"}{generate data using a control object of class 
  \code{"DataControl"}.  Its slots may be supplied as additional 
  arguments.}

\item{control = "DataControl"}{generate data as defined by the control 
  object \code{control}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{generate}}, \code{\linkS4class{DataControl}}, 
  \code{\linkS4class{VirtualDataControl}}
}
\keyword{methods}
\keyword{distribution}
