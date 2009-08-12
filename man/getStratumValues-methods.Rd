\name{getStratumValues-methods}
\docType{methods}
\alias{getStratumValues-methods}
\alias{getStratumValues,data.frame,BasicVector,list-method}
\alias{getStratumValues,data.frame,BasicVector,missing-method}
\title{Methods for Function getStratumValues}
\description{
Get the stratum number for each observation of a data set.
}
\section{Methods}{
\describe{

\item{x = "data.frame", design = "BasicVector", split = "list"}{get the 
  stratum number for each observation, according to the variables specified by 
  \code{design}.  A previously computed list in which each list element 
  contains the indices of the observations belonging to the corresponding 
  stratum (as returned by \code{getStrataSplit}) speeds things up a bit.}

\item{x = "data.frame", design = "BasicVector", split = "missing"}{get the 
  stratum number for each observation, according to the variables specified by 
  \code{design}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{getStratumValues}}, \code{\link{getStrataSplit}}
}
\keyword{methods}
\keyword{manip}
