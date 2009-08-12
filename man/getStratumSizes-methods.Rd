\name{getStratumSizes-methods}
\docType{methods}
\alias{getStratumSizes-methods}
\alias{getStratumSizes,list,missing-method}
\alias{getStratumSizes,data.frame,BasicVector-method}

\title{Methods for Function getStratumSizes}
\description{
Get the stratum sizes.
}
\section{Methods}{
\describe{

\item{x = "list", design = "missing"}{get the stratum sizes for a list in 
  which each list element contains the indices of the observations belonging to 
  the corresponding stratum (as returned by \code{getStrataSplit}).}

\item{x = "data.frame", design = "BasicVector"}{get the stratum sizes of a 
  data set, according to the variables specified by \code{design}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{getStratumSizes}}, \code{\link{getStrataSplit}}
}
\keyword{methods}
\keyword{manip}
