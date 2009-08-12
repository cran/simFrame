\name{BasicVector-class}
\Rdversion{1.1}
\docType{class}
\alias{BasicVector-class}

\title{Class "BasicVector"}
\description{Virtual class used internally for convenience.}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Methods}{
  \describe{
    \item{getStrataLegend}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get a \code{data.frame} describing the strata.}
    \item{getStrataSplit}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get a list in which each element contains the 
      indices of the observations belonging to the corresponding stratum.}
    \item{getStrataTable}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get a \code{data.frame} describing the strata 
      and containing the stratum sizes.}
    \item{getStratumSizes}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get the stratum sizes.}
    \item{getStratumValues}{\code{signature(x = "data.frame", 
      design = "BasicVector", split = "missing")}: get the stratum number for 
      each observation.}
    \item{getStratumValues}{\code{signature(x = "data.frame", 
      design = "BasicVector", split = "list")}: get the stratum number for 
      each observation.}
    \item{simApply}{\code{signature(x = "data.frame", design = "BasicVector", 
      fun = "function")}: apply a function to subsets.}
    \item{simSapply}{\code{signature(x = "data.frame", design = "BasicVector", 
      fun = "function")}: apply a function to subsets.}
    \item{stratify}{\code{signature(x = "data.frame", design = "BasicVector")}: 
      stratify data.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\examples{
showClass("BasicVector")
}
\keyword{classes}
