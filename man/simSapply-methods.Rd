\name{simSapply-methods}
\docType{methods}
\alias{simSapply-methods}
\alias{simSapply,data.frame,BasicVector,function-method}
\alias{simSapply,data.frame,Strata,function-method}

\title{Methods for applying a function to subsets}
\description{
Apply a function to subsets of a data set.
}
\section{Methods}{
\describe{

\item{x = "data.frame", design = "BasicVector", fun = "function"}{apply a 
  function to subsets given by the variables (columns) in \code{design}.}

\item{x = "data.frame", design = "Strata", fun = "function"}{apply a function 
  to subsets given by \code{design}.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\link{simSapply}}
}
\keyword{methods}
\keyword{iteration}
