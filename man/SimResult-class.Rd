\name{SimResult-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResult-class}

\title{Class "SimResult"}
\description{Class for the results of one simulation run.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SimResult", \dots)} or 
\code{SimResult(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{values}:}{Object of class \code{"numeric"} containing 
      quantities of interest.}
    \item{\code{add}:}{Object of class \code{"ANY"} containing additional 
      results, e.g., statistical models.}
  }
}
\section{Details}{
  The function to be evaluated in the simulation runs (see 
  \code{\link{runSimulation}}) must return a numeric vector or an object of 
  class \code{SimResult}.  Note that the latter is computationally more 
  expensive.  Returning a list with components \code{values} and \code{add} is 
  also accepted and slightly faster than using a \code{"SimResult"} object.
}
\section{Methods}{
No methods defined with class "SimResult" in the signature.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{runSimulation}}, \code{\linkS4class{SimResults}}
}
\examples{
showClass("SimResult")
}
\keyword{classes}
