\name{ContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{ContControl-class}

\title{Class "ContControl"}
\description{
  Virtual class for controlling contamination in a simulation experiment for 
  internal use.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying specifying the variables (columns) to be contaminated, 
      or \code{NULL} to contaminate all variables (except the additional ones 
      generated internally).}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} giving the 
      contamination levels.}
    \item{\code{group}:}{Object of class \code{"character"} specifying a 
      variable (column) to be used for contaminating whole groups rather than 
      individual observations.}
    \item{\code{aux}:}{Object of class \code{"character"} specifying an 
      auxiliary variable (column) whose values are used as probability weights 
      for selecting the items (observations or groups) to be contaminated.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualContControl}"}, directly.
Class \code{"\linkS4class{OptContControl}"}, by class "VirtualContControl", 
distance 2.
}
\section{Methods}{
  \describe{
    \item{contaminate}{\code{signature(x = "data.frame", 
      control = "ContControl")}: contaminate data.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{DCARContControl}}, \code{\linkS4class{DARContControl}}, 
  \code{\linkS4class{VirtualContControl}}, \code{\link{contaminate}}
}
\examples{
showClass("ContControl")
}
\keyword{classes}
