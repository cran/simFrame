\name{VirtualContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualContControl-class}
\alias{length,VirtualContControl-method}

\title{Class "VirtualContControl"}
\description{
  Virtual superclass for controlling contamination in a simulation experiment.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying specifying the variables (columns) to be contaminated, 
      or \code{NULL} to contaminate all variables  (except the additional ones 
      generated internally).}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} giving the 
      contamination levels.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{OptContControl}"}, directly.
}
\section{Methods}{
  \describe{
    \item{length}{\code{signature(x = "VirtualContControl")}: get the number of 
      contamination levels to be used.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{DCARContControl}}, \code{\linkS4class{DARContControl}}, 
  \code{\linkS4class{ContControl}}, \code{\link{contaminate}}
}
\examples{
showClass("VirtualContControl")
}
\keyword{classes}
