\name{DARContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{DARContControl-class}

\title{Class "DARContControl"}
\description{
  Class for controlling contamination in a simulation experiment.  The values 
  of the contaminated observations will be distributed at random (\emph{DAR}), 
  i.e., they will depend on on the original values.
}
\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{new("DARContControl", \dots)}, \code{DARContControl(\dots)} or 
\code{ContControl(\dots, type="DAR")}.
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
    \item{\code{fun}:}{Object of class \code{"function"} generating 
      the values of the contamination data.  The original values of the 
      observations to be contaminated will be passed as its first argument.  
      Furthermore, it should return an object that can be coerced to a 
      \code{data.frame}, containing the contamination data.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional 
      arguments to be passed to \code{fun}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ContControl}"}, directly.
Class \code{"\linkS4class{VirtualContControl}"}, by class "ContControl", distance 2.
Class \code{"\linkS4class{OptContControl}"}, by class "ContControl", distance 3.
}
\section{Methods}{
No methods defined with class "DARContControl" in the signature.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{DCARContControl}}, \code{\linkS4class{ContControl}}, 
  \code{\linkS4class{VirtualContControl}}, \code{\link{contaminate}}
}
\examples{
showClass("DARContControl")
}
\keyword{classes}
