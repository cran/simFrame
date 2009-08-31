\name{DataControl-class}
\Rdversion{1.1}
\docType{class}
\alias{DataControl-class}

\title{Class "DataControl"}
\description{Class for controlling model-based generation of data.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DataControl", \dots)} or 
\code{DataControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{size}:}{Object of class \code{"numeric"} giving the number of 
      observations to be generated.}
    \item{\code{distribution}:}{Object of class \code{"function"} generating 
      the data, e.g., \code{\link[stats:Normal]{rnorm}} (the default) or 
      \code{\link[MASS]{mvrnorm}}.  It should take a positive integer as 
      its first argument, giving the number of observations to be generated, 
      and return an object that can be coerced to a \code{data.frame}.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional 
      arguments to be passed to \code{distribution}.}
    \item{\code{colnames}:}{Object of class \code{"OptCharacter"} ; a character 
      vector to be used as column names for the generated \code{data.frame}, or 
      \code{NULL}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualDataControl}"}, directly.
}
\section{Methods}{
  \describe{
    \item{generate}{\code{signature(control = "DataControl")}: generate data.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{VirtualDataControl}}, \code{\link{generate}}}
\examples{
showClass("DataControl")
}
\keyword{classes}
