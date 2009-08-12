\name{VirtualNAControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualNAControl-class}
\alias{length,VirtualNAControl-method}

\title{Class "VirtualNAControl"}
\description{
  Virtual superclass for controlling the insertion of missing values in a 
  simulation experiment.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying the variables (columns) in which missing values should 
      be inserted, or \code{NULL} to insert missing values in all variables 
      (except the additional ones generated internally).}
    \item{\code{NArate}:}{Object of class \code{"NumericMatrix"} giving the 
      missing value rates, which may be selected individually for the target 
      variables. In case of a vector, the same missing value rates are used for 
      all target variables. In case of a matrix, on the other hand, the missing 
      value rates to be used for each target variable are given by the 
      respective column.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{OptNAControl}"}, directly.
}
\section{Methods}{
  \describe{
    \item{length}{\code{signature(x = "VirtualNAControl")}: get the number of 
      missing value rates to be used (the length in case of a vector or the 
      number of rows in case of a matrix).}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{NAControl}}, \code{\link{setNA}}
}
\examples{
showClass("VirtualNAControl")
}
\keyword{classes}
