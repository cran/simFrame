\name{NAControl-class}
\Rdversion{1.1}
\docType{class}
\alias{NAControl-class}

\title{Class "NAControl"}
\description{
Class for controlling the insertion of missing values in a simulation 
experiment.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NAControl", \dots)} or 
\code{NAControl(\dots)}.
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
    \item{\code{group}:}{Object of class \code{"character"} specifying a 
        variable (column) to be used for setting whole groups to \code{NA} 
        rather than individual values.}
    \item{\code{aux}:}{Object of class \code{"character"} specifying an 
      auxiliary variable (column) whose values are used as probability weights 
      for selecting the values to be set to \code{NA}.}
    \item{\code{intoContamination}:}{Object of class \code{"logical"} 
      indicating whether missing values should also be inserted into 
      contaminated observations.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualNAControl}"}, directly.
Class \code{"\linkS4class{OptNAControl}"}, by class "VirtualNAControl", 
distance 2.
}
\section{Methods}{
  \describe{
    \item{setNA}{\code{signature(x = "data.frame", control = "NAControl")}: 
      set missing values.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{VirtualNAControl}}, \code{\link{setNA}}
}
\examples{
showClass("NAControl")
}
\keyword{classes}
