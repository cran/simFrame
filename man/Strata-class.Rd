\name{Strata-class}
\Rdversion{1.1}
\docType{class}
\alias{Strata-class}

\title{Class "Strata"}
\description{Class containing strata information for a data set.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Strata", \dots)} or 
\code{Strata(\dots)}.
}
\section{Slots}{
	 \describe{
    \item{\code{values}:}{Object of class \code{"integer"} giving the stratum 
        number for each observation.}
    \item{\code{split}:}{Object of class \code{"list"}; each list element 
        contains the indices of the observations belonging to the corresponding 
        stratum.}
    \item{\code{design}:}{Object of class \code{"character"} giving the 
        variables (columns) defining the strata.}
    \item{\code{nr}:}{Object of class \code{"integer"} giving the stratum 
        numbers.}
    \item{\code{legend}:}{Object of class \code{"data.frame"} describing the 
        strata.}
    \item{\code{size}:}{Object of class \code{"numeric"} giving the stratum 
        sizes.}
    \item{\code{call}:}{Object of class \code{"OptCall"}; the function call 
        used to stratify the data, or \code{NULL}.}
  }
}
\section{Methods}{
  \describe{
    \item{simApply}{\code{signature(x = "data.frame", design = "Strata", 
      fun = "function")}: apply a function to subsets.}
    \item{simSapply}{\code{signature(x = "data.frame", design = "Strata", 
      fun = "function")}: apply a function to subsets.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{stratify}}
}
\examples{
showClass("Strata")
}
\keyword{classes}
