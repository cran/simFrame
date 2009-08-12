\name{stratify-utilities}
\Rdversion{1.1}
\alias{getStrataLegend}
\alias{getStrataSplit}
\alias{getStrataTable}
\alias{getStratumSizes}
\alias{getStratumValues}

\title{Utility functions for stratifying data}
\description{
Generic utility functions for stratifying data. These are useful if not all the 
information of class \code{"Strata"} is necessary.
}
\usage{
getStrataLegend(x, design)

getStrataSplit(x, design, USE.NAMES = TRUE)

getStrataTable(x, design)

getStratumSizes(x, design, USE.NAMES = TRUE)

getStratumValues(x, design, split)
}
\arguments{
  \item{x}{the \code{data.frame} to be stratified. For \code{getStratumSizes}, 
    it is also possible to supply a list in which each list element contains 
    the indices of the observations belonging to the corresponding stratum (as 
    returned by \code{getStrataSplit}).}
  \item{design}{a character, logical or numeric vector specifying the variables 
    (columns) to be used for stratification.}
  \item{USE.NAMES}{a logical indicating whether information about the strata 
    should be used as \code{names} for the result.}
  \item{split}{an optional list in which each list element contains the indices 
    of the observations belonging to the corresponding stratum (as returned by 
    \code{getStrataSplit}).}
}
\value{
  For \code{getStrataLegend}, a \code{data.frame} describing the strata.
  
  For \code{getStrataSplit}, a list in which each element contains the 
  indices of the observations belonging to the corresponding stratum.
  
  For \code{getStrataTable}, a \code{data.frame} describing the strata 
  and containing the stratum sizes.
  
  For \code{getStratumSizes}, a numeric vector of the stratum sizes.
  
  For \code{getStratumValues}, a numeric vector giving the stratum number for 
  each observation.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{stratify}}, \code{\linkS4class{Strata}}
}
\examples{
data(eusilc)

## all data
getStrataLegend(eusilc, c("region", "gender"))
getStrataTable(eusilc, c("region", "gender"))
getStratumSizes(eusilc, c("region", "gender"))

## small sample
sam <- draw(eusilc, size = 25)
getStrataSplit(sam, "gender")
getStratumValues(sam, c("region","gender"))
}
\keyword{manip}
