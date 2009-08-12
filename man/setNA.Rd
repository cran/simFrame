\name{setNA}
\Rdversion{1.1}
\alias{setNA}
\alias{setNA,data.frame,NAControl-method}

\title{Set missing values}
\description{Generic function for inserting missing values into data.}
\usage{
setNA(x, control, \dots)

\S4method{setNA}{data.frame,NAControl}(x, control, i)
}
\arguments{
  \item{x}{the data in which missing values should be inserted.}
  \item{control}{a control object inheriting from the virtual class 
    \code{"VirtualNAControl"} or a character string specifying such a control 
    class (the default being \code{"NAControl"}).}
  \item{i}{an integer giving the element or row of the slot \code{NArate} of 
    \code{control} to be used as missing value rate(s).}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.}
}
\details{
  In order to extend the framework by a user-defined control class 
  \code{"MyNAControl"} (which must extend \code{"VirtualNAControl"}), a method 
  \code{setNA(x, control, i)} with signature \code{'data.frame, MyNAControl'} 
  needs to be implemented.
}
\value{
  A \code{data.frame} containing the data with missing values.  For every 
  target variable, a logical variable indicating the observations set to 
  \code{NA} is added to the \code{data.frame}. The names of these columns start 
  with a dot, followed by the name of the corresponding target variable (e.g., 
  if missing values are generated in column \code{"foo"}, a column 
  \code{".foo"} is added).
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{NAControl}}, \code{\linkS4class{VirtualNAControl}}
}
\examples{
data(eusilc)
sam <- draw(eusilc[, c("id", "age", "eqIncome")], size = 25)

## missing completely at random
setNA(sam, target = "eqIncome", NArate = 0.2)

## missing at random
setNA(sam, target = "eqIncome", NArate = 0.2, aux = "age")

## missing not at random
setNA(sam, target = "eqIncome", NArate = 0.2, aux = "eqIncome")
}
\keyword{manip}
