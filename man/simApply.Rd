\name{simApply}
\Rdversion{1.1}
\alias{simApply}
\alias{simSapply}

\title{Apply a function to subsets}
\description{
Generic functions for applying a function to subsets of a data set.
}
\usage{
simApply(x, design, fun, \dots)

simSapply(x, design, fun, \dots, simplify = TRUE)
}
\arguments{
  \item{x}{the \code{data.frame} to be subsetted.}
  \item{design}{a character, logical or numeric vector specifying the variables 
    (columns) used for subsetting.}
  \item{fun}{a function to be applied to the subsets.}
  \item{simplify}{a logical indicating whether the results should be 
    simplified to a vector or matrix (if possible).}
  \item{\dots}{additional arguments to be passed to \code{fun}.}
}
\value{
  For \code{simApply} a \code{data.frame}.
  
  For \code{simSapply}, a list, vector or matrix (see \code{\link{sapply}}).
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\link{sapply}}
}
\examples{
data(eusilc)
eusilc <- eusilc[, c("region", "gender", "eqIncome")]

## returns data.frame
simApply(eusilc, c("region", "gender"), 
    function(x) median(x$eqIncome))

## returns vector
simSapply(eusilc, c("region", "gender"), 
    function(x) median(x$eqIncome))
}
\keyword{iteration}
