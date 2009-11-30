\name{aggregate-methods}
\docType{methods}
\alias{aggregate-methods}
\alias{aggregate,SimResults-method}
\title{Method for aggregating simulation results}
\description{
  Aggregate simulation results.
}
\usage{
\S4method{aggregate}{SimResults}(x, select = NULL, FUN = mean, \dots)
}
\arguments{
  \item{x}{the simulation results to be aggregated, i.e., an object of class 
    \code{"SimResults"}.}
  \item{select}{a character vector specifying the columns to be aggregated.  It 
    must be a subset of the \code{colnames} slot of \code{x}, which is the 
    default.}
  \item{FUN}{a scalar function to compute the summary statistics (defaults to 
    \code{mean}).}
  \item{\dots}{additional arguments to be passed down to 
    \code{\link[stats]{aggregate}} or \code{\link{apply}}.}
}
\section{Details}{
  If contamination or missing values have been inserted or the simulations have 
  been split into different domains, \code{\link[stats]{aggregate}} is called 
  to compute the summary statistics for the respective subsets.
  
  Otherwise, \code{\link{apply}} is called to compute the summary statistics 
  for each column specified by \code{select}.
}
\section{Methods}{
\describe{

\item{x = "SimResults"}{aggregate simulation results.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link[stats]{aggregate}}, \code{\link{apply}}
}
\section{Value}{
  If contamination or missing values have been inserted or the simulations have 
  been split into different domains, a \code{data.frame} is returned, otherwise 
  a vector. 
}
\examples{
## for reproducibility
set.seed(1234)

## function for generating data
grnorm <- function(n, means) {
    group <- sample(1:2, n, replace=TRUE)
    data.frame(group=group, value=rnorm(n) + means[group])
}

## control objects for data generation and contamination
means <- c(0, 0.5)
dc <- DataControl(size = 500, distribution = grnorm, 
    dots = list(means = means))
cc <- DCARContControl(target = "value", 
    epsilon = 0.1, dots = list(mean = 10))

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$value), 
        trimmed = mean(x$value, trim = 0.1), 
        median = median(x$value))
}

## run simulation
results <- runSimulation(dc, nrep = 50, 
    contControl = cc, design = "group", fun = sim)

## aggregate
aggregate(results)
}
\keyword{methods}
\keyword{category}
