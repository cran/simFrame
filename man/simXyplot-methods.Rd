\name{simXyplot-methods}
\docType{methods}
\alias{simXyplot-methods}
\alias{simXyplot,SimResults-method}

\title{Methods for producing x-y plots}
\description{
Produce x-y plots.
}
\usage{
\S4method{simXyplot}{SimResults}(x, true = NULL, epsilon, NArate, \dots)
}
\arguments{
  \item{x}{the simulation results to be plotted, i.e., an object of class 
    \code{"SimResults"}.}
  \item{true}{a numeric vector giving the true values.  If supplied, 
    reference lines are drawn in the corresponding panels.} 
  \item{epsilon}{a numeric value specifying a contamination level.  If 
    supplied, the values corresponding to that contamination level are 
    extracted from the simulation results and plotted.}
  \item{NArate}{a numeric value specifying a missing value rate.  If
    supplied, the values corresponding to that missing value rate are 
    extracted from the simulation results and plotted.}
  \item{\dots}{additional arguments to be passed down to 
    \code{\link[lattice]{xyplot}}.}
}
\section{Methods}{
\describe{

\item{x = "SimResults"}{produce x-y plots of simulation results.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{simBwplot}}, \code{\link{simDensityplot}}, 
  \code{\link[lattice]{xyplot}}, \code{\linkS4class{SimResults}}
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
    epsilon = seq(from = 0, to = 0.25, by = 0.05), 
    dots = list(mean = 10))

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$value), 
        trimmed = mean(x$value, trim = 0.25), 
        median = median(x$value))
}

## run simulation
results <- runSimulation(dc, nrep = 25, 
    contControl = cc, design = "group", fun = sim)

## plot results
simXyplot(results, true = means)
}
\keyword{methods}
\keyword{hplot}
