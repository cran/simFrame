\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,SimResults,missing-method}

\title{Plot simulation results}
\description{
Plot simulation results.  A suitable plot function is selected 
automatically, depending on the structure of results.
}
\usage{
\S4method{plot}{SimResults,missing}(x, y , \dots)
}
\arguments{
  \item{x}{the simulation results.}
  \item{y}{not used.}
  \item{\dots}{further arguments to be passed to the selected plot function.}
}
\section{Methods}{
\describe{

\item{x = "SimResults", y = "missing"}{plot simulation results.}
}}
\section{Details}{
  The results of simulation experiments with at most one contamination level 
  and at most one missing value rate are visualized by (conditional) 
  box-and-whisker plots.  For simulations involving different contamination 
  levels or missing value rates, the average results are plotted against the 
  contamination levels or missing value rates.
  
  Currently no plot function is implemented for simulations with more than one 
  contamination level and more than one missing value rate.
}
\section{Value}{
  An object of class \code{"trellis"}.  The 
  \code{\link[lattice:update.trellis]{update}} method can be used to update 
  components of the object and the \code{\link[lattice:print.trellis]{print}} 
  method (usually called by default) will plot it on an appropriate plotting 
  device.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{simBwplot}}, \code{\link{simDensityplot}}, 
  \code{\link{simXyplot}}, \code{\linkS4class{SimResults}}
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

## plot results
plot(results, true = means)
}
\keyword{methods}
\keyword{hplot}
