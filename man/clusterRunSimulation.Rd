\name{clusterRunSimulation}
\Rdversion{1.1}
\alias{clusterRunSimulation}

\title{Run a simulation experiment on a snow cluster}
\description{
Generic function for running a simulation experiment on a \code{snow} cluster.
}
\usage{
clusterRunSimulation(cl, x, setup, nrep, control, 
                     contControl = NULL, NAControl = NULL, 
                     design = character(), fun, \dots, 
                     SAE = FALSE)
}
\arguments{
  \item{cl}{a \code{snow} cluster.}
  \item{x}{a \code{data.frame} (for design-based simulation or simulation based 
    on real data) or a control object for data generation inheriting from 
    \code{"VirtualDataControl"} (for model-based simulation).}
  \item{setup}{an object of class \code{"SampleSetup"}, containing previously 
    set up samples, or a control class for setting up samples inheriting 
    from \code{"VirtualSampleControl"}.}
  \item{nrep}{a non-negative integer giving the number of repetitions of the 
    simulation experiment (for model-based simulation or simulation based on 
    real data).}
  \item{control}{a control object of class \code{"SimControl"}}    
  \item{contControl}{an object of a class inheriting from 
    \code{"VirtualContControl"}, controlling contamination in the simulation 
    experiment.}
  \item{NAControl}{an object of a class inheriting from 
    \code{"VirtualNAControl"}, controlling the insertion of missing values in 
    the simulation experiment.}
  \item{design}{a character vector specifying the variables (columns) to be 
    used for splitting the data into domains.  The simulations, including 
    contamination and the insertion of missing values (unless \code{SAE=TRUE}), 
    are then performed on every domain.}
  \item{fun}{a function to be applied in each simulation run.}
  \item{\dots}{for \code{runSimulation}, additional arguments to be passed 
    to \code{fun}.  For \code{runSim}, arguments to be passed to 
    \code{runSimulation}.}
  \item{SAE}{a logical indicating whether small area estimation will be used in 
    the simulation.}
}
\details{
  Statistical simulation is embarrassingly parallel, hence computational 
  performance can be increased by parallel computing.  In \code{simFrame}, 
  parallel computing is implemented using the package \code{snow}.  Note that 
  all objects and packages required for the computations (including 
  \code{simFrame}) need to be made available on every worker process.
   
  In order to prevent problems with random numbers and to ensure 
  reproducibility, random number streams should be used.  In \R, the packages 
  \code{rlecuyer} and \code{rsprng} are available for creating random number 
  streams, which are supported by \code{snow} via the function 
  \code{clusterSetupRNG}.
  
  There are some requirements for slot \code{fun} of the control object 
  \code{control}.  The function must return a numeric vector or an object of 
  class \code{"SimResult"}, which consists of a slot \code{values} (a numeric 
  vector) and a slot \code{add} (additional results of any class, e.g., 
  statistical models).  Note that the latter is computationally more 
  expensive.  Returning a list with components \code{values} and \code{add} 
  is also accepted and slightly faster than using a \code{"SimResult"} 
  object.  A \code{data.frame} is passed to \code{fun} in every simulation 
  run.  The corresponding argument must be called \code{x}.  If comparisons 
  with the original data need to be made, e.g., for evaluating the quality of 
  imputation methods, the function should have an argument called \code{orig}.  
  If different domains are used in the simulation, the indices of the current 
  domain can be passed to the function via an argument called \code{domain}.
  
  For small area estimation, the following points have to be kept in mind.  The 
  slot \code{design} of \code{control} for splitting the data must be supplied 
  and the slot \code{SAE} must be set to \code{TRUE}.  However, the data are 
  not actually split into the specified domains.  Instead, the whole data set 
  (sample) is passed to \code{fun}.  Also contamination and missing values are 
  added to the whole data (sample).  Last, but not least, the function must 
  have a \code{domain} argument so that the current domain can be extracted 
  from the whole data (sample).
  
  In every simulation run, \code{fun} is evaluated using \code{try}.  Hence 
  no results are lost if computations fail in any of the simulation runs.
}
\value{
  An object of class \code{"SimResults"}.
}
\references{
L'Ecuyer, P., Simard, R., Chen E and Kelton, W. (2002) An object-oriented 
random-number package with many long streams and substreams. \emph{Operations 
Research}, \bold{50}(6), 1073--1075.

Mascagni, M. and Srinivasan, A. (2000) Algorithm 806: \code{SPRNG}: a scalable 
library for pseudorandom number generation. \emph{ACM Transactions on 
Mathematical Software}, \bold{26}(3), 436--461.

Rossini, A., Tierney L. and Li, N. (2007) Simple parallel statistical computing 
in \R. \emph{Journal of Computational and Graphical Statistics}, \bold{16}(2), 
399--420.

Tierney, L., Rossini, A. and Li, N. (2009) \code{snow}: A parallel computing 
framework for the \R system. \emph{International Journal of Parallel 
Programming}, \bold{37}(1), 78--90.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link[snow:snow-startstop]{makeCluster}}, 
  \code{\link[snow:snow-rand]{clusterSetupRNG}}, 
  \code{\link{runSimulation}}, \code{\linkS4class{SimControl}}, 
  \code{\linkS4class{SimResults}}, \code{\link{simBwplot}}, 
  \code{\link{simDensityplot}}, \code{\link{simXyplot}}
}
\examples{
\dontrun{
# these examples require at least dual core processor

# start snow cluster
cl <- makeCluster(2, type = "SOCK")

# load package on workers
clusterEvalQ(cl, library(simFrame))

# setup random number stream
clusterSetupRNG(cl, seed = "1234")

# function for generating data
grnorm <- function(n, means) {
    group <- sample(1:2, n, replace=TRUE)
    data.frame(group=group, value=rnorm(n) + means[group])
}

# control objects for data generation and contamination
means <- c(0, 0.5)
dc <- DataControl(size = 500, distribution = grnorm, 
    dots = list(means = means))
cc <- DCARContControl(target = "value", 
    epsilon = 0.1, dots = list(mean = 10))

# function for simulation runs
sim <- function(x) {
    c(mean = mean(x$value), 
        trimmed = mean(x$value, trim = 0.1), 
        median = median(x$value))
}

# export objects to workers
clusterExport(cl, c("grnorm", "means", "dc", "cc", "sim"))

# run simulation
results <- clusterRunSimulation(cl, dc, nrep = 100, 
    contControl = cc, design = "group", fun = sim)

# plot results
plot(results, true = means)
}
}
\keyword{design}
