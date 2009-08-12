\name{SimControl}
\Rdversion{1.1}
\alias{SimControl}

\title{Create objects of class "SimControl"}
\description{Create objects of class \code{"SimControl"}.}
\usage{
SimControl(\dots)
}
\arguments{
    \item{\dots}{arguments passed to \code{new("SimControl", \dots)}.}
}
\value{
  An object of class \code{"SimControl"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\linkS4class{SimControl}}
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

## combine these to "SimControl" object and run simulation
sc <- SimControl(contControl = cc, design = "group", fun = sim)
results <- runSimulation(dc, nrep = 50, control = sc)

## plot results
plot(results, true = means)
}
\keyword{classes}
