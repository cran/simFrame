\name{clusterRunSimulation-methods}
\docType{methods}
\alias{clusterRunSimulation-methods}
\alias{clusterRunSimulation,ANY,ANY,ANY,ANY,missing-method}
\alias{clusterRunSimulation,ANY,data.frame,missing,numeric,SimControl-method}
\alias{clusterRunSimulation,ANY,data.frame,SampleSetup,missing,SimControl-method}
\alias{clusterRunSimulation,ANY,data.frame,VirtualSampleControl,missing,SimControl-method}
\alias{clusterRunSimulation,ANY,VirtualDataControl,missing,numeric,SimControl-method}

\title{Methods for running a simulation experiment on a snow cluster}
\description{
Run a simulation experiment on a \code{snow} cluster.
}
\section{Methods}{
\describe{

\item{cl = "ANY", x = "ANY", setup = "ANY", nrep = "ANY", control = "missing"}{
  convenience wrapper that allows the slots of \code{control} to be supplied 
  as arguments}

\item{cl = "ANY", x = "data.frame", setup = "missing", nrep = "numeric", 
  control = "SimControl"}{run a simulation experiment based on real data 
  with repetitions on a \code{snow} cluster.}

\item{cl = "ANY", x = "data.frame", setup = "SampleSetup", nrep = "missing", 
  control = "SimControl"}{run a design-based simulation experiment with 
  previously set up samples on a \code{snow} cluster.}

\item{cl = "ANY", x = "data.frame", setup = "VirtualSampleControl", 
  nrep = "missing", control = "SimControl"}{run a design-based simulation 
  experiment on a \code{snow} cluster.}

\item{cl = "ANY", x = "VirtualDataControl", setup = "missing", 
  nrep = "numeric", control = "SimControl"}{run a model-based simulation 
  experiment with repetitions on a \code{snow} cluster.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{clusterRunSimulation}}, \code{\link{runSimulation}}, 
  \code{\linkS4class{SimControl}}, \code{\linkS4class{SimResults}}, 
  \code{\link{simBwplot}}, \code{\link{simDensityplot}}, \code{\link{simXyplot}}
}
\keyword{methods}
\keyword{design}
