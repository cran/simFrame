\name{runSimulation-methods}
\docType{methods}
\alias{runSimulation-methods}
\alias{runSimulation,ANY,ANY,ANY,missing-method}
\alias{runSimulation,data.frame,missing,missing,SimControl-method}
\alias{runSimulation,data.frame,missing,numeric,SimControl-method}
\alias{runSimulation,data.frame,SampleSetup,missing,SimControl-method}
\alias{runSimulation,data.frame,VirtualSampleControl,missing,SimControl-method}
\alias{runSimulation,VirtualDataControl,missing,missing,SimControl-method}
\alias{runSimulation,VirtualDataControl,missing,numeric,SimControl-method}

\title{Methods for running a simulation experiment}
\description{
Run a simulation experiment.
}
\section{Methods}{
\describe{

\item{x = "ANY", setup = "ANY", nrep = "ANY", control = "missing"}{
  convenience wrapper that allows the slots of \code{control} to be supplied 
  as arguments}

\item{x = "data.frame", setup = "missing", nrep = "missing", 
  control = "SimControl"}{run a simulation experiment based on real data 
  without repetitions (probably useless, but for completeness).}

\item{x = "data.frame", setup = "missing", nrep = "numeric", 
  control = "SimControl"}{run a simulation experiment based on real data 
  with repetitions.}

\item{x = "data.frame", setup = "SampleSetup", nrep = "missing", 
  control = "SimControl"}{run a design-based simulation experiment with 
  previously set up samples.}

\item{x = "data.frame", setup = "VirtualSampleControl", nrep = "missing", 
  control = "SimControl"}{run a design-based simulation experiment.}

\item{x = "VirtualDataControl", setup = "missing", nrep = "missing", 
  control = "SimControl"}{run a model-based simulation experiment without 
  repetitions (probably useless, but for completeness).}

\item{x = "VirtualDataControl", setup = "missing", nrep = "numeric", 
  control = "SimControl"}{run a model-based simulation experiment with 
  repetitions.}
}}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{runSimulation}}, \code{\linkS4class{SimControl}}, 
  \code{\linkS4class{SimResults}}, \code{\link{simBwplot}}, 
  \code{\link{simDensityplot}}, \code{\link{simXyplot}}
}
\keyword{methods}
\keyword{design}
