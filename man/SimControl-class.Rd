\name{SimControl-class}
\Rdversion{1.1}
\docType{class}
\alias{SimControl-class}

\title{Class "SimControl"}
\description{Class for controlling how simulation runs are performed.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SimControl", \dots)} or 
\code{SimControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{contControl}:}{Object of class \code{"OptContControl"}; a 
      control object for contamination, or \code{NULL}.}
    \item{\code{NAControl}:}{Object of class \code{"OptNAControl"}; a control 
      object for inserting missing values, or \code{NULL}.}
    \item{\code{design}:}{Object of class \code{"character"} specifying the 
      variables (columns) to be used for splitting the data into domains.  The 
      simulations, including contamination and the insertion of missing values 
      (unless \code{SAE=TRUE}), are then performed on every domain.}
    \item{\code{fun}:}{Object of class \code{"function"} to be applied in each 
      simulation run.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional 
      arguments to be passed to \code{fun}.}
    \item{\code{SAE}:}{Object of class \code{"logical"} indicating whether 
      small area estimation will be used in the simulation.}
  }
}
\section{Details}{
  There are some requirements for \code{fun}.  It must return a numeric vector 
  or an object of class \code{"SimResult"}, which consists of a slot 
  \code{values} (a numeric vector) and a slot \code{add} (additional 
  results of any class, e.g., statistical models).  Note that the latter 
  is computationally more expensive.  Returning a list with components 
  \code{values} and \code{add} is also accepted and slightly faster than using 
  a \code{"SimResult"} object.  A \code{data.frame} is passed to 
  \code{fun} in every simulation run.  The corresponding argument must be 
  called \code{x}.  If comparisons with the original data need to be made, 
  e.g., for evaluating the quality of imputation methods, the function should 
  have an argument called \code{orig}.  If different domains are used in the 
  simulation, the indices of the current domain can be passed to the function 
  via an argument called \code{domain}.
  
  For small area estimation, the following points have to be kept in mind.  The 
  \code{design} for splitting the data must be supplied and \code{SAE} 
  must be set to \code{TRUE}.  However, the data are not actually split into 
  the specified domains.  Instead, the whole data set (sample) is passed to 
  \code{fun}.  Also contamination and missing values are added to the whole 
  data (sample).  Last, but not least, the function must have a \code{domain} 
  argument so that the current domain can be extracted from the whole data 
  (sample).
  
  In every simulation run, \code{fun} is evaluated using \code{try}.  Hence 
  no results are lost if computations fail in any of the simulation runs.
}
\section{Methods}{
  \describe{
    \item{clusterRunSimulation}{\code{signature(cl = "ANY", x = "data.frame", 
      setup = "VirtualSampleControl", nrep = "missing", 
      control = "SimControl")}: run a simulation experiment on a \code{snow} 
      cluster.}
    \item{clusterRunSimulation}{\code{signature(cl = "ANY", x = "data.frame", 
      setup = "SampleSetup", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment on a \code{snow} cluster.}
    \item{clusterRunSimulation}{\code{signature(cl = "ANY", 
      x = "VirtualDataControl", setup = "missing", nrep = "numeric", 
      control = "SimControl")}: run a simulation experiment on a \code{snow} 
      cluster.}
    \item{runSimulation}{\code{signature(x = "data.frame", 
      setup = "VirtualSampleControl", nrep = "missing", 
      control = "SimControl")}: run a simulation experiment.}
    \item{runSimulation}{\code{signature(x = "data.frame", 
      setup = "SampleSetup", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment.}
    \item{runSimulation}{\code{signature(x = "data.frame", setup = "missing", 
      nrep = "numeric", control = "SimControl")}: run a simulation experiment.}
    \item{runSimulation}{\code{signature(x = "data.frame", setup = "missing", 
      nrep = "missing", control = "SimControl")}: run a simulation experiment.}
    \item{runSimulation}{\code{signature(x = "VirtualDataControl", 
      setup = "missing", nrep = "numeric", control = "SimControl")}: run a 
      simulation experiment.}
    \item{runSimulation}{\code{signature(x = "VirtualDataControl", 
      setup = "missing", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{runSimulation}}, \code{\linkS4class{SimResults}}
}
\examples{
showClass("SimControl")
}
\keyword{classes}
