\name{simFrame-package}
\Rdversion{1.1}
\alias{simFrame-package}
\alias{simFrame}
%% aliases to avoid confusion due to capitalization
\alias{simframe-package}
\alias{SimFrame-package}
\alias{Simframe-package}
\alias{simframe}
\alias{SimFrame}
\alias{Simframe}

\docType{package}
\title{
Simulation framework
}
\description{
A general framework for statistical simulation.
}
\details{
\tabular{ll}{
Package: \tab simFrame\cr
Version: \tab 0.3\cr
Date: \tab 2010-08-10\cr
Depends: \tab R (>= 2.8.0), lattice, snow\cr
Imports: \tab lattice, methods, stats, stats4, utils\cr
Suggests: \tab laeken, mvtnorm, robCompositions, sampling\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

Index:
\preformatted{
BasicVector-class       Class "BasicVector"
ContControl             Create contamination control objects
ContControl-class       Class "ContControl"
DARContControl          Create objects of class "DARContControl"
DARContControl-class    Class "DARContControl"
DCARContControl         Create objects of class "DCARContControl"
DCARContControl-class   Class "DCARContControl"
DataControl             Create objects of class "DataControl"
DataControl-class       Class "DataControl"
NAControl               Create objects of class "NAControl"
NAControl-class         Class "NAControl"
NumericMatrix-class     Class "NumericMatrix"
OptCall-class           Class "OptCall"
OptCharacter-class      Class "OptCharacter"
OptContControl-class    Class "OptContControl"
OptDataControl-class    Class "OptDataControl"
OptNAControl-class      Class "OptNAControl"
OptNumeric-class        Class "OptNumeric"
OptSampleControl-class
                        Class "OptSampleControl"
SampleControl           Create objects of class "SampleControl"
SampleControl-class     Class "SampleControl"
SampleSetup             Create objects of class "SampleSetup"
SampleSetup-class       Class "SampleSetup"
SimControl              Create objects of class "SimControl"
SimControl-class        Class "SimControl"
SimResults              Create objects of class "SimResults"
SimResults-class        Class "SimResults"
Strata                  Create objects of class "Strata"
Strata-class            Class "Strata"
SummarySampleSetup      Create objects of class "SummarySampleSetup"
SummarySampleSetup-class
                        Class "SummarySampleSetup"
VirtualContControl-class
                        Class "VirtualContControl"
VirtualDataControl-class
                        Class "VirtualDataControl"
VirtualNAControl-class
                        Class "VirtualNAControl"
VirtualSampleControl-class
                        Class "VirtualSampleControl"
aggregate-methods       Method for aggregating simulation results
clusterRunSimulation    Run a simulation experiment on a snow cluster
clusterSetup            Set up multiple samples on a snow cluster
contaminate             Contaminate data
draw                    Draw a sample
eusilcP                 Synthetic EU-SILC data
generate                Generate data
getAdd                  Accessor and mutator functions for objects
getStrataLegend         Utility functions for stratifying data
head-methods            Methods for returning the first parts of an
                        object
inclusionProb           Inclusion probabilities
length-methods          Methods for getting the length of an object
plot-methods            Plot simulation results
runSimulation           Run a simulation experiment
setNA                   Set missing values
setup                   Set up multiple samples
simApply                Apply a function to subsets
simBwplot               Box-and-whisker plots
simDensityplot          Kernel density plots
simFrame-package        Simulation framework
simSample               Set up multiple samples
simXyplot               X-Y plots
srs                     Random sampling
stratify                Stratify data
summary-methods         Methods for producing a summary of an object
tail-methods            Methods for returning the last parts of an
                        object
}

Further information is available in the following vignettes:
\tabular{ll}{
\code{simFrame-eusilc} \tab Applications of Statistical Simulation in the Case of EU-SILC: Using the R Package simFrame (source, pdf)\cr
\code{simFrame-intro} \tab An Object-Oriented Framework for Statistical Simulation: The R Package simFrame (source, pdf)\cr
}
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\author{
Andreas Alfons

Maintainer: Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}
}
\keyword{package}
