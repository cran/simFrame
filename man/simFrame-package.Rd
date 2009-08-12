\name{simFrame-package}
\Rdversion{1.1}
\alias{simFrame-package}
\alias{simFrame}
\docType{package}
\title{
Simulation Framework
}
\description{
A general framework for statistical simulation.
}
\details{
\tabular{ll}{
Package: \tab simFrame\cr
Version: \tab 0.1\cr
Date: \tab 2009-08-12\cr
Depends: \tab R (>= 2.8.0), lattice, snow\cr
Imports: \tab methods, stats4\cr
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
OptNAControl-class      Class "OptNAControl"
OptNumeric-class        Class "OptNumeric"
SampleControl           Create objects of class "SampleControl"
SampleControl-class     Class "SampleControl"
SampleSetup             Create objects of class "SampleSetup"
SampleSetup-class       Class "SampleSetup"
SimControl              Create objects of class "SimControl"
SimControl-class        Class "SimControl"
SimResult               Create objects of class "SimResult"
SimResult-class         Class "SimResult"
SimResults              Create objects of class "SimResults"
SimResults-class        Class "SimResults"
Strata                  Create objects of class "Strata"
Strata-class            Class "Strata"
VirtualContControl-class
                        Class "VirtualContControl"
VirtualDataControl-class
                        Class "VirtualDataControl"
VirtualNAControl-class
                        Class "VirtualNAControl"
VirtualSampleControl-class
                        Class "VirtualSampleControl"
clusterRunSimulation    Run a simulation experiment on a snow cluster
clusterRunSimulation-methods
                        Methods for running a simulation experiment on
                        a snow cluster
clusterSetup            Set up multiple samples on a snow cluster
clusterSetup-methods    Methods for setting up multiple samples on a
                        snow cluster
contaminate             Contaminate data
contaminate-methods     Methods for contaminating data
draw                    Draw a sample
draw-methods            Methods for drawing a sample
eusilc                  Artificial EU-SILC data
generate                Generate data
generate-methods        Methods for generating data
getProb                 Inclusion probabilities
getStrataLegend         Utility functions for stratifying data
getStrataLegend-methods
                        Methods for Function getStrataLegend
getStrataSplit-methods
                        Methods for Function getStrataSplit
getStrataTable-methods
                        Methods for Function getStrataTable
getStratumSizes-methods
                        Methods for Function getStratumSizes
getStratumValues-methods
                        Methods for Function getStratumValues
plot-methods            Plot simulation results
runSimulation           Run a simulation experiment
runSimulation-methods   Methods for running a simulation experiment
setNA                   Set missing values
setNA-methods           Methods for setting missing values
setup                   Set up multiple samples
setup-methods           Methods for setting up multiple samples
simApply                Apply a function to subsets
simApply-methods        Methods for applying a function to subsets
simBwplot               Box-and-whisker plots
simBwplot-methods       Methods for producing box-and-whisker plots
simDensityplot          Kernel density plots
simDensityplot-methods
                        Methods for producing kernel density plots
simSample               Set up multiple samples
simSapply-methods       Methods for applying a function to subsets
simXyplot               X-Y plots
simXyplot-methods       Methods for producing x-y plots
srs                     Random sampling
stratify                Stratify data
stratify-methods        Methods for stratifying data
}
}
\author{
Andreas Alfons

Maintainer: Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}
}
\keyword{ package }
