\name{SimResults-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResults-class}
\alias{summary,SimResults-method}

\title{Class "SimResults"}
\description{Class for simulation results.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SimResults", \dots)} or 
\code{SimResults(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{values}:}{Object of class \code{"data.frame"} containing the 
      simulation results.}
    \item{\code{add}:}{Object of class \code{"list"} containing additional 
      simulation results, e.g., statistical models.}
    \item{\code{design}:}{Object of class \code{"character"} giving the 
      variables (columns) defining the domains used in the simulation 
      experiment.}
    \item{\code{colnames}:}{Object of class \code{"character"} giving the names 
      of the columns of \code{values} that contain the actual simulation 
      results.}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} containing the 
      contamination levels used in the simulation experiment.}
    \item{\code{NArate}:}{Object of class \code{"NumericMatrix"} containing the 
      missing value rates used in the simulation experiment.}
    \item{\code{seed}:}{Object of class \code{"list"} containing the seeds of 
      the random number generator before and after the simulation experiment, 
      respectively (for replication of the results).}
    \item{\code{call}:}{Object of class \code{"SimCall"}; the function call 
       used to run the simulation experiment, or \code{NULL}.}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "SimResults", y = "missing")}: selects a 
      suitable graphical representation of the simulation results 
      automatically.}
    \item{simBwplot}{\code{signature(x = "SimResults")}: conditional 
      box-and-whisker plot of simulation results.}
    \item{simDensityplot}{\code{signature(x = "SimResults")}: conditional 
      kernel density plot of simulation results.}
    \item{simXyplot}{\code{signature(x = "SimResults")}: conditional x-y 
      plot of simulation results.}
    \item{summary}{\code{signature(x = "SimResults")}: summarize 
      simulation results.}
  }
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{runSimulation}}, \code{\link{simBwplot}}, 
  \code{\link{simDensityplot}}, \code{\link{simXyplot}}
}
\examples{
showClass("SimResults")
}
\keyword{classes}
