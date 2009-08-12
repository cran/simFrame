\name{simDensityplot}
\Rdversion{1.1}
\alias{simDensityplot}

\title{Kernel density plots}
\description{
Generic function for producing kernel density plots.
}
\usage{
simDensityplot(x, \dots)
}
\arguments{
  \item{x}{the object to be plotted.}
  \item{\dots}{additional arguments to be passed down to methods.}
}
\value{
  An object of class \code{"trellis"}.  The 
  \code{\link[lattice:update.trellis]{update}} method can be used to update 
  components of the object and the \code{\link[lattice:print.trellis]{print}} 
  method (usually called by default) will plot it on an appropriate plotting 
  device.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link{simBwplot}}, \code{\link{simXyplot}}, 
  \code{\link[lattice]{densityplot}}, \code{\linkS4class{SimResults}}
}
\keyword{hplot}
