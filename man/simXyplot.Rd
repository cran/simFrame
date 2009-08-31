\name{simXyplot}
\Rdversion{1.1}
\alias{simXyplot}

\title{X-Y plots}
\description{
Generic function for producing x-y plots.
}
\usage{
simXyplot(x, \dots)
}
\arguments{
  \item{x}{the object to be plotted.}
  \item{\dots}{additional arguments to be passed down to methods}
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
  \code{\link{simBwplot}}, \code{\link{simDensityplot}}, 
  \code{\link[lattice]{xyplot}}, \code{\linkS4class{SimResults}}
}
\keyword{hplot}