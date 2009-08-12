\name{getProb}
\Rdversion{1.1}
\alias{getProb}

\title{Inclusion probabilities}
\description{
Get the first-order inclusion probabilities from a vector of probability 
weights.
}
\usage{
getProb(prob, size)
}
\arguments{
  \item{prob}{a numeric vector of non-negative probability weights.}
  \item{size}{a non-negative integer giving the sample size.}
}
\value{
  A numeric vector of the first-order inclusion probabilities.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\note{
  This is a faster C implementation of 
  \code{\link[sampling]{inclusionprobabilities}} from package \code{sampling}.
}
\seealso{
  \code{\link{setup}}, \code{\linkS4class{SampleSetup}}
}
\examples{
pweights <- sample(1:5, 25, replace = TRUE)
getProb(pweights, 10)
}
\keyword{distribution}
\keyword{survey}
