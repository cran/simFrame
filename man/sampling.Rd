\name{sampling}
\Rdversion{1.1}
\alias{srs}
\alias{ups}

\title{Random sampling}
\description{
Functions for random sampling.
}
\usage{
srs(N, size, replace = FALSE)

ups(N, size, prob, replace = FALSE)
}
\arguments{
  \item{N}{a non-negative integer giving the number of observations from 
    which to sample.}
  \item{size}{a non-negative integer giving the number of observations to 
    sample.}
  \item{prob}{a numeric vector giving the probability weights.}
  \item{replace}{a logical indicating whether sampling should be performed 
    with or without replacement.}
}
\details{
\code{srs} and \code{ups} are wrappers for simple random sampling and 
unequal probability sampling, respectively. Both functions make use of 
\code{\link{sample}}.
}
\value{
  An integer vector giving the indices of the sampled observations.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{SampleControl}}, \code{\link{setup}}, \code{\link{sample}}
}
\examples{
## simple random sampling without replacement
srs(10, 5)

## simple random sampling with replacement
srs(5, 10, replace = TRUE)

## unequal probability sampling without replacement
ups(10, 5, prob = 1:10)

## unequal probability sampling with replacement
ups(5, 10, prob = 1:5, replace = TRUE)
}
\keyword{distribution}
