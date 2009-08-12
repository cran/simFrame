\name{simSample}
\Rdversion{1.1}
\alias{simSample}

\title{Set up multiple samples}
\description{
A convenience wrapper for setting up multiple samples using \code{\link{setup}} 
with control class \code{\linkS4class{SampleControl}}.
}
\usage{
simSample(x, design = character(), group = character(), 
          method = srs, size = NULL, prob = NULL, \dots, k = 1)
}
\arguments{
  \item{x}{the \code{data.frame} to sample from.}
  \item{design}{a character, logical or numeric vector specifying the variables 
    (columns) to be used for stratified sampling.}
  \item{group}{a character string, single integer or logical vector specifying 
    a variable (column) to be used for sampling whole groups rather than 
    individual observations.}
  \item{method}{a function to be used for sampling (defaults to 
    \code{\link{srs}}). It should return a vector containing the indices 
    of the sampled items (observations or groups).}
  \item{size}{an optional non-negative integer giving the number of items 
    (observations or groups) to sample.  For stratified sampling, 
    a vector of non-negative integers, each giving the number of items 
    to sample from the corresponding stratum.}
  \item{prob}{an optional numeric vector giving the probability weights}
  \item{\dots}{additional arguments to be passed to \code{method}.}
  \item{k}{a single positive integer giving the number of samples to be set 
    up.}
}
\details{
  There are some restrictions on the argument names of the function 
  supplied to \code{method}.  If it needs population data as input, 
  the corresponding argument should be called \code{x} and should expect 
  a \code{data.frame}.  If the sampling method only needs the population size 
  as input, the argument should be called \code{N}.  Note that \code{method} is 
  not expected to have both \code{x} and \code{N} as arguments, and that the 
  latter is much faster for stratified sampling or group sampling.  
  Furthermore, if the function has arguments for sample size and probability 
  weights, they should be called \code{size} and \code{prob}, respectively.  
  Note that a function with \code{prob} as its only argument is perfectly valid 
  (for probability proportional to size sampling).  Further arguments of 
  \code{method} may be passed directly via the \dots argument.
}
\value{
  An object of class \code{"SampleSetup"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
    \code{\link{setup}}, \code{\linkS4class{SampleControl}}, 
    \code{\linkS4class{SampleSetup}}
}
\examples{
data(eusilc)

## simple random sampling
srss <- simSample(eusilc, size = 20, k = 3)
draw(eusilc[, c("id", "eqIncome")], srss, i = 1)

## group sampling
gss <- simSample(eusilc, group = "hid", size = 10, k = 3)
draw(eusilc[, c("hid", "id", "eqIncome")], gss, i = 2)

## stratified sampling
stss <- simSample(eusilc, design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), k = 3)
draw(eusilc[, c("id", "region", "eqIncome")], stss, i = 3)
}
\keyword{distribution}
