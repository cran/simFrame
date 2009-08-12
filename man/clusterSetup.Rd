\name{clusterSetup}
\Rdversion{1.1}
\alias{clusterSetup}
\alias{clusterSetup,ANY,data.frame,SampleControl-method}

\title{Set up multiple samples on a snow cluster}
\description{
Generic function for setting up multiple samples on a \code{snow} cluster.
}
\usage{
clusterSetup(cl, x, control, \dots)

\S4method{clusterSetup}{ANY,data.frame,SampleControl}(cl, x, control)
}
\arguments{
  \item{cl}{a \code{snow} cluster.}
  \item{x}{the \code{data.frame} to sample from.}
  \item{control}{a control object inheriting from the virtual class 
    \code{"VirtualSampleControl"} or a character string specifying such a 
    control class (the default being \code{"SampleControl"}).}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.}
}
\details{
  The computational performance of setting up multiple samples can be increased 
  by parallel computing.  In \code{simFrame}, parallel computing is implemented 
  using the package \code{snow}.  Note that all objects and packages required 
  for the computations (including \code{simFrame}) need to be made available on 
  every worker process.
   
  In order to prevent problems with random numbers and to ensure 
  reproducibility, random number streams should be used.  In \R, the packages 
  \code{rlecuyer} and \code{rsprng} are available for creating random number 
  streams, which are supported by \code{snow} via the function 
  \code{clusterSetupRNG}.
  
  The control class \code{"SampleControl"} is highly flexible and allows 
  stratified sampling as well as sampling of whole groups rather than 
  individuals with a specified sampling method.  Hence it is often sufficient 
  to implement the desired sampling method for the simple non-stratified case 
  to extend the existing framework.  See \code{"\linkS4class{SampleControl}"} 
  for some restrictions on the argument names of such a function, which should 
  return a vector containing the indices of the sampled observations.
  
  Nevertheless, for very complex sampling procedures, it is possible to define 
  a control class \code{"MySampleControl"} extending 
  \code{"VirtualSampleControl"}, and the corresponding method 
  \code{clusterSetup(cl, x, control)} with signature \code{'ANY, data.frame, 
  MySampleControl'}.  In order to optimize computational performance, it is 
  necessary to efficiently set up multiple samples.  Thereby the slot \code{k} 
  of \code{"VirtualSampleControl"} needs to be used to control the number of 
  samples, and the resulting object must be of class \code{"SampleSetup"}.
}
\value{
  An object of class \code{"SampleSetup"}.
}
\references{
L'Ecuyer, P., Simard, R., Chen E and Kelton, W. (2002) An object-oriented 
random-number package with many long streams and substreams. \emph{Operations 
Research}, \bold{50}(6), 1073--1075.

Mascagni, M. and Srinivasan, A. (2000) Algorithm 806: \code{SPRNG}: a scalable 
library for pseudorandom number generation. \emph{ACM Transactions on 
Mathematical Software}, \bold{26}(3), 436--461.

Rossini, A., Tierney L. and Li, N. (2007) Simple parallel statistical computing 
in \R. \emph{Journal of Computational and Graphical Statistics}, \bold{16}(2), 
399--420.

Tierney, L., Rossini, A. and Li, N. (2009) \code{snow}: A parallel computing 
framework for the \R system. \emph{International Journal of Parallel 
Programming}, \bold{37}(1), 78--90.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\link[snow]{makeCluster}}, \code{\link[snow]{clusterSetupRNG}}, 
  \code{\link{setup}}, \code{\link{draw}}, \code{\linkS4class{SampleControl}}, 
  \code{\linkS4class{VirtualSampleControl}}, \code{\linkS4class{SampleSetup}}
}
\examples{
\dontrun{
# these examples require at least dual core processor

# load data
data(eusilc)

# start snow cluster
cl <- makeCluster(2, type = "SOCK")

# load package and data on workers
clusterEvalQ(cl, {
        library(simFrame)
        data(eusilc)
    })

# simple random sampling
srss <- clusterSetup(cl, eusilc, size = 20, k = 4)
draw(eusilc[, c("id", "eqIncome")], srss, i = 1)

# group sampling
gss <- clusterSetup(cl, eusilc, group = "hid", size = 10, k = 4)
draw(eusilc[, c("hid", "id", "eqIncome")], gss, i = 2)

# stratified sampling
stss <- clusterSetup(cl, eusilc, design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), k = 4)
draw(eusilc[, c("id", "region", "eqIncome")], stss, i = 3)

# stop cluster
stopCluster(cl)
}
}
\keyword{distribution}
