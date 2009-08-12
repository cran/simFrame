\name{stratify}
\Rdversion{1.1}
\alias{stratify}

\title{Stratify data}
\description{Generic function for stratifying data.}
\usage{
stratify(x, design)
}
\arguments{
  \item{x}{the \code{data.frame} to be stratified.}
  \item{design}{a character, logical or numeric vector specifying the variables 
    (columns) to be used for stratification.}
}
\value{
  An object of class \code{"Strata"}.
}
\author{Andreas Alfons, \email{alfons@statistik.tuwien.ac.at}}
\seealso{
  \code{\linkS4class{Strata}}
}
\examples{
data(eusilc)
stratify(eusilc, c("region", "gender"))
}
\keyword{manip}
