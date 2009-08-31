\name{eusilc}
\Rdversion{1.1}
\alias{eusilc}
\docType{data}
\title{
Artificial EU-SILC data
}
\description{
This data set is synthetically generated from real Austrian EU-SILC (European 
Union Statistics on Income and Living Conditions) data.
}
\usage{data(eusilc)}
\format{
  A \code{data.frame} with 58 191 observations on the following 19 variables:
  \describe{
    \item{hid}{the household ID.}
    \item{region}{the federal state in which the household is located.}
    \item{hsize}{the number of persons in the household.}
    \item{eqsize}{the equivalized household size according to the modified 
      OECD scale.}
    \item{eqIncome}{a simplified version of the equivalized household income.}
    \item{pid}{the personal ID.}
    \item{id}{the household ID combined with the personal ID.  The first five 
      digits represent the household ID, the last two digits the personal ID 
      (both with leading zeros).}
    \item{age}{the person's age.}
    \item{gender}{the person's gender.}
    \item{ecoStat}{the person's economic status.}
    \item{citizenship}{the person's citizenship (\code{AT}, \code{EU} or 
      \code{Other}).}
    \item{persNetIncome}{the personal net income.}
    \item{py010n}{employee cash or near cash income (net).}
    \item{py050n}{cash benefits or losses from self-employment (net).}
    \item{py090n}{unemployment benefits (net).}
    \item{py100n}{old-age benefits (net).}
    \item{py110n}{survivor's benefits (net).}
    \item{py120n}{sickness benefits (net).}
    \item{py130n}{disability benefits (net).}
    \item{py140n}{education-related allowances (net).}
  }
}
\details{
The data set is used as population data in some of the examples in package 
\code{simFrame}.  Note that it is included for illustrative purposes only.  It 
consists of 25 000 households, hence it does not represent the true population 
sizes of Austria and its regions.

Some variable names are different from the standardized names used by the 
statistical agencies, as the latter are rather cryptic codes.  Moreover, as 
household income components are not yet available in the synthetic data set, 
the equivalized household income is computed only from the personal net income 
of the household members.
}
\source{
This is a synthetic data set based on Austrian EU-SILC data from 2006.  The 
original sample was provided by Statistics Austria.
}
\references{
Kraft, S. (2009) \emph{Simulation of a Population for the European Income and 
Living Conditions Survey}. Master's thesis, Department of Statistics and 
Probability Theory, Vienna University of Technology, Vienna, Austria.
}
\examples{
data(eusilc)
stratify(eusilc, design = "region")
}
\keyword{datasets}
