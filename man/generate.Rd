\name{generate}
\Rdversion{1.1}
\docType{methods}
\alias{generate}
\alias{generate-methods}
\alias{generate,character-method}
\alias{generate,DataControl-method}
\alias{generate,missing-method}

\title{Generate data}
\description{
Generic function for generating data based on a (distribution) model.
}
\usage{
generate(control, \dots)

\S4method{generate}{DataControl}(control)
}
\arguments{
  \item{control}{a control object inheriting from the virtual class
    \code{"VirtualDataControl"} or a character string specifying such a
    control class (the default being \code{"DataControl"}).}
  \item{\dots}{if \code{control} is a character string or missing, the slots of
    the control object may be supplied as additional arguments.  See
    \code{"\linkS4class{DataControl}"} for details on the slots.}
}
\details{
  The control class \code{"DataControl"} is quite simple but general.  For
  user-defined data generation, it often suffices to implement a function and
  use it as the \code{distribution} slot in the \code{"DataControl"} object.
  See \code{"\linkS4class{DataControl}"} for some requirements for such a
  function.

  However, if more specialized data generation models are required, the
  framework can be extended by defining a control class \code{"MyDataControl"}
  extending \code{"\linkS4class{VirtualDataControl}"} and the corresponding
  method \code{generate(control)} with signature \code{'MyDataControl'}.  If,
  e.g., a specific distribution or mixture of distributions is frequently used
  in simulation experiments, a distinct control class may be more convenient
  for the user.
}
\value{
  A \code{data.frame}.
}
\section{Methods}{
  \describe{
  \item{\code{control = "character"}}{generate data using a control class
    specified by the character string \code{control}.  The slots of the control
    object may be supplied as additional arguments.}
  \item{\code{control = "missing"}}{generate data using a control object of
    class \code{"DataControl"}.  Its slots may be supplied as additional
    arguments.}
  \item{\code{control = "DataControl"}}{generate data as defined by the control
    object \code{control}.}
  }
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{"\linkS4class{DataControl}"}, \code{"\linkS4class{VirtualDataControl}"}
}
\examples{
# using a control object
dc <- DataControl(size = 10, distribution = rnorm,
    dots = list(mean = 0, sd = 2))
generate(dc)

# supply slots of control object as arguments
generate(size = 10, distribution = rnorm,
    dots = list(mean = 0, sd = 2))
}
\keyword{distribution}
\keyword{methods}
