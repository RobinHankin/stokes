\name{print.stokes}
\alias{print.stokes}
\alias{print.kform}
\alias{print.ktensor}
\title{Print methods for \eqn{k}-tensors and \eqn{k}-forms}
\description{
  Print methods for
 objects with options for printing in matrix form
or multivariate polynomial form
}
\usage{
\method{print}{kform}(x, ...)
\method{print}{ktensor}(x, ...)
}
\arguments{
  \item{x}{\eqn{k}-form or \eqn{k}-tensor}
  \item{...}{Further arguments (currently ignored)}
}
\details{
  The print method is designed to tell the user that an object is a
  tensor or a \eqn{k}-form.  It prints a message to this effect (with
  special dispensation for zero tensors), then calls the \code{spray}
  print method.
}
\value{
Returns its argument invisibly.
}
\author{Robin K. S. Hankin}
\examples{

rform()
rtensor()



## spray print options work:
options(polyform = TRUE)
rtensor()


## reset to default 
options(polyform = FALSE)

}



\keyword{symbolmath}
