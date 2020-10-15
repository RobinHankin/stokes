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
\details{\loadmathjax
  The print method is designed to tell the user that an object is a
  tensor or a \eqn{k}-form.  It prints a message to this effect (with
  special dispensation for zero tensors), then calls the \code{spray}
  print method.
}
\value{Returns its argument invisibly.}
\author{Robin K. S. Hankin}
\note{

  The print method asserts that its argument is a map from \eqn{R^n} to
  \eqn{R}, where \eqn{n} is the largest element in the index matrix.
  However, such a map naturally furnishes a map from \eqn{R^m} to
  \eqn{R} provided that \eqn{m\geq n}{m>=n} via the natural projection
  from \eqn{R^n} to \eqn{R^m}.  Formally this would be
  \mjeqn{\left(x_1,\ldots,x_n\right)\mapsto\left(x_1,\ldots,x_n,0,\ldots,0\right)\in
  R^m}{equation omitted}.  In the case of the zero \eqn{k}-form or
  \eqn{k}-tensor, \dQuote{\code{n}} is to be interpreted as \dQuote{any
    \eqn{n\geq 0}{n>=0}}.
}

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
