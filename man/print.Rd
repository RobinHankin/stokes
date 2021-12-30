\name{print.stokes}
\alias{print.stokes}
\alias{print.kform}
\alias{print.ktensor}
\title{Print methods for \eqn{k}-tensors and \eqn{k}-forms}
\description{\loadmathjax
  Print methods for
 objects with options for printing in matrix form
or multivariate polynomial form
}
\usage{
\method{print}{kform}(x, ...)
\method{print}{ktensor}(x, ...)
}
\arguments{
  \item{x}{\mjseqn{k}-form or \mjseqn{k}-tensor}
  \item{...}{Further arguments (currently ignored)}
}
\details{\loadmathjax
  The print method is designed to tell the user that an object is a
  tensor or a \mjseqn{k}-form.  It prints a message to this effect (with
  special dispensation for zero tensors), then calls the \code{spray}
  print method.
}
\value{Returns its argument invisibly.}
\author{Robin K. S. Hankin}
\note{

  The print method asserts that its argument is a map from \mjseqn{R^n}
  to \mjseqn{R}, where \mjseqn{n} is the largest element in the index
  matrix.  However, such a map naturally furnishes a map from
  \mjseqn{R^m} to \mjseqn{R} provided that \mjeqn{m\geq n}{m>=n} via the
  natural projection from \mjseqn{R^n} to \mjseqn{R^m}.  Formally this
  would be
  \mjeqn{\left(x_1,\ldots,x_n\right)\mapsto\left(x_1,\ldots,x_n,0,\ldots,0\right)\in
  R^m}{equation omitted}.  In the case of the zero \mjseqn{k}-form or
  \mjseqn{k}-tensor, \dQuote{\code{n}} is to be interpreted as
  \dQuote{any \mjeqn{n\geq 0}{n>=0}}.

  By default, the print method uses the \pkg{spray} print methods, and
  as such respects the \code{polyform} option.

  However, the print method is sensitive to the
  \code{stokes_symbolic_print} option.  If \code{TRUE}, it uses
  \code{as.symbolic()} to give an alternate way of displaying
  \mjseqn{k}-forms.

}
\seealso{\code{\link{as.symbolic}}}
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
