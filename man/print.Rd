\name{print.stokes}
\alias{print.stokes}
\alias{print.kform}
\alias{print.ktensor}
\alias{stokes_symbolic_print}
\alias{kform_symbolic_print}
\alias{ktensor_symbolic_print}
\alias{polyform}
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
\details{
  The print method is designed to tell the user that an object is a
  tensor or a \mjseqn{k}-form.  It prints a message to this effect (with
  special dispensation for zero tensors), then calls the \code{spray}
  print method.
}
\value{Returns its argument invisibly.}
\author{Robin K. S. Hankin}
\note{

  The print method asserts that its argument is a map from \mjseqn{V^k}
  to \mjseqn{R} with \mjseqn{V=R^n}.  Here, \mjseqn{n} is the largest
  element in the index matrix.  However, such a map naturally furnishes
  a map from \mjseqn{(R^m)^k} to \mjseqn{R} provided that \mjeqn{m\geq
  n}{m>=n} via the natural projection from \mjseqn{R^n} to \mjseqn{R^m}.
  Formally this would be
  \mjeqn{\left(x_1,\ldots,x_n\right)\mapsto\left(x_1,\ldots,x_n,0,\ldots,0\right)\in
  R^m}{equation omitted}.  In the case of the zero \mjseqn{k}-form or
  \mjseqn{k}-tensor, \dQuote{\code{n}} is to be interpreted as
  \dQuote{any \mjeqn{n\geq 0}{n>=0}}.  See also \code{dovs()}.
  

  By default, the print method uses the \CRANpkg{spray} print methods,
  and as such respects the \code{polyform} option.  However, setting
  \code{polyform} to \code{TRUE} can give misleading output, because
  \code{spray} objects are interpreted as multivariate polynomials not
  differential forms (and in particular uses the caret to signify
  powers).

  It is much better to use options \code{ktensor_symbolic_print} or
  \code{kform_symbolic_print} instead.  If these options are non-null,
  the print method uses \code{as.symbolic()} to give an alternate way of
  displaying \mjseqn{k}-tensors and \mjseqn{k}-forms.  The generic
  non-null value would be \dQuote{\code{x}} which gives output like
  \dQuote{\code{dx1 ^ dx2}}.  However, it has two special values: set
  \code{kform_symbolic_print} to \dQuote{\code{dx}} for output like
  \dQuote{\code{dx ^ dz}} and \dQuote{\code{txyz}} for output like
  \dQuote{\code{dt ^ dx}}, useful in relativistic physics with a
  Minkowski metric.  See the examples.

  More detail is given at \code{symbolic.Rd} and the
  \code{dx} vignette.

}
\seealso{\code{\link{as.symbolic}},\code{\link{dovs}}}
\examples{

a <- rform()
a

options(kform_symbolic_print = "x")
a


options(kform_symbolic_print = "dx")
kform(spray(kform_basis(3,2),1:3))


kform(spray(kform_basis(4,2),1:6))  # runs out of symbols


options(kform_symbolic_print = "txyz")
kform(spray(kform_basis(4,2),1:6))  # standard notation


options(kform_symbolic_print = NULL) # revert to default
a

}

\keyword{symbolmath}
