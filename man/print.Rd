\name{print.stokes}
\alias{print.stokes}
\alias{print.kform}
\alias{print.ktensor}
\alias{stokes_symbolic_print}
\alias{kform_symbolic_print}
\alias{ktensor_symbolic_print}
\alias{polyform}
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

Printing is dispatched to \code{print.ktensor()} and
\code{print.kform()} depending on its argument.  Special dispensation is
given for the zero object.

Although \eqn{k}-forms are alternating tensors and thus mathematically
are tensors, they are handled differently.

  The default print method uses the \CRANpkg{spray} print methods,
  and as such respects the \code{polyform} option.  However, setting
  \code{polyform} to \code{TRUE} can give misleading output, because
  \code{spray} objects are interpreted as multivariate polynomials not
  differential forms (and in particular uses the caret to signify
  powers).

  It is much better to use options \code{ktensor_symbolic_print} or
  \code{kform_symbolic_print} instead: the bespoke print methods
  \code{print.kform()} and \code{print.ktensor()} are sensitive to these
  options.

  For \code{kform} objects, if option \code{kform_symbolic_print} is
  non-null, the print method uses \code{as.symbolic()} to give an
  alternate way of displaying \eqn{k}-tensors and \eqn{k}-forms.  The
  generic non-null value for this option would be \dQuote{\code{x}}
  which gives output like \dQuote{\code{dx1 ^ dx2}}.  However, it has
  two special values: set \code{kform_symbolic_print} to
  \dQuote{\code{dx}} for output like \dQuote{\code{dx ^ dz}} and
  \dQuote{\code{txyz}} for output like \dQuote{\code{dt ^ dx}}, useful
  in relativistic physics with a Minkowski metric.  See the examples.

  For \code{ktensor} objects, if option \code{ktensor_symbolic_print} is
  \code{TRUE}, a different system is used.  Given a tensor
  \eqn{3\phi_4\otimes\phi_1 -5\phi_2\otimes\phi_2}, for example (where
  \eqn{\phi_i(x^j)=\delta_i^j}), the method will give output that looks
  like \dQuote{\code{+3 d4*d1 -5 d2*d2}}.  I am not entirely happy with
  this and it might change in future.

  More detail is given at \code{symbolic.Rd} and the \code{dx} vignette.

}
\value{Returns its argument invisibly.}
\author{Robin K. S. Hankin}
\note{

  For both \code{kform} and \code{ktensor} objects,
  the print method asserts that its argument is a map from \eqn{V^k}
  to \eqn{\mathbb{R}}{R} with \eqn{V=\mathbb{R}^n}{V=R^n}.  Here,
  \eqn{n} is the largest element in the index matrix.  However, such
  a map naturally furnishes a map from \eqn{(\mathbb{R}^m)^k}{(R^m)^k}
  to \eqn{\mathbb{R}}{R}, provided that \eqn{m\geq n}{m>=n} via the
  natural projection from \eqn{\mathbb{R}^n}{R^n} to
  \eqn{\mathbb{R}^m}{R^m}.  Formally this would be
  \eqn{\left(x_1,\ldots,x_n\right)\mapsto\left(x_1,\ldots,x_n,0,\ldots,0\right)\in\mathbb{R}^m}{equation
  omitted}.  In the case of the zero \eqn{k}-form or
  \eqn{k}-tensor, \dQuote{\code{n}} is to be interpreted as
  \dQuote{any \eqn{n\geq 0}{n>=0}}.  See also \code{dovs()}.
  

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
