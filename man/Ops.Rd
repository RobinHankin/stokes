\name{Ops.kform}
\alias{Ops.kform}
\alias{Ops.stokes}
\alias{Ops.ktensor}
\alias{Ops}
\title{Arithmetic Ops Group Methods for \code{kform} and \code{ktensor}
  objects}
\description{
  Allows arithmetic operators to be used for \eqn{k}-forms and
  \eqn{k}-tensors such as addition, multiplication, etc, where
  defined.
}
\usage{
\method{Ops}{kform}(e1, e2 = NULL)
\method{Ops}{ktensor}(e1, e2 = NULL)
}
\arguments{
  \item{e1, e2}{Objects of class \code{kform} or \code{ktensor}}
}
\details{
  The functions \code{Ops.kform()} and \code{Ops.ktensor()} pass unary
  and binary arithmetic operators (\dQuote{\code{+}}, \dQuote{\code{-}},
  \dQuote{\code{*}}, \dQuote{\code{/}} and \dQuote{\code{^}}) to the
  appropriate specialist function by coercing to \code{spray} objects.

  Binary addition and subtraction, as in \code{x+y} or \code{x-y}, are
  performed by \code{spray::spray_plus_spray()}.

  For wedge products of \eqn{k}-forms, use \code{wedge()} or
  \code{\%^\%} or \code{^}; and for tensor products of
  \eqn{k}-tensors, use \code{tensorprod()} or \code{\%X\%}.
}
\value{All functions documented here return an object of class
  \code{kform} or \code{ktensor}.
}
\author{Robin K. S. Hankin}
\note{

  A plain asterisk, \dQuote{\code{*}} behaves differently for ktensors
  and kforms.

  Given two \strong{ktensors} \code{T1} and \code{T2}, then
  \dQuote{\code{T1*T2}} will return their tensor product,
  \code{tensorprod(T1,T2)}.  This on the grounds that the idiom has only
  one natural interpretation.  But its use is discouraged (use
  \code{\%X\%} or \code{tensorprod()} instead).  An asterisk can also be
  used to multiply a tensor by a scalar, as in \code{T1*5}, in which the
  scalar is interpreted as a \eqn{0}-tensor.

  Given two \strong{kforms} \code{K1} and \code{K2}, asterisks behave
  differently.  An asterisk cannot be used to multiply \code{K1} and
  \code{K2}: \code{K1*K2} will return an error.  Multiplication by
  scalars is OK, as in \code{K1*6}.

  Powers simply do not make sense for alternating forms: if we wish to
  calculate \eqn{S^2} (that is, the square of \eqn{S}), then the only
  natural interpretation of this is \eqn{S^S}, but \code{S \%^\% S =
  S^S} is zero identically.  In the package, therefore, the caret
  (\dQuote{\code{^}}) evaluates the wedge product; note that
  \code{\%^\%} is also acceptable.

  Here the caret is interpreted consistently as a wedge product, and if
  one of the factors is numeric it is interpreted as a zero-form (that
  is, a scalar).  Thus \code{S^2 = wedge(S,2) = 2^S = S*2 = S+S}, and
  indeed \code{S^n==S*n}.  Caveat emptor!  If \code{S} is a \code{kform}
  object, it is very tempting [but incorrect] to interpret
  \dQuote{\code{S^3}} as something like \dQuote{\code{S} to the power
  3}.  See also the note at \code{Ops.clifford} in the
  \CRANpkg{clifford} package.

  Powers are not implemented for ktensors on the grounds that a ktensor
  to the power zero is ot defined.

  Note that one has to take care with order of operations if we mix
  \code{^} with \code{*}.  For example, \code{dx ^ (6*dy)} is perfectly
  acceptable; but \code{(dx ^ 6)*dy)} will return an error, as will the
  unbracketed form \code{dx ^ 6 * dy}.  In the second case we attempt to
  use an asterisk to multiply two k-forms, which triggers the error.
  
}
\examples{
## dx_1 ^ dx_2 + 6dx_5 ^ dx_6:
as.kform(1) ^ as.kform(2) + 6*as.kform(5) ^ as.kform(6)

k1 <- kform_general(4, 2, rnorm(6))
k2 <- kform_general(4, 2, rnorm(6))

E <- matrix(rnorm(8), 4, 2)
as.function(k1 + k2)(E)

## verify linearity, here 2*k1 + 3*k2:
as.function(2*k1 + 3*k2)(E)-(2*as.function(k1)(E) + 3*as.function(k2)(E))
## should be small
}
\keyword{symbolmath}
