\name{Ops.kform}
\alias{Ops.kform}
\alias{Ops.stokes}
\alias{Ops.ktensor}
\alias{Ops}
\title{Arithmetic Ops Group Methods for \code{kform} and \code{ktensor}
  objects}
\description{\loadmathjax
  Allows arithmetic operators to be used for \mjseqn{k}-forms and
  \mjseqn{k}-tensors such as addition, multiplication, etc, where
  defined.
}
\usage{
\method{Ops}{kform}(e1, e2 = NULL)
\method{Ops}{ktensor}(e1, e2 = NULL)
}
\arguments{
  \item{e1,e2}{Objects of class \code{kform} or \code{ktensor}}
}
\details{
  The functions \code{Ops.kform()} and \code{Ops.ktensor()} pass unary
  and binary arithmetic operators (\dQuote{\code{+}}, \dQuote{\code{-}},
  \dQuote{\code{*}}, \dQuote{\code{/}} and \dQuote{\code{^}}) to the
  appropriate specialist function by coercing to \code{spray} objects.

  For wedge products of \mjseqn{k}-forms, use \code{wedge()} or
  \code{\%^\%} or \code{^}; and for tensor products of
  \mjseqn{k}-tensors, use \code{tensorprod()} or \code{\%X\%}.
}
\value{All functions documented here return an object of class
  \code{kform} or \code{ktensor}.
}
\author{Robin K. S. Hankin}
\note{

  A plain asterisk, \dQuote{\code{*}} behaves differently for ktensors
  and kforms.  Given two ktensors \code{T1}, \code{T2}, then
  \dQuote{\code{T1*T2}} will return the their tensor product.  This on
  the grounds that the idiom has only one natural interpretation.  But
  its use is discouraged (use \code{\%X\%} or \code{tensorprod()}
  instead).  An asterisk can also be used to multiply a tensor by a
  scalar, as in \code{T1*5}.

  An asterisk cannot be used to multiply two kforms \code{K1},
  \code{K2}, as in \code{K1*K2}, which will always return an error.
  This on the grounds that it has no sensible interpretation in general
  and you probably meant to use a wedge product, \code{K1^K2}.  Note
  that multiplication by scalars is acceptable, as in \code{K1*6}.
  Further note that \code{K1*K2} returns an error even if one or both is
  a 0-form (or scalar), as in \code{K1*scalar(3)}.  This behaviour may
  change in the future.

  In the package the caret (\dQuote{\code{^}}) evaluates the wedge
  product; note that \code{\%^\%} is also acceptable.  Powers simply do
  not make sense for alternating forms: \code{S \%^\% S = S^S} is zero
  identically.  Here the caret is interpreted consistently as a wedge
  product, and if one of the factors is numeric it is interpreted as a
  zero-form (that is, a scalar).  Thus \code{S^2 = wedge(S,2) = 2^S =
  S*2 = S+S}, and indeed \code{S^n==S*n}.  Caveat emptor!  If \code{S}
  is a \code{kform} object, it is very tempting [but incorrect] to
  interpret \dQuote{\code{S^3}} as something like \dQuote{\code{S} to
  the power 3}.  See also the note at \code{Ops.clifford} in the
  \pkg{clifford} package.

  Powers are not implemented for ktensors on the grounds that a ktensor
  to the power zero is not defined.

  Note that one has to take care with order of operations, as package
  idiom is not quite associative if we mix \code{^} with \code{*}.  For
  example, \code{dx ^ (6*dy)} is perfectly acceptable; but \code{(dx ^
  6)*dy)} will return an error, as will the unbracketed form \code{dx ^
  6 * dy}.  In the second case we attempt to use an asterisk to multiply
  two k-forms, which triggers the error. 
  
}
\examples{
## dx_1 ^ dx_2 + 6dx_5 ^ dx_6:
as.kform(1) ^ as.kform(2) + 6*as.kform(5) ^ as.kform(6)

k1 <- kform_general(4,2,rnorm(6))
k2 <- kform_general(4,2,rnorm(6))

E <- matrix(rnorm(8),4,2)
as.function(k1+k2)(E)

## verify linearity, here 2*k1 + 3*k2:
as.function(2*k1+3*k2)(E)-(2*as.function(k1)(E) + 3*as.function(k2)(E))
## should be small
}
\keyword{symbolmath}
