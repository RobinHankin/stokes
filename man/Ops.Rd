\name{Ops.kform}
\alias{Ops.kform}
\alias{Ops.ktensor}
\alias{Ops}
\title{Arithmetic Ops Group Methods for \code{kform} and \code{ktensor}
  objects}
\description{
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
  \dQuote{\code{*}}, and \dQuote{\code{/}}) to the appropriate
  specialist function by coercing to \code{spray} objects.

  For wedge products of \mjseqn{k}-forms, use \code{wedge()} or
  \code{\%^\%}; and for cross products of \mjseqn{k}-tensors, use
  \code{cross()} or \code{\%X\%}.
}

\value{All functions documented here return an object of class
  \code{kform} or \code{ktensor}.
}
\author{Robin K. S. Hankin}
\note{

  A plain asterisk, \dQuote{\code{*}}, given two ktensors, will return
  the cross product, on the grounds that the idiom has only one natural
  interpretation.  But its use is discouraged.

  In the package the caret (\dQuote{\code{^}}) evaluates the wedge
  product; note that \code{\%^\%} is also acceptable.  Of course, if
  \code{S} is a \code{kform} object, it is very tempting to interpret
  \dQuote{\code{S^3}} as something like \dQuote{\code{S} to the power
  3}.  Further, if we interpret a caret as a power, idiom such as
  \dQuote{\code{2^S}} becomes meaningless.

  But powers do not make sense for alternating forms: \code{a \%^\% a}
  is zero identically.  Here the caret is interpreted consistently as a
  wedge product, and if one of the factors is numeric it is interpreted
  as a zero-form (that is, a scalar).  Thus \code{a^2 == 2^a == a+a},
  and indeed \code{a^n==a*n}.

  Powers are not implemented for ktensors on the grounds that a ktensor
  to the power zero is not defined.
  
}
\examples{
## dx_1 ^ dx_2 + 6dx_5 ^ dx_6:
as.kform(1) ^ as.kform(2) + 6*as.kform(5) ^ as.kform(6)

k1 <- kform_general(4,2,rnorm(6))
k2 <- kform_general(4,2,rnorm(6))

E <- matrix(rnorm(8),4,2)
as.function(k1+k2)(E)


as.function(2*k1+3*k2)(E)-(2*as.function(k1)(E) + 3*as.function(k1)(E))
## should be small
}
\keyword{symbolmath}
