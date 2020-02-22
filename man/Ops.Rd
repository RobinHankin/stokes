\name{Ops.kform}
\alias{Ops.kform}
\alias{Ops.ktensor}
\alias{Ops}
\title{Arithmetic Ops Group Methods for \code{kform} and \code{ktensor}
  objects}
\description{
  Allows arithmetic operators to be used for \eqn{k}-forms and
  \eqn{k}-tensors such as addition, multiplication, etc, where defined.
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

  For wedge products of \eqn{k}-forms, use \code{wedge()} or
  \code{\%^\%}; and for cross products of \eqn{k}-tensors, use
  \code{cross()} or \code{\%X\%}.
}

\value{All functions documented here return an object of class \code{kform} or
  \code{ktensor}.
}

\examples{
## dx_1 ^ dx_2 + 6dx_5 ^ dx_6:
as.kform(1) \%^\% as.kform(2) + 6*as.kform(5) \%^\% as.kform(6)

k1 <- kform_general(4,2,rnorm(6))
k2 <- kform_general(4,2,rnorm(6))

E <- matrix(rnorm(8),4,2)
as.function(k1+k2)(E)


as.function(2*k1+3*k2)(E)-(2*as.function(k1)(E) + 3*as.function(k1)(E))
## should be small
}
\keyword{symbolmath}
