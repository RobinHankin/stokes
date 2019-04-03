\name{Ops.kform}
\alias{Ops.kform}
\alias{Ops}
\title{Arithmetic Ops Group Methods for kform and ktensor objects}
\description{
  Allows arithmetic operators to be used for kforms
  such as addition, multiplication, etc, where defined.
}
\usage{
\method{Ops}{kform}(e1, e2 = NULL)
}
\arguments{
  \item{e1,e2}{Objects of class \dQuote{\code{kform}}}
}
\details{
  The function \code{Ops.kform()} passes unary and binary arithmetic
  operators (\dQuote{\code{+}}, \dQuote{\code{-}}, \dQuote{\code{*}}, and
  \dQuote{\code{/}}) to the appropriate specialist function.

  For wedge products of kforms, use \code{wedge()} or \code{\%^\%}; and
  for cross products of ktensors, use \code{cross()} or \code{\%X\%}.

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
