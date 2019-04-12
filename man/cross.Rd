\name{cross}
\alias{cross}
\alias{cross2}
\alias{\%X\%}
\title{Cross products of \eqn{k}-tensors}
\description{Cross products of \eqn{k}-tensors}
\usage{
cross(x,...)
cross2(S1,S2)
}
\arguments{
  \item{S1,S2,x}{Object of class \code{ktensor}}
  \item{...}{Further arguments, currently ignored}
}
\details{

Given a \eqn{k}-tensor object \eqn{S} and an \eqn{l}-tensor \eqn{T}, we
can form the cross product \eqn{S\otimes T}{S \%X\% T}, defined as

\deqn{S\otimes T\left(v_1,\ldots,v_k,v_{k+1},\ldots, v_{k+l}\right)=
  S\left(v_1,\ldots v_k\right)\cdot T\left(v_{k+1},\ldots
  v_{k+l}\right).}{omitted; see PDF}

Package idiom for this includes \code{cross(S,T)} and \code{S \%X\% T};
note that the cross product is not commutative.  Function \code{cross()}
can take any number of arguments (the result is well-defined because the
cross product is associative); it uses \code{cross2()} as a low-level
helper function.
}

\references{Spivak 1961}
\note{
The binary form \code{\%X\%} uses uppercase X to avoid clashing with
\code{\%x\%} which is the Kronecker product in base \R.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ktensor}}
}
\examples{
M <- cbind(1:4,2:5)
S1 <- as.ktensor(M,rnorm(4))
S2 <- as.ktensor(t(M),1:2)

cross(S1, S2)
cross(S2, S1)

S1 \%X\% S2 - S2 \%X\% S1


}