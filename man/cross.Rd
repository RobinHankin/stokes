\name{cross}
\alias{cross}
\alias{cross2}
\alias{\%X\%}
\title{Cross products of \eqn{k}-tensors}
\description{Cross products of \eqn{k}-tensors}
\usage{
cross(U, ...)
cross2(U1,U2)
}
\arguments{
  \item{U,U1,U2}{Object of class \code{ktensor}}
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
\value{The functions documented here all return a \code{spray} object.}
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
U1 <- as.ktensor(M,rnorm(4))
U2 <- as.ktensor(t(M),1:2)

cross(U1, U2)
cross(U2, U1)  # not the same!

U1 \%X\% U2 - U2 \%X\% U1


}