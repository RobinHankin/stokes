\name{tensorprod}
\alias{tensorprod}
\alias{tensorprod2}
\alias{\%X\%}
\title{Tensor products of \eqn{k}-tensors}
\description{
 Tensor products of \eqn{k}-tensors}
\usage{
tensorprod(U, ...)
tensorprod2(U1,U2)
}
\arguments{
  \item{U,U1,U2}{Object of class \code{ktensor}}
  \item{...}{Further arguments, currently ignored}
}
\details{

Given a \eqn{k}-tensor \eqn{S} and an \eqn{l}-tensor
\eqn{T}, we can form the tensor product \eqn{S\otimes T}{S \%X\%
T}, defined as

\deqn{S\otimes T\left(v_1,\ldots,v_k,v_{k+1},\ldots, v_{k+l}\right)=
  S\left(v_1,\ldots v_k\right)\cdot T\left(v_{k+1},\ldots
  v_{k+l}\right).}{omitted; see latex}

Package idiom for this includes \code{tensorprod(S,T)} and \code{S \%X\%
T}; note that the tensor product is not commutative.  Function
\code{tensorprod()} can take any number of arguments (the result is
well-defined because the tensor product is associative); it uses
\code{tensorprod2()} as a low-level helper function.

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

(A <- ktensor(spray(matrix(c(1,1,2,2,3,3),2,3,byrow=TRUE),1:2)))
(B <- ktensor(spray(10+matrix(4:9,3,2),5:7)))
tensorprod(A,B)

A \%X\% B - B \%X\% A


Va <- matrix(rnorm(9),3,3)
Vb <- matrix(rnorm(38),19,2)

LHS <- as.function(A \%X\% B)(cbind(rbind(Va,matrix(0,19-3,3)),Vb))
RHS <-  as.function(A)(Va) * as.function(B)(Vb)

c(LHS=LHS,RHS=RHS,diff=LHS-RHS)



}
