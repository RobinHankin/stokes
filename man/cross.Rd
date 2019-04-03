\name{cross}
\alias{cross}
\alias{cross2}
\alias{\%X\%}
\title{Cross products of k-tensors}
\description{Cross products of k-tensors}
\usage{
cross(x,...)
cross2(S1,S2)
}
\arguments{
  \item{S1,S2,x}{Object of class ktensor}
  \item{...}{Further arguments, currently ignored}
}
\details{

Given two k-tensor objects \eqn{S,T} we can form the cross product
\eqn{S\otimes T}{S*T}, defined as

\deqn{S\otimes T\left(v_1,\ldots,v_k,v_{k+1},\ldots, v_{k+l}\right)=
  S\left(v_1,\ldots v_k\right)\cdot T\left(v_{k+1},\ldots
  v_{k+l}\right)}{omitted; see PDF}

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
k <- 4
n <- 5
no_of_terms <- 3
S  <- ktensor(spray(matrix(1+sample(no_of_terms*k)\%\%n,no_of_terms,k),0.5+seq_len(no_of_terms)))
S1 <- ktensor(spray(matrix(1+sample(no_of_terms*k)\%\%n,no_of_terms,k),0.5+seq_len(no_of_terms)))

cross(S,S1)



}