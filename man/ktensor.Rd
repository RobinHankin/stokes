\name{ktensor}
\alias{ktensor}
\alias{as.ktensor}
\alias{as.function.ktensor}
\title{k-tensors}
\description{
Functionality for k-tensors
}
\usage{
ktensor(S)
as.ktensor(M,coeffs)
\method{as.function}{ktensor}(x,...)
}
\arguments{
  \item{M,coeffs}{Matrix of indices and coefficients, as in
    \code{spray(M,coeffs)}}
  \item{S}{Object of class spray}
  \item{x}{Object of class ktensor}
  \item{...}{Further arguments, currently ignored}
}
\details{

A \dfn{ktensor} object \eqn{S} is a map from \eqn{V^k} to the reals
\eqn{R}, where \eqn{V} is a vector space (here \eqn{R^n}) that satisfies
multilinearity:

\deqn{S\left(v_1,\ldots,av_i,\ldots,v_k\right)=a\cdot
  S\left(v_1,\ldots,v_i,\ldots,v_k\right)}{omitted; see PDF}

and

\deqn{S\left(v_1,\ldots,v_i+{v_i}',\ldots,v_k\right)=S\left(v_1,\ldots,v_i,\ldots,x_v\right)+
S\left(v_1,\ldots,{v_i}',\ldots,v_k\right).}{omitted; see PDF}
Note that this is \emph{not} equivalent to linearity
over \eqn{V^{nk}}{V^nk} (see examples).

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
\seealso{\code{\link{cross}},\code{\link{kform}}}
\examples{
k <- 4
n <- 5
no_of_terms <- 3
S  <- ktensor(spray(matrix(1+sample(no_of_terms*k)\%\%n,no_of_terms,k),0.5+seq_len(no_of_terms)))
S1 <- ktensor(spray(matrix(1+sample(no_of_terms*k)\%\%n,no_of_terms,k),0.5+seq_len(no_of_terms)))
E <- matrix(rnorm(n*k),n,k)   # random point in V^k

## Test multilinearity
E1 <- E2 <- E3 <- E

x1 <- rnorm(n)
x2 <- rnorm(n)
r1 <- rnorm(1)
r2 <- rnorm(1)

E1[,2] <- x1
E2[,2] <- x2
E3[,2] <- r1*x1 + r2*x2

f <- as.function(S)

r1*f(E1) + r2*f(E2) -f(E3) # should be small

## Note that multilinearity is different from linearity:
r1*f(E1) + r2*f(E2) - f(r1*E1 + r2*E2)  # not small!




}