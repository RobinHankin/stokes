\name{ktensor}
\alias{ktensor}
\alias{as.ktensor}
\alias{is.ktensor}
\alias{is.tensor}
\alias{as.function.ktensor}
\title{k-tensors}
\description{
Functionality for \eqn{k}-tensors
}
\usage{
ktensor(S)
as.ktensor(M,coeffs)
is.ktensor(x)
\method{as.function}{ktensor}(x,...)
}
\arguments{
  \item{M,coeffs}{Matrix of indices and coefficients, as in
    \code{spray(M,coeffs)}}
  \item{S}{Object of class \code{spray}}
  \item{x}{Object of class \code{ktensor}}
  \item{...}{Further arguments, currently ignored}
}
\details{

A \dfn{\eqn{k}-tensor} object \eqn{S} is a map from \eqn{V^k}
to the reals \eqn{R}, where \eqn{V} is a vector space (here
\eqn{R^n}) that satisfies multilinearity:

\deqn{S\left(v_1,\ldots,av_i,\ldots,v_k\right)=a\cdot
  S\left(v_1,\ldots,v_i,\ldots,v_k\right)}{omitted; see latex}

and

\deqn{S\left(v_1,\ldots,v_i+{v_i}',\ldots,v_k\right)=S\left(v_1,\ldots,v_i,\ldots,x_v\right)+
  S\left(v_1,\ldots,{v_i}',\ldots,v_k\right).}{omitted; see latex}

Note that this is \emph{not} equivalent to linearity over
\eqn{V^{nk}}{V^nk} (see examples).

In the \CRANpkg{stokes} package, \eqn{k}-tensors are represented as
sparse arrays (\code{spray} objects), but with a class of
\code{c("ktensor", "spray")}.  This is a natural and efficient
representation for tensors that takes advantage of sparsity using
\CRANpkg{spray} package features.

Function \code{as.ktensor()} will coerce a \eqn{k}-form to a
\eqn{k}-tensor via \code{kform_to_ktensor()}.

}
\value{
  All functions documented here return a \code{ktensor} object
  except \code{as.function.ktensor()}, which returns a function.
}
\references{Spivak 1961}
\author{Robin K. S. Hankin}
\seealso{\code{\link{tensorprod}},\code{\link{kform}},\code{\link{wedge}}}
\examples{

as.ktensor(cbind(1:4,2:5,3:6),1:4)


## Test multilinearity:
k <- 4
n <- 5
u <- 3

## Define a randomish k-tensor:
S  <- ktensor(spray(matrix(1+sample(u*k)\%\%n,u,k),seq_len(u)))

## And a random point in V^k:
E <- matrix(rnorm(n*k),n,k)  

E1 <- E2 <- E3 <- E

x1 <- rnorm(n)
x2 <- rnorm(n)
r1 <- rnorm(1)
r2 <- rnorm(1)

# change one column:
E1[,2] <- x1
E2[,2] <- x2
E3[,2] <- r1*x1 + r2*x2

f <- as.function(S)

r1*f(E1) + r2*f(E2) -f(E3) # should be small

## Note that multilinearity is different from linearity:
r1*f(E1) + r2*f(E2) - f(r1*E1 + r2*E2)  # not small!


}
