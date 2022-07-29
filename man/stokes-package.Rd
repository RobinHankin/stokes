\name{stokes-package}
\alias{stokes-package}
\alias{stokes}
\docType{package}
\title{
\packageTitle{stokes}
}
\description{\loadmathjax
\packageDescription{stokes}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{stokes}
\packageIndices{stokes}

Generally in the package, arguments that are \mjseqn{k}-forms are
denoted \code{K}, \mjseqn{k}-tensors by \code{U}, and spray objects by
\code{S}.  Multilinear maps (which may be either \mjseqn{k}-forms or
\mjseqn{k}-tensors) are denoted by \code{M}.

}
\author{
\packageAuthor{stokes}

Maintainer: \packageMaintainer{stokes}
}
\references{
  \itemize{
    \item J. H. Hubbard and B. B. Hubbard 2015.  \emph{Vector
      calculus, linear algebra and differential forms: a unified
      approach}.  Ithaca, NY.
    \item M. Spivak 1971. \emph{Calculus on manifolds},
    Addison-Wesley.
  }
}
\keyword{ package }
\seealso{\code{\link[spray:spray-package]{spray}}
}
\examples{
## Some k-tensors:
U1 <- as.ktensor(matrix(1:15,5,3))
U2 <- as.ktensor(cbind(1:3,2:4),1:3)

## Coerce a tensor to functional form, here mapping V^3  -> R (here V=R^15):
as.function(U1)(matrix(rnorm(45),15,3))

## Tensor product is tensorprod() or \%X\%:
U1 \%X\% U2


## A k-form is an alternating k-tensor:
K1 <- as.kform(cbind(1:5,2:6),rnorm(5))
K2 <- kform_general(3:6,2,1:6)
K3 <- rform(9,3,9,runif(9))

## The distributive law is true

(K1 + K2) ^ K3 == K1 ^ K3 + K2 ^ K3 # TRUE to numerical precision

## Wedge product is associative (non-trivial):
(K1 ^ K2) ^ K3
K1 ^ (K2 ^ K3)


## k-forms can be coerced to a function and wedge product:
f <- as.function(K1 ^ K2 ^ K3)

## E is a a random point in V^k:
E <- matrix(rnorm(63),9,7)

## f() is alternating:
f(E)
f(E[,7:1])



## The package blurs the distinction between symbolic and numeric computing:
dx <- as.kform(1)
dy <- as.kform(2)
dz <- as.kform(3)

dx ^ dy ^ dz

K3 ^ dx ^ dy ^ dz
}
