\name{wedge-package}
\alias{wedge-package}
\docType{package}
\title{
\packageTitle{wedge}
}
\description{
\packageDescription{wedge}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{wedge}
\packageIndices{wedge}
}
\author{
\packageAuthor{wedge}

Maintainer: \packageMaintainer{wedge}
}
\references{
  \itemize{
    \item J. H. Hubbard and B. B. Hubbard 2015.  \emph{Vector
      calculus, linear algebra and differential forms: a unified
      aproach}.  Ithaca, NY.
    \item M. Spivak 1971. \emph{Calculus on manifolds}.
    Addison-Wesley.
  }
}
\keyword{ package }
\seealso{\code{\link[spray:spray-package]{spray}}
}
\examples{
## Some k-tensors:
T1 <- as.ktensor(matrix(1:15,5,3))
T2 <- as.ktensor(cbind(1:3,2:4),1:3)

## Coerce a tensor to functional form, here mapping V^3  -> R (here V=R^15):
as.function(T1)(matrix(rnorm(45),15,3))

## Tensor cross-product is cross() or \%X\%:
T1 \%X\% T2


## A k-form is an alternating k-tensor:
K1 <- as.kform(cbind(1:5,2:6),rnorm(5))
K2 <- kform_general(5:8,2,1:6)
K3 <- rform(9,3,9,runif(9))

## Wedge product is associative (non-trivial):
(K1 \%^\% K2) \%^\% K3
K1 \%^\% (K2 \%^\% K3)


## k-forms can be coerced to a function and wedge product:
f <- as.function(K1 \%^\% K2 \%^\% K3)

## E is a a random point in V^k:
E <- matrix(rnorm(63),9,7)

## f() is alternating:
f(E)
f(E[,7:1])



## The package blurs the distinction between symbolic and numeric computing:
dx <- as.kform(1)
dy <- as.kform(2)
dz <- as.kform(3)

dx \%^\% dy \%^\% dz

K3 \%^\% dx \%^\% dy \%^\% dz
}
