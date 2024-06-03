\name{inner}
\alias{inner}
\alias{inner_product}
\alias{inner.product}
\title{Inner product operator}
\description{
  The inner product
}
\usage{inner(M)}
\arguments{
  \item{M}{square matrix}
}
\value{Returns a \eqn{k}-tensor, an inner product}
\details{

The inner product of two vectors \eqn{\mathbf{x}}{x} and
\eqn{\mathbf{y}}{y} is usually written
\eqn{\left\langle\mathbf{x},\mathbf{y}\right\rangle}{<x,y>} or
\eqn{\mathbf{x}\cdot\mathbf{y}}{x.y}, but the most general form would
be \eqn{\mathbf{x}^TM\mathbf{y}}{x'My} where \eqn{M}{M} is a matrix.
Noting that inner products are multilinear, that is
\eqn{\left\langle\mathbf{x},a\mathbf{y}+b\mathbf{z}\right\rangle=a\left\langle\mathbf{x},\mathbf{y}\right\rangle+b\left\langle\mathbf{x},\mathbf{z}\right\rangle}{<x,ay+bz>=a<x,y>+b<y,z>}
and \eqn{\left\langle
a\mathbf{x}+b\mathbf{y},\mathbf{z}\right\rangle=a\left\langle\mathbf{x},\mathbf{z}\right\rangle+b\left\langle\mathbf{y},\mathbf{z}\right\rangle}{<x,ay+bz>=a<x,y>+b<y,z>},
we see that the inner product is indeed a multilinear map, that is, a
tensor.

Given a square matrix \eqn{M}, function \code{inner(M)} returns the
\eqn{2}-form that maps \eqn{\mathbf{x},\mathbf{y}}{x,y} to
\eqn{\mathbf{x}^TM\mathbf{y}}{x'My}.  Non-square matrices are
effectively padded with zeros.

A short vignette is provided with the package: type
\code{vignette("inner")} at the commandline.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{kform}}}
\examples{

inner(diag(7))
inner(matrix(1:9,3,3))

## Compare the following two:
Alt(inner(matrix(1:9,3,3)))      # An alternating k tensor
as.kform(inner(matrix(1:9,3,3))) # Same thing coerced to a kform

f <- as.function(inner(diag(7)))
X <- matrix(rnorm(14),ncol=2)  # random element of (R^7)^2
f(X) - sum(X[,1]*X[,2]) # zero to numerical precision

## verify positive-definiteness:
g <- as.function(inner(crossprod(matrix(rnorm(56),8,7))))
stopifnot(g(kronecker(rnorm(7),t(c(1,1))))>0)

}
