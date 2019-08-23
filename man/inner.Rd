\name{inner}
\alias{inner}
\alias{inner_product}
\alias{inner.product}
\title{Inner product operator}
\description{The inner product}
\usage{inner(M)}
\arguments{
  \item{M}{square matrix}
}
\value{Returns a \eqn{k}-tensor, an inner product}
\details{

The inner product of two vectors \eqn{\bold x}{x} and \eqn{\bold y}{y}
is usually written \eqn{\left<\bold x,\bold y\right>}{<x,y>} or
\eqn{\bold x\cdot\bold y}{x.y}, but the most general form would be
\eqn{\bold x^TM\bold y}{x'My} where \eqn{M} is a positive-definite
matrix.  Noting that inner products are symmetric, that is
\eqn{\left<\bold x,\bold y\right>=\left<\bold x,\bold
y\right>}{<x,y>=<y,x>} (we are considering the real case only), and
multilinear, that is \eqn{\left<\bold x,a\bold y+b\bold
z\right>=a\left<\bold x,\bold y\right>+b\left<\bold x,\bold
z\right>}{<x,ay+bz>=a<x,y>+b<x,z>}, we see that the inner product is
indeed a multilinear map, that is, a tensor.

Function \code{inner(m)} returns the \eqn{2}-form that maps \eqn{\bold
  x,\bold y}{x,y} to \eqn{\bold x^TM\bold y}{x'My}.

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
