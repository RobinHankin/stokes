\name{inner}
\alias{inner}
\alias{inner_product}
\alias{inner.product}
\title{Inner product operator}
\description{\loadmathjax
  The inner product
}
\usage{inner(M)}
\arguments{
  \item{M}{square matrix}
}
\value{Returns a \eqn{k}-tensor, an inner product}
\details{

The inner product of two vectors \mjeqn{\mathbf{x}}{x} and
\mjeqn{\mathbf{y}}{y} is usually written
\mjeqn{\left<\mathbf{x},\mathbf{y}\right>}{<x,y>} or
\mjeqn{\mathbf{x}\cdot\mathbf{y}}{x.y}, but the most general form would
be \mjeqn{\mathbf{x}^TM\mathbf{y}}{x'My} where \mjeqn{M}{M} is a
positive-definite matrix.  Noting that inner products are symmetric,
that is
\mjeqn{\left<\mathbf{x},\mathbf{y}\right>=\left<\mathbf{y},\mathbf{x}\right>}{<x,y>=<y,x>}
(we are considering the real case only), and multilinear, that is
\mjeqn{\left<\mathbf{x},a\mathbf{y}+b\mathbf{z}\right>=a\left<\mathbf{x},\mathbf{y}\right>+b\left<\mathbf{x},\mathbf{z}\right>}{<x,ay+bz>=a<x,y>+b<x,z>},
we see that the inner product is indeed a multilinear map, that is, a
tensor.

Given a square matrix \code{m} with entries \mjeqn{M_{ij}}{M_ij},
function \code{inner(m)} returns the \eqn{2}-form that maps
\mjeqn{\mathbf{x},\mathbf{y}}{x,y} to
\mjeqn{\mathbf{x}^TM\mathbf{y}}{x'My}.

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
