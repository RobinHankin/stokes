\name{inner}
\alias{inner}
\alias{inner_product}
\alias{inner.product}
\title{Inner product operator}
\description{The inner product}
\usage{
inner(M)
}
\arguments{
  \item{M}{square matrix}
}
\value{Returns a \eqn{k}-tensor, an inner product}
\author{Robin K. S. Hankin}
\seealso{\code{\link{kform}}}
\examples{

inner(diag(7))

f <- as.function(inner(diag(7)))
X <- matrix(rnorm(14),ncol=2)  # random element of (R^7)^2
f(X) - sum(X[,1]*X[,2]) # should be zero

## verify positive-definiteness:
g <- as.function(inner(crossprod(matrix(rnorm(56),8,7))))
stopifnot(g(kronecker(rnorm(7),t(c(1,1))))>0)

}
