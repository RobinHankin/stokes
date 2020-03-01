\name{zero}
\alias{zeroform}
\alias{zerotensor}
\title{Zero tensors and zero forms}
\description{
Correct idiom for generating zero \eqn{k}-tensors and \eqn{k}-forms
}
\usage{
zeroform(n)
zerotensor(n)
}
\arguments{
  \item{n}{Arity of the \eqn{k}-form or \eqn{k}-tensor}
}
\author{Robin K. S. Hankin}
\note{
  Idiom such as \code{as.ktensor(rep(1,n),0)} and
  \code{as.kform(rep(1,5),0)} and indeed \code{as.kform(1:5,0)} is
  incorrect as the arity of the tensor is lost.

  A \eqn{0}-form is not the same thing as a zero tensor.  A \eqn{0}-form
  maps \eqn{V^0} to the reals; a scalar.  A zero tensor maps \eqn{V^k} to
  zero.
}
\value{Returns an object of class \code{kform} or \code{ktensor}.}
\seealso{\code{\link{scalar}}}
\examples{

as.ktensor(1+diag(5)) + zerotensor(5)
as.kform(matrix(1:6,2,3)) + zeroform(3)

## Following idiom is plausible but fails because as.ktensor(coeffs=0)
## and as.kform(coeffs=0) do not retain arity:

\dontrun{ 
as.ktensor(1+diag(5)) +  as.ktensor(rep(1,5),0)   # fails
as.kform(matrix(1:6,2,3)) + as.kform(1:3,0)   # also fails
}

}