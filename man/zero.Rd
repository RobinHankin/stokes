\name{zero}
\alias{zero}
\alias{zeroform}
\alias{zerotensor}
\alias{is.zero}
\alias{is.empty}
\title{Zero tensors and zero forms}
\description{
Correct idiom for generating zero \eqn{k}-tensors and \eqn{k}-forms
}
\usage{
zeroform(n)
zerotensor(n)
is.zero(x)
is.empty(x)
}
\arguments{
  \item{n}{Arity of the \eqn{k}-form or \eqn{k}-tensor}
  \item{x}{Object to be tested for zero}
}
\author{Robin K. S. Hankin}
\note{
  Idiom such as \code{as.ktensor(rep(1,n),0)} and
  \code{as.kform(rep(1,5),0)} and indeed \code{as.kform(1:5,0)} is
  incorrect as the arity of the tensor is lost.

  A \eqn{0}-form is not the same thing as a zero tensor.  A
  \eqn{0}-form maps \eqn{V^0} to the reals; a scalar.  A zero
  tensor maps \eqn{V^k} to zero.  Some discussion is given at
  \code{scalar.Rd}.
}
\value{Returns an object of class \code{kform} or \code{ktensor}.}
\seealso{\code{\link{scalar}}}
\examples{

zerotensor(5)
zeroform(3)


x <- rform(k=3)
x*0 == zeroform(3)       # should be true
x   == x + zeroform(3)  # should be true

y <- rtensor(k=3)
y*0 == zerotensor(3)    # should be true
y   == y+zerotensor(3)  # should be true


## Following idiom is plausible but fails because as.ktensor(coeffs=0)
## and as.kform(coeffs=0) do not retain arity:

## as.ktensor(1+diag(5)) +  as.ktensor(rep(1,5),0)   # fails
## as.kform(matrix(1:6,2,3)) + as.kform(1:3,0)   # also fails


}
