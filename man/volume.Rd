\name{volume}
\alias{volume}
\alias{is.volume}
\title{The volume element}
\description{\loadmathjax
  The volume element in \mjseqn{n} dimensions
}
\usage{
volume(n)
is.volume(K,n=dovs(K))
}
\arguments{
  \item{n}{Dimension of the space}
  \item{K}{Object of class \code{kform}}
}
\details{

Spivak phrases it well (theorem 4.6, page 82):

If \mjseqn{V} has dimension \mjseqn{n}, it follows that
\mjeqn{\Lambda^n(V)}{.} has dimension 1. Thus all alternating
\mjseqn{n}-tensors on \mjseqn{V} are multiples of any non-zero one.  Since the
determinant is an example of such a member of \mjeqn{\Lambda^n(V)}{.} it
is not surprising to find it in the following theorem:

Let \mjeqn{v_1,\ldots,v_n}{v_1,...,v_n} be a basis for \mjseqn{V} and let
\mjeqn{\omega\in\Lambda^n(V)}{.}.  If \mjeqn{w_i=\sum_{j=1}^n
a_{ij}v_j}{.}  then

\mjdeqn{
  \omega\left(w_1,\ldots,w_n\right)=\det\left(a_{ij}\right)\cdot\omega\left(v_1,\ldots
  v_n\right)}{omitted; see PDF}

(see the examples for numerical verification of this).

Neither the zero \mjseqn{k}-form, nor scalars, are considered to be a
volume element.

}
\value{
  Function \code{volume()} returns an object of class \code{kform};
  function \code{is.volume()} returns a Boolean.
}
\references{Spivak}
\author{Robin K. S. Hankin}
\seealso{
  \code{\link{zeroform}},\code{\link{as.1form}},\code{\link{dovs}}
}
\examples{


dx^dy^dz == volume(3) 

p <- 1
for(i in 1:7){p <- p ^ as.kform(i)}
p
p == volume(7)  # should be TRUE

o <- volume(5)
M <- matrix(runif(25),5,5)
det(M) - as.function(o)(M)   # should be zero


is.volume(d(1) ^ d(2) ^ d(3) ^ d(4))
is.volume(d(1:9))

}
