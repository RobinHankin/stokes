\name{volume}
\alias{volume}
\alias{is.volume}
\title{The volume element}
\description{The volume element in \eqn{n} dimensions}
\usage{
volume(n)
is.volume(K)
}
\arguments{
  \item{n}{Dimension of the space}
  \item{K}{Object of class \code{kform}}
}
\details{

Spivak phrases it well (theorem 4.6, page 82):

If \eqn{V} has dimension \eqn{n}, it follows that \eqn{\Lambda^n(V)}{.}
has dimension 1. Thus all alternating \eqn{n}-tensors on \eqn{V} are
multiples of any non-zero one.  Since the determinant is an example of
such a member of \eqn{\Lambda^n(V)}{.} it is not surprising to find it
in the following theorem:

Let \eqn{v_1,\ldots,v_n}{.} be a basis for \eqn{V} and let
\eqn{\omega\in\Lambda^n(V)}{.}.  If \eqn{w_i=\sum_{j=1}^n a_{ij}v_j}{.}
then

\deqn{
  \omega\left(w_1,\ldots,w_n\right)=\det\left(a_{ij}\right)\cdot\omega\left(v_1,\ldots
  v_n\right)}{ommitted; see PDF}


(see the examples for numerical verification of this).

Neither the zero \eqn{k}-form, nor scalars, are considered to be a
volume element.

}
\value{
  Function \code{volume()} returns an object of class \code{kform};
  function \code{is.volume()} returns a Boolean.
}
\references{Spivak}
\author{Robin K. S. Hankin}
\seealso{
\code{\link{zeroform}},\code{\link{as.1form}}}
\examples{

as.kform(1) \%^\% as.kform(2) \%^\% as.kform(3)  == volume(3)  # should be TRUE

o <- volume(5)
M <- matrix(runif(25),5,5)
det(M) - as.function(o)(M)   # should be zero


}
