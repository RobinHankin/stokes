\name{kform}
\alias{kform}
\alias{as.kform}
\alias{is.kform}
\alias{is.form}
\alias{kform_basis}
\alias{as.function.kform}
\alias{kform_general}
\alias{general_kform}
\title{k-forms}
\description{\loadmathjax
  Functionality for dealing with \eqn{k}-forms
}
\usage{
kform(S)
as.kform(M,coeffs,lose=TRUE)
kform_basis(n, k)
kform_general(W,k,coeffs,lose=TRUE)
is.kform(x)
\method{as.function}{kform}(x,...)
}
\arguments{
  \item{n}{Dimension of the vector space \mjeqn{V=R^n}{V=R^n}}
  \item{k}{A \eqn{k}-form maps \mjeqn{V^k}{V^k} to \eqn{R}}
  \item{W}{Integer vector of dimensions}
  \item{M,coeffs}{Index matrix and coefficients for a \eqn{k}-form}
  \item{S}{Object of class \code{spray}}
  \item{lose}{Boolean, with default \code{TRUE} meaning to coerce a
    \eqn{0}-form to a scalar and \code{FALSE} meaning to return the
    formal \eqn{0}-form}
  \item{x}{Object of class \code{kform}}
  \item{...}{Further arguments, currently ignored}
}
\details{

  A \dfn{\eqn{k}-form} is an alternating \eqn{k}-tensor.  

  Recall that a \eqn{k}-tensor is a multilinear map from \eqn{V^k} to
  the reals, where \eqn{V=R^n} is a vector space.  A multilinear
  \eqn{k}-tensor \eqn{T} is \dfn{alternating} if it satisfies

  \mjdeqn{T\left(v_1,\ldots,v_i,\ldots,v_j,\ldots,v_k\right)=
    T\left(v_1,\ldots,v_j,\ldots,v_i,\ldots,v_k\right)
  }{omitted; see latex}
  
  Function \code{kform_basis()} is a low-level helper function that
  returns a matrix whose rows constitute a basis for the vector space
  \mjeqn{\Lambda^k(R^n)}{L^k(R^n)} of \eqn{k}-tensors:
  
  \mjdeqn{\phi=\sum_{1\leq i_1 < \cdots < i_k\leq n} a_{i_1\ldots i_k}dx_{i_1}\wedge\cdots\wedge dx_{i_k}}{omitted; see latex}

  and indeed we have:

  \mjdeqn{a_{i_1\ldots i_k}=\phi\left(\mathbf{e}_{i_1},\ldots,\mathbf{e}_{i_k}\right)
  }{omitted; see latex}
  
  where \mjeqn{\mathbf{e}_j,1\leq j\leq k}{e_j,1<=j<=k} is a basis for
  \eqn{V}.
  
  In the \pkg{stokes} package, \eqn{k}-forms are represented as sparse
  arrays (\code{spray} objects), but with a class of \code{c("kform",
    "spray")}.  The constructor function (\code{kform()}) ensures that rows
  of the index matrix are strictly nonnegative integers, have no repeated
  entries, and are strictly increasing.
  
}
\value{
  All functions documented here return a \code{kform} object except
  \code{as.function.kform()}, which returns a function.
}
\references{Hubbard and Hubbard; Spivak}
\author{Robin K. S. Hankin}
\note{
  Hubbard and Hubbard use the term \dQuote{\eqn{k}-form}, but Spivak
  does not.
}
\seealso{\code{\link{ktensor}},\code{\link{lose}}}
\examples{

as.kform(cbind(1:5,2:6),rnorm(5))
kform_general(1:4,2,coeffs=1:6)  # used in electromagnetism

K1 <- as.kform(cbind(1:5,2:6),rnorm(5))
K2 <- kform_general(5:8,2,1:6)
wedge(K1,K2)


f <- as.function(wedge(K1,K2))
E <- matrix(rnorm(32),8,4)

f(E) + f(E[,c(1,3,2,4)])  # should be zero


}
