\name{kform}
\alias{kform}
\alias{as.kform}
\alias{kform_basis}
\alias{as.function.kform}
\alias{kform_general}
\alias{general_kform}
\title{k-forms}
\description{
Functionality for dealing with \eqn{k}-forms.
 }
\usage{
as.kform(M,coeffs)
kform_basis(n, k)
kform_general(W,k,coeffs)
\method{as.function}{kform}(x,...)
}
\arguments{
  \item{n}{Dimension of the vector space \eqn{V=R^n}}
  \item{k}{A \eqn{k}-form maps \eqn{V^k} to \eqn{R}}
  \item{W}{Integer vector of dimensions}
  \item{M}{Index matrix for a \eqn{k}-form}
  \item{coeffs}{Coefficients of the \eqn{k}-form}
  \item{x}{Object of class \code{kform}}
  \item{...}{Further arguments, currently ignored}
}
\details{

  A \dfn{\eqn{k}-form} is an alternating \eqn{k}-tensor.  In the
  \pkg{wedge} package, an object of class \code{kform} is a natural and
  efficient representation of an alternating \eqn{k}-tensor that allows
  one to use operations such as wedge products.

  Recall that a \eqn{k}-tensor is a multilinear map from \eqn{V^k} to
  the reals, where \eqn{V=R^n} is a vector space.  A multilinear
  \eqn{k}-tensor \eqn{T} is \dfn{alternating} if it satisfies

  \deqn{T\left(v_1,\ldots,v_i,\ldots,v_j,\ldots,v_k\right)=
    T\left(v_1,\ldots,v_j,\ldots,v_i,\ldots,v_k\right)
  }{omitted; see PDF}
  
Function \code{kform_basis()} is a low-level helper function that
returns a matrix whose rows constitute a basis for the vector space
\eqn{\Lambda^k(R^n)}{L^k(R^n)} of \eqn{k}-tensors:

\deqn{\phi=\sum_{1\leq i_1<\cdots<i_k\leq n}a_{i_1\ldots i_k}
  dx_{i_1}\wedge\cdots\wedge dx_{i_k}
}{omitted; see PDF}


and in fact

\deqn{a_{i_1\ldots i_k}=\phi\left(\mathbf{e}_{i_1},\ldots,\mathbf{e}_{i_k}\right)
}{omitted; see PDF}

where \eqn{\mathbf{e}_j,1\leq j\leq k}{e_j,1<=j<=k} is a basis for
\eqn{V}.

}
\references{Hubbard and Hubbard; Spivak}
\author{Robin K. S. Hankin}
\note{
  Hubbard and Hubbard use the term \dQuote{\eqn{k}-form}, but Spivak
  does not.
}
\seealso{\code{\link{ktensor}}}
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