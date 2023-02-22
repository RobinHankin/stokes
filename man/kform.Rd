\name{kform}
\alias{kform}
\alias{as.kform}
\alias{is.kform}
\alias{is.form}
\alias{kform_basis}
\alias{as.function.kform}
\alias{kform_general}
\alias{general_kform}
\alias{d}
\alias{e}
\title{k-forms}
\description{\loadmathjax
  Functionality for dealing with \mjseqn{k}-forms
}
\usage{
kform(S)
as.kform(M,coeffs,lose=TRUE)
kform_basis(n, k)
kform_general(W,k,coeffs,lose=TRUE)
is.kform(x)
d(i)
e(i,n)
\method{as.function}{kform}(x,...)
}
\arguments{
  \item{n}{Dimension of the vector space \mjeqn{V=R^n}{V=R^n}}
  \item{i}{Integer}
  \item{k}{A \mjseqn{k}-form maps \mjeqn{V^k}{V^k} to \mjseqn{R}}
  \item{W}{Integer vector of dimensions}
  \item{M,coeffs}{Index matrix and coefficients for a \mjseqn{k}-form}
  \item{S}{Object of class \code{spray}}
  \item{lose}{Boolean, with default \code{TRUE} meaning to coerce a
    \mjseqn{0}-form to a scalar and \code{FALSE} meaning to return the
    formal \mjseqn{0}-form}
  \item{x}{Object of class \code{kform}}
  \item{...}{Further arguments, currently ignored}
}
\details{

  A \dfn{\mjseqn{k}-form} is an alternating \mjseqn{k}-tensor.  In the
  package, \mjseqn{k}-forms are represented as sparse arrays
  (\code{spray} objects), but with a class of \code{c("kform",
  "spray")}.  The constructor function \code{kform()} takes a
  \code{spray} object and returns a \code{kform} object: it ensures that
  rows of the index matrix are strictly nonnegative integers, have no
  repeated entries, and are strictly increasing.  Function
  \code{as.kform()} is more user-friendly.

  \itemize{
    \item\code{kform()} is the constructor function.  It takes a
    \code{spray} object and returns a \code{kform}.
    \item\code{as.kform()} also returns a \code{kform} but is a bit more
    user-friendly  than \code{kform()}.
    \item\code{kform_basis()} is a low-level helper function that 
    returns a matrix whose rows constitute a basis for the vector space
    \mjeqn{\Lambda^k(R^n)}{L^k(R^n)} of \mjseqn{k}-forms.
    \item\code{kform_general()} returns a \code{kform} object with terms
    that span the space of alternating tensors.
    \item\code{is.kform()} returns \code{TRUE} if its argument is a
    \code{kform} object.
    \item{\code{d()}} is an easily-typed synonym for
    \code{as.kform()}.  The idea is that \code{d(1) = dx},
    \code{d(2)=dy}, \code{d(5)=dx^5}, etc.  Also note that, for
    example, \code{d(1:3)=dx^dy^dz}, the volume form.
  }
  
  Recall that a \mjseqn{k}-tensor is a multilinear map from \mjseqn{V^k} to
  the reals, where \mjseqn{V=R^n} is a vector space.  A multilinear
  \mjseqn{k}-tensor \mjseqn{T} is \dfn{alternating} if it satisfies

  \mjdeqn{T\left(v_1,\ldots,v_i,\ldots,v_j,\ldots,v_k\right)=
    -T\left(v_1,\ldots,v_j,\ldots,v_i,\ldots,v_k\right)
  }{omitted; see latex}
  
  In the package, an object of class \code{kform} is an efficient
  representation of an alternating tensor.
  
  Function \code{kform_basis()} is a low-level helper function that
  returns a matrix whose rows constitute a basis for the vector space
  \mjeqn{\Lambda^k(R^n)}{L^k(R^n)} of \mjseqn{k}-forms:
  
  \mjdeqn{\phi=\sum_{1\leq i_1 < \cdots < i_k\leq n} a_{i_1\ldots i_k}dx_{i_1}\wedge\cdots\wedge dx_{i_k}}{omitted; see latex}

  and indeed we have:

  \mjdeqn{a_{i_1\ldots i_k}=\phi\left(\mathbf{e}_{i_1},\ldots,\mathbf{e}_{i_k}\right)
  }{omitted; see latex}
  
  where \mjeqn{\mathbf{e}_j,1\leq j\leq k}{e_j,1<=j<=k} is a basis for
  \mjseqn{V}.
  
}
\value{
  All functions documented here return a \code{kform} object except
  \code{as.function.kform()}, which returns a function, and
  \code{is.kform()}, which returns a Boolean, and \code{e()}, which
  returns a conjugate basis to that of \code{d()}.
}
\references{Hubbard and Hubbard; Spivak}
\author{Robin K. S. Hankin}
\note{
  Hubbard and Hubbard use the term \dQuote{\mjseqn{k}-form}, but Spivak
  does not.
}
\seealso{\code{\link{ktensor}},\code{\link{lose}}}
\examples{

as.kform(cbind(1:5,2:6),rnorm(5))
kform_general(1:4,2,coeffs=1:6)  # used in electromagnetism

K1 <- as.kform(cbind(1:5,2:6),rnorm(5))
K2 <- kform_general(5:8,2,1:6)
K1^K2  # or wedge(K1,K2)

d(1:3)
dx^dy^dz   # same thing

d(sample(9)) # coeff is +/-1 depending on even/odd permutation of 1:9

f <- as.function(wedge(K1,K2))
E <- matrix(rnorm(32),8,4)
f(E) + f(E[,c(1,3,2,4)])  # should be zero by alternating property

options(kform_symbolic_print = 'd')
(d(5)+d(7)) ^ (d(2)^d(5) + 6*d(4)^d(7))
options(kform_symbolic_print = NULL)  # revert to default

}
