\name{vector_cross_product}
\alias{vector_cross_product}
\alias{vcp3}
\title{The Vector cross product}
\description{

  The vector cross product \eqn{\mathbf{u}\times\mathbf{v}}{omitted}
  for \eqn{\mathbf{u},\mathbf{u}\in\mathbb{R}^3}{omitted} is defined
  in elementary school as

  \deqn{
    \mathbf{u}\times\mathbf{v}=\left(u_2v_3-u_3v_2,u_2v_3-u_3v_2,u_2v_3-u_3v_2\right).
  }{
    u x v = (u_2 v_3 - u_3v_2, u_2 v_3 - u_3 v_2, u_2 v_3 - u_3 v_2).
  }

Function \code{vcp3()} is a convenience wrapper for this.  However, the
vector cross product may easily be generalized to a product of
\eqn{n-1}-tuples of vectors in \eqn{\mathbb{R}^n}{R^n}, given by
package function \code{vector_cross_product()}.

Vignette \code{vector_cross_product}, supplied with the package, gives
an extensive discussion of vector cross products, including formal
definitions and verification of identities.

}
\usage{
vector_cross_product(M)
vcp3(u,v)
}
\arguments{
  \item{M}{Matrix with one more row than column; columns are interpreted
  as vectors}
\item{u,v}{Vectors of length 3, representing vectors in \eqn{\mathbb{R}^3}{R^3}}
}
\value{Returns a vector}
\details{

A joint function profile for \code{vector_cross_product()} and
\code{vcp3()} is given with the package at
\code{vignette("vector_cross_product")}.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{cross}}}
\examples{

vector_cross_product(matrix(1:6,3,2))


M <- matrix(rnorm(30),6,5)
LHS <- hodge(as.1form(M[,1])^as.1form(M[,2])^as.1form(M[,3])^as.1form(M[,4])^as.1form(M[,5]))
RHS <- as.1form(vector_cross_product(M))
LHS-RHS  # zero to numerical precision

# Alternatively:
hodge(Reduce(`^`,sapply(seq_len(5),function(i){as.1form(M[,i])},simplify=FALSE)))
}
