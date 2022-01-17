\name{vector_cross_product}
\alias{vector_cross_product}
\title{The Vector cross product}
\description{\loadmathjax

  The vector cross product is defined in elementary school for pairs of
  vectors in \mjeqn{\mathcal{R}^3}{R^3} as

  \mjdeqn{
    \mathbf{u}\times\mathbf{v}=\left(u_2v_3-u_3v_2,u_2v_3-u_3v_2,u_2v_3-u_3v_2\right).
  }{
    u x v=(u_2 v_3 - u_3v_2, u_2 v_3 - u_3 v_2, u_2 v_3 - u_3 v_2).
  }

However, this may easily be generalized to a product from
\mjseqn{n-1}-tuples of vectors in \mjeqn{\mathcal{R}^3}{R^3}.  Vignette
\code{vector_cross_product} gives a discussion.

}
\usage{vector_cross_product(M)}
\arguments{
  \item{M}{Matrix with one more row than column; columns are interpreted
  as vectors}
}
\value{Returns a vector}
\details{
See vignette \code{vector_cross_product}
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
