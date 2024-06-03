\name{kinner}
\alias{kinner}
\title{Inner product of two kforms}
\description{

  Given two \eqn{k}-forms \eqn{\alpha}{a} and \eqn{\beta}{b},
  return the inner product
  \eqn{\left\langle\alpha,\beta\right\rangle}{<a,b>}.  Here our
  underlying vector space \eqn{V} is \eqn{\mathcal{R}^n}{R^n}.

  The inner product is a symmetric bilinear form defined in two stages.
  First, we specify its behaviour on decomposable \eqn{k}-forms
  \eqn{\alpha=\alpha_1\wedge\cdots\wedge\alpha_k}{omitted} and
  \eqn{\beta=\beta_1\wedge\cdots\wedge\beta_k}{omitted} as

  \deqn{
    \left\langle\alpha,\beta\right\rangle=\det\left(
    \left\langle\alpha_i,\beta_j\right\rangle_{1\leq i,j\leq n}\right)
  }{ omitted}

and secondly, we extend to the whole of \eqn{\Lambda^k(V)}{omitted}
through linearity.
}

\usage{
kinner(o1,o2,M)
}
\arguments{
  \item{o1,o2}{Objects of class \code{kform}}
\item{M}{Matrix}
}
\value{Returns a real number}
\author{Robin K. S. Hankin}
\note{There is a vignette available: type \code{vignette("kinner")} at
  the command line.
}
\seealso{\code{\link{hodge}}}
\examples{

a <- (2*dx)^(3*dy)
b <- (5*dx)^(7*dy)

kinner(a,b)
det(matrix(c(2*5,0,0,3*7),2,2))  # mathematically identical, slight numerical mismatch


}
