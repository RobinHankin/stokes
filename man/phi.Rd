\name{phi}
\alias{phi}
\title{Elementary tensors}
\description{
Creates the elementary tensors or tensor products of elementary tensors
}
\usage{
phi(n)
}
\arguments{\item{n}{Vector of strictly non-negative integers}}

\details{

  If \eqn{v_1,\ldots,v_n}{omitted} is the standard basis for
  \eqn{\mathbb{R}^n}{omitted} then \eqn{\phi_i}{omitted} is defined so
  that \eqn{\phi_i(v_j)=\delta_{ij}}{omitted}.  \code{phi(n)} returns
  \eqn{\phi_n}{omitted}.

  If \code{n} is a vector of strictly positive integers, then
  \code{phi(n)} returns the tensor cross product of \eqn{\phi}{omitted}
  applied to the individual elements of \code{n} [which is a lot
  easier and more obvious than it sounds].
}

\author{Robin K. S. Hankin}

\note{There is a vignette, \code{phi}}

\examples{

phi(6)
phi(6:8)

v <- sample(9)
phi(v) == Reduce("\%X\%",sapply(v,phi))
}
