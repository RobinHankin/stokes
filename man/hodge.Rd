\name{hodge}
\alias{hodge}
\alias{Hodge}
\alias{star}
\title{Hodge star operator}
\description{
  Given a \eqn{k}-form, return its Hodge dual}
\usage{
hodge(K, n=dovs(K), g, lose=TRUE)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{n}{Dimensionality of space, defaulting the the largest element
    of the index}
  \item{g}{Diagonal of the metric tensor, with missing default being the
    standard metric of the identity matrix.  Currently, only entries of
    \eqn{\pm 1}{+/-1} are accepted}
  \item{lose}{Boolean, with default \code{TRUE} meaning to coerce to a
    scalar if appropriate}
}
\value{Given a \eqn{k}-form, in an \eqn{n}-dimensional space,
  return a \eqn{(n-k)}-form.}
\author{Robin K. S. Hankin}
\note{Most authors write the Hodge dual of \eqn{\psi}{omitted} as
  \eqn{*\psi}{omitted} or \eqn{\star\psi}{omitted}, but Weintraub
  uses \eqn{\psi *}{omitted}.
}
\seealso{\code{\link{wedge}}}
\examples{


(o <- kform_general(5,2,1:10))
hodge(o)
o == hodge(hodge(o))

Faraday <- kform_general(4,2,runif(6)) # Faraday electromagnetic tensor
mink <- c(-1,1,1,1)  # Minkowski metric
hodge(Faraday,g=mink)

Faraday  == Faraday |>
      hodge(g=mink) |>
      hodge(g=mink) |>
      hodge(g=mink) |>
      hodge(g=mink)

hodge(dx,3) == dy^dz


## Some edge-cases:
hodge(scalar(1),2)
hodge(zeroform(5),9)
hodge(volume(5))
hodge(volume(5),lose=TRUE)
hodge(scalar(7),n=9)



}
