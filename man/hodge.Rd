\name{hodge}
\alias{hodge}
\alias{Hodge}
\alias{star}
\title{Hodge star operator}
\description{\loadmathjax
  Given a \mjseqn{k}-form, return its Hodge dual}
\usage{
hodge(K, n=dovs(K), g, lose=TRUE)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{n}{Dimensionality of space, defaulting the the largest element
    of the index}
  \item{g}{Diagonal of the metric tensor, with missing default being the
    standard metric of the identity matrix.  Currently, only entries of
    \mjeqn{\pm 1}{+/-1} are accepted}
  \item{lose}{Boolean, with default \code{TRUE} meaning to coerce to a
    scalar if appropriate}
}
\value{Given a \mjseqn{k}-form, in an \mjseqn{n}-dimensional space,
  return a \mjseqn{(n-k)}-form.}
\author{Robin K. S. Hankin}
\note{Most authors write the Hodge dual of \mjeqn{\psi}{omitted} as
  \mjeqn{*\psi}{omitted} or \mjeqn{\star\psi}{omitted}, but Weintraub
  uses \mjeqn{\psi *}{omitted}.
}
\seealso{\code{\link{wedge}}}
\examples{


(o <- kform_general(5,2,1:10))
hodge(o)

Faraday <- kform_general(4,2,runif(6)) # Faraday electromagnetic tensor
minsk <- c(-1,1,1,1)  # Minkowski metric
hodge(Faraday,g=minsk)
Faraday==Faraday|>hodge(g=minsk)|>hodge(g=minsk)|>hodge(g=minsk)|>hodge(g=minsk)

hodge(dx,3) == dy^dz


## Some edge-cases:
hodge(scalar(1),2)
hodge(zero(5),9)
hodge(volume(5))
hodge(volume(5),lose=TRUE)
hodge(scalar(7),n=9)



}
