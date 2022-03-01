\name{hodge}
\alias{hodge}
\alias{Hodge}
\alias{star}
\title{Hodge star operator}
\description{\loadmathjax
  Given a \mjseqn{k}-form, return its Hodge dual}
\usage{
hodge(K, n=max(index(K)), lose=TRUE)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{n}{Dimensionality of space, defaulting the the largest element
    of the index}
  \item{lose}{Boolean, with default \code{TRUE} meaning to coerce to a
    scalar if appropriate}
}
\value{Given a \mjseqn{k}-form, in an \mjseqn{n}-dimensional space, returns a
  \mjseqn{(n-k)}-form.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{wedge}}}
\examples{

hodge(rform())

M <- matrix(rnorm(30),6,5)
hodge(Reduce(`^`,sapply(seq_len(5),function(i){as.1form(M[,i])},simplify=FALSE)))


hodge(dx,3) == dy^dz


## Some edge-cases:
hodge(zero(5),9)
hodge(volume(5))
hodge(volume(5),lose=TRUE)
hodge(scalar(7),n=9)





}
