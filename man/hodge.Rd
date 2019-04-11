\name{hodge}
\alias{hodge}
\alias{Hodge}
\alias{star}
\title{Hodge star operator}
\description{Given a kform, return the its Hodge dual}
\usage{
hodge(K, n=max(index(K)), g=rep(1,n))
}
\arguments{
  \item{K}{Object of class kform}
  \item{n}{Dimensionality of space, defaulting the the largest element
    of the index}
  \item{g}{Diagonal of the metric tensor, defaulting to the standard metric}
}
\value{Returns a volume element}
\author{Robin K. S. Hankin}
\seealso{\code{\link{wedge}}}
\examples{

hodge(rform())

hodge(kform_general(4,2),g=c(-1,1,1,1))

}
