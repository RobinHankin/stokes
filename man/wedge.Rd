\name{wedge}
\alias{wedge}
\alias{wedge2}
\alias{\%^\%}
\title{Wedge products}
\description{
  Wedge products of \eqn{k}-forms}
\usage{
wedge2(K1,K2)
wedge(x, ...)
}
\arguments{
  \item{K1,K2,x,...}{\eqn{k}-forms}
}
\details{
Wedge product of \eqn{k}-forms.  
}
\value{
  The functions documented here return an object of class
  \code{kform}.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{Ops}}}
\note{
  In general use, use \code{wedge()} or \code{^} or \code{\%^\%}, as
  documented under \code{Ops}.  Function \code{wedge()} uses low-level
  helper function \code{wedge2()}, which takes only two arguments.

  A short vignette is provided with the package: type
  \code{vignette("wedge")} at the commandline.

}
\examples{

k1 <- as.kform(cbind(1:5,2:6),1:5)
k2 <- as.kform(cbind(5:7,6:8,7:9),1:3)
k3 <- kform_general(1:6,2)

a1 <- wedge2(k1,wedge2(k2,k3))
a2 <- wedge2(wedge2(k1,k2),k3)

is.zero(a1-a2)  # NB terms of a1, a2 in a different order!

# This is why wedge(k1,k2,k3) is well-defined.  Can also use ^:
k1 ^ k2 ^ k3
}
