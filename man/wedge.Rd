\name{wedge}
\alias{wedge}
\alias{wedge2}
\alias{\%^\%}
\title{Wedge products}
\description{Wedge products of k forms}
\usage{
wedge2(F1,F2)
wedge(x, ...)
}

\arguments{
  \item{F1,F2,x,...}{k-forms}
}
\details{
Wedge product of k-forms.  
}
\value{
Returns a k-form.
}
\author{Robin K. S. Hankin}
\note{
In general use, use \code{wedge()}.  This uses low-level helper function \code{wedge2()}.
}
\examples{
k1 <- as.kform(cbind(1:5,2:6),1:5)
k2 <- as.kform(cbind(5:7,6:8,7:9),1:3)



wedge(k1,k2)

dx <- as.kform(1)
dy <- as.kform(2)
dz <- as.kform(3)

wedge(dx,dy,dz)
dx \%^\% dy \%^\% dz

wedge(k1,k2,as.kform(3))


k3 <- kform_general(1:6,2)

a1 <- wedge2(k1,wedge2(k2,k3))
a2 <- wedge2(wedge2(k1,k2),k3)

#is.zero(a1-a2)  # NB terms of a1, a2 in a different order!

# This is why wedge(k1,k2,k3) is well-defined.
}