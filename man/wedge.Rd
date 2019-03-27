\name{wedge}
\alias{wedge}
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
k1 <- kform(cbind(1:5,2:6),1:5)
k2 <- kform(cbind(5:7,6:8,7:9),1:3)

wedge(k1,k2)

dx <- kform(1)
dy <- kform(2)
dz <- kform(3)

wedge(dx,dy,dz)

wedge(k1,k2,kform(3))


}