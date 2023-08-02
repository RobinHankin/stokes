\name{coeffs}
\alias{coeffs}
\alias{coeff}
\alias{coeffs<-}
\alias{coeffs<-.spray}
\alias{coeffs.ktensor}
\alias{coeffs.kform}
\alias{coeffs<-.ktensor}
\alias{coeffs<-.kform}
\alias{coeffs,ktensor-method}
\alias{coeffs,kform-method}
\alias{value<-}
\alias{coeffs<-,ktensor-method}
\alias{coeffs<-,kform-method}
\alias{spray}
\alias{as.spray}
\alias{nterms}
\title{Extract and manipulate coefficients}
\description{
  Extract and manipulate coefficients of ktensor and kform objects;
  this using the methods of the \CRANpkg{spray} package.

  Functions \code{as.spray()} and \code{nterms()} are imoported from
  \CRANpkg{spray}.

}
\details{
  To see the coefficients of a \code{kform} or \code{ktensor} object,
  use \code{coeffs()}, which returns a \code{disord} object (this is
  actually \code{spray::coeffs()}).  Replacement methods also use the
  methods of the \CRANpkg{spray} package.
}
\author{Robin K. S. Hankin}
  
\examples{
(a <- kform_general(5,2,1:10))
coeffs(a) # a disord object
coeffs(a)[coeffs(a)\%\%2==1] <- 100  # replace every odd coeff with 100
a

coeffs(a*0)
}

\keyword{symbolmath}
