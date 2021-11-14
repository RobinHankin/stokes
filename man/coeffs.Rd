\name{coeffs}
\alias{coeffs}
\alias{coeff}
\alias{coeffs<-}
\alias{coeffs.ktensor}
\alias{coeffs.kform}
\alias{coeffs<-.ktensor}
\alias{coeffs<-.kform}
\alias{coeffs,ktensor-method}
\alias{coeffs,kform-method}
\alias{value<-}
\alias{coeffs<-,ktensor-method}
\alias{coeffs<-,kform-method}
\title{
  Extract and manipulate coefficients
}
\description{
  Extract and manipulate coefficients of ktensor and kform objects
}
\usage{
\method{coeffs}{kform}(S) <- value
\method{coeffs}{ktensor}(S) <- value
}
\arguments{
  \item{S}{Object to be tested for being a spray}
  \item{value}{In the assignment operator \code{coeffs<-()}, a disord
    object so that \code{coeffs(S) <- x} works as expected}
}
\details{

  To see the coefficients of a \code{kform} or \code{ktensor} object,
  use \code{coeffs()}, which returns a \code{disord} object (this is
  actually \code{spray::coeffs()}).  Replacement methods are specific to
  the \pkg{stokes} package but also use \code{disord} discipline.
  
}
\author{Robin K. S. Hankin}
  
\examples{
a <- kform_general(5,2,1:10)
coeffs(a) # a disord object
coeffs(a)[coeffs(a)\%\%2==1] <- 100  # replace every odd coeff with 100

}

\keyword{symbolmath}
