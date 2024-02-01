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

  Functions \code{as.spray()} and \code{nterms()} are imported from
  \CRANpkg{spray}.
}

\details{

  To see the coefficients of a \code{kform} or \code{ktensor} object,
  use \code{coeffs()}, which returns a \code{disord} object (this is
  actually \code{spray::coeffs()}).  Replacement methods also use the
  methods of the \CRANpkg{spray} package.  Note that \CRANpkg{disordR}
  discipline is enforced.

  Experimental functionality for \dQuote{pure} extraction and
  replacement is provided, following \CRANpkg{spray} version 1.0-25 or
  above.  Thus idiom such as \code{a[abs(coeffs(a)) > 0.1]} or indeed
  \code{a[coeffs(a) < 1] <- 0} should work as expected.

}

\author{Robin K. S. Hankin}  
\examples{
(a <- kform_general(5,2,1:10))
coeffs(a) # a disord object
coeffs(a)[coeffs(a)\%\%2==1] <- 100  # replace every odd coeff with 100
a

coeffs(a*0)

a <- rform()
a[coeffs(a) < 5]         # experimental
a[coeffs(a) > 3] <- 99   # experimental

}

\keyword{symbolmath}
