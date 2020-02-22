\name{rform}
\alias{rform}
\alias{rkform}
\alias{rtensor}
\alias{rktensor}
\title{Random kforms and ktensors}
\description{Random \eqn{k}-form objects and \eqn{k}-tensors,
  intended as quick \dQuote{get you going} examples}
\usage{
rform(terms=9,k=3,n=7,coeffs)
rtensor(terms=9,k=3,n=7,coeffs)
}
\arguments{
  \item{terms}{Number of distinct terms}
  \item{k,n}{A \eqn{k}-form maps \eqn{V^k} to \eqn{R}, where \eqn{V=R^n}}
  \item{coeffs}{The coefficients of the form; if missing use 1
    (inherited from \code{spray()})}
}
\details{
  What you see is what you get, basically.

  Note that argument \code{terms} is an upper bound, as the index matrix
  might contain repeats.  But \code{coeffs} should have length equal to
  \code{terms} (or 1).
}
\value{
  All functions documented here return an object of class \code{kform} or
  \code{ktensor}.
}
\author{Robin K. S. Hankin}
\examples{

rform()
rform(coeffs=1:9)   # any repeated rows are combined

dx <- as.kform(1)
dy <- as.kform(2)
rform() \%^\% dx
rform() \%^\% dx \%^\% dy

rtensor()
}