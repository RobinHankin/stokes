\name{rform}
\alias{rform}
\alias{rkform}
\alias{rtensor}
\alias{rktensor}
\title{Random kforms and ktensors}
\description{Random k-form objects and k-tensors, intended as quick
  \dQuote{get you going} examples}
\usage{
rform(terms=9,k=3,n=7,coeffs)
rtensor(terms=9,k=3,n=7,coeffs)
}
\arguments{
  \item{terms}{Number of distinct terms}
  \item{k,n}{A kform maps \eqn{V^k} to \eqn{R}, where \eqn{V=R^n}}
  \item{coeffs}{The coefficients of the form; if missing use 1
    (inherited from \code{spray()})}
}
\details{
What you see is what you get, basically.
}
\author{Robin K. S. Hankin}
\examples{
rform()


dx <- as.kform(1)
dy <- as.kform(2)
rform() \%^\% dx
rform() \%^\% dx \%^\% dy


rtensor()
}