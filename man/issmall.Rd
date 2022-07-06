\name{issmall}
\alias{issmall}
\title{Is a form zero to within numerical precision?}
\description{\loadmathjax
  Given a \mjseqn{k}-form, return \code{TRUE} if it is \dQuote{small}}
\usage{
issmall(M, tol=1e-8)
}
\arguments{
  \item{M}{Object of class \code{kform} or \code{ktensor}}
  \item{tol}{Small tolerance, defaulting to \code{1e-8}}
}
\value{Returns a logical}
\author{Robin K. S. Hankin}
\examples{

o <- kform_general(3,2,runif(3))
M <- matrix(rnorm(9),3,3)

discrepancy <- o - pullback(pullback(o,M),solve(M))

discrepancy # print method might imply coefficents are zeros

issmall(discrepancy)  # should be TRUE
is.zero(discrepancy)  # might be FALSE
}
