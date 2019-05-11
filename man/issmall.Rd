\name{issmall}
\alias{issmall}
\title{Is a form zero to within numerical precision?}
\description{Given a \eqn{k}-form, return \code{TRUE} if it is \dQuote{small}}
\usage{
issmall(x, tol=1e-8)
}
\arguments{
  \item{x}{Object of class \code{kform}}
  \item{tol}{Small tolerance, defaulting to \code{1e-8}}
}
\value{Returns a logical}
\author{Robin K. S. Hankin}
\examples{

o <- kform_general(4,2,runif(6))
M <- matrix(rnorm(36),6,6)

discrepancy <- o - transform(transform(o,M),solve(M))

issmall(discrepancy)  # should be TRUE
is.zero(discrepancy)  # might be FALSE
}
