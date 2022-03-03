\name{rform}
\alias{rform}
\alias{rkform}
\alias{rtensor}
\alias{rktensor}
\title{Random kforms and ktensors}
\description{\loadmathjax
  Random \mjseqn{k}-form objects and \mjseqn{k}-tensors,
  intended as quick \dQuote{get you going} examples}
\usage{
rform(terms=9,k=3,n=7,coeffs,ensure=TRUE)
rtensor(terms=9,k=3,n=7,coeffs)
}
\arguments{
  \item{terms}{Number of distinct terms}
  \item{k,n}{A \mjseqn{k}-form maps \mjseqn{V^k} to \mjseqn{R}, where
    \mjseqn{V=R^n}} 
  \item{coeffs}{The coefficients of the form; if missing use
    \code{seq_len(terms)}}
\item{ensure}{Boolean with default \code{TRUE} meaning to ensure that
  the \code{dovs()} of the returned value is in fact equal to \code{n}.
  If \code{FALSE}, sometimes the \code{dovs()} is strictly less than
  \code{n} because of random sampling}
}
\details{
  What you see is what you get, basically.

  Note that argument \code{terms} is an upper bound, as the index matrix
  might contain repeats which are combined.

}
\value{
  All functions documented here return an object of class \code{kform} or
  \code{ktensor}.
}
\author{Robin K. S. Hankin}
\examples{

rform()
rform() \%^\% rform()

rtensor() \%X\% rtensor()

rform() ^ dx
rform() ^ dx ^ dy

}