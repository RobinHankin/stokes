\name{rform}
\alias{rform}
\alias{rformm}
\alias{rformmm}
\alias{rkform}
\alias{rtensor}
\alias{rktensor}
\title{Random kforms and ktensors}
\description{
  Random \eqn{k}-form objects and \eqn{k}-tensors,
  intended as quick \dQuote{get you going} examples}
\usage{
rform(terms=9,k=3,n=7,coeffs,ensure=TRUE)
rformm(terms=30, k=7, n=20, coeffs, ensure)
rformmm(terms=90, k=15, n=30, coeffs, ensure)
rtensor(terms=9,k=3,n=7,coeffs)
}
\arguments{
  \item{terms}{Number of distinct terms}
  \item{k,n}{A \eqn{k}-form maps \eqn{V^k} to
    \eqn{\mathbb{R}}{R}, where \eqn{V=\mathbb{R}^n}{V=R^n}}
  \item{coeffs}{The coefficients of the form; if missing use
    \code{seq_len(terms)}}
  \item{ensure}{Boolean with default \code{TRUE} meaning to ensure that
    the \code{dovs()} of the returned value is in fact equal to
    \code{n}.  If \code{FALSE}, sometimes the \code{dovs()} is strictly
    less than \code{n} because of random sampling}
}
\details{
  Random \eqn{k}-form objects and \eqn{k}-tensors.

  By default, function \code{rform()} returns a simple \eqn{k}-form;
  \code{rformm()} and \code{rformmm()} return successively more
  complicated objects.

  Note that argument \code{terms} is an upper bound, as the index matrix
  might contain repeats which are combined.

}
\value{
  All functions documented here return an object of class \code{kform} or
  \code{ktensor}.
}
\author{Robin K. S. Hankin}
\examples{

(a <- rform())
(b <- rform())
a ^ b

a
a ^ dx
a ^ dx ^ dy

(x <- rtensor())
x \%X\% x

}
