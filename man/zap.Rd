\name{zap}
\alias{zap}
\alias{zap}
\alias{zapsmall}
\alias{zaptiny}
\alias{zap.spray}
\alias{zap.kform}
\alias{zap.ktensor}
\title{Zap small values in \eqn{k}-forms and \eqn{k}-tensors}
\description{
Equivalent to \code{zapsmall()}}
\usage{
zap(X)
\method{zap}{kform}(X)
\method{zap}{ktensor}(X)
}
\arguments{
  \item{X}{Tensor or \eqn{k}-form to be zapped}
}
\details{

  Given an object of class \code{ktensor} or \code{kform}, coefficients
  close to zero are \sQuote{zapped}, i.e., replaced by \sQuote{0}, using
  \code{base::zapsmall()}.

  Note, \code{zap()} actually changes the numeric value, it is not just
  a print method.

}
\value{Returns an object of the same class}
\author{Robin K. S. Hankin}
\examples{

S <- rform(7)
S == zap(S)
}
