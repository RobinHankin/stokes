\name{transform}
\alias{transform}
\alias{pullback}
\alias{pull-back}
\alias{pushforward}
\alias{push-forward}
\alias{stretch}
\title{Linear transforms of \eqn{k}-forms}
\description{\loadmathjax
  Given a \eqn{k}-form, express it in terms of linear
  combinations of the \mjeqn{dx_i}{dx_i}}
\usage{
transform(K,M)
stretch(K,d)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{M}{Matrix of transformation}
  \item{d}{Numeric vector representing the diagonal elements of a
    diagonal matrix}
}
\details{
  Suppose we are given a two-form

  \mjdeqn{
    \omega=\sum_{i < j}a_{ij}dx_i\wedge dx_j}{omitted: see latex}

  and relationships

  \mjdeqn{dx_i=\sum_rM_{ir}dy_r}{omitted: see latex}

  then we would have

  \mjdeqn{\omega =
    \sum_{i < j}
    a_{ij}\left(\sum_rM_{ir}dy_r\right)\wedge\left(\sum_rM_{jr}dy_r\right).
  }{omitted: see latex}

  The general situation would be  a \eqn{k}-form where we would have
  \mjdeqn{
    \omega=\sum_{i_1 < \cdots < i_k}a_{i_1\ldots i_k}dx_{i_1}\wedge\cdots\wedge dx_{i_k}}{omitted: see latex}

  giving

  \mjdeqn{\omega =
    \sum_{i_1 < \cdots < i_k}\left[
    a_{i_1,\ldots, i_k}\left(\sum_rM_{i_1r}dy_r\right)\wedge\cdots\wedge\left(\sum_rM_{i_kr}dy_r\right)\right].
  }{omitted: see latex}

The \code{transform()} function does all this but it is slow.  I am not
100\% sure that there isn't a much more efficient way to do such a
transformation.  There are a few tests in \code{tests/testthat} and a
discussion in the \code{stokes} vignette.

Function \code{stretch()} carries out the same operation but for \eqn{M}
a diagonal matrix.  It is much faster than \code{transform()}.

}
\value{
  The functions documented here return an object of class
  \code{kform}.
} 
\references{
  S. H. Weintraub 2019. \emph{Differential forms: theory and practice}.
  Elsevier.  (Chapter 3)
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{wedge}}}
\examples{

# Example in the text:
K <- as.kform(matrix(c(1,1,2,3),2,2),c(1,5))
M <- matrix(1:9,3,3)
transform(K,M)

# Demonstrate that the result can be complicated:
M <- matrix(rnorm(25),5,5)
transform(as.kform(1:2),M)

# Numerical verification:
o <- rform(terms=2,n=5)

o2 <- transform(transform(o,M),solve(M))
max(abs(value(o-o2))) # zero to numerical precision

# Following should be zero:
transform(as.kform(1),M)-as.kform(matrix(1:5),c(crossprod(M,c(1,rep(0,4)))))

# Following should be TRUE:
issmall(transform(o,crossprod(matrix(rnorm(10),2,5))))

# Some stretch() use-cases:

p <- rform()
p
stretch(p,seq_len(7))
stretch(p,c(1,0,1,1,1))   # kills dimension 2

}
