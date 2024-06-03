\name{dovs}
\alias{dovs}
\title{Dimension of the underlying vector space}
\description{

A \eqn{k}-form \eqn{\omega\in\Lambda^k(V)}{omitted} maps
\eqn{V^k} to the reals, where \eqn{V=\mathbb{R}^n}{V=R^n}.
Function \code{dovs()} returns \eqn{n}, the dimensionality of the
underlying vector space.  The function itself is almost trivial,
returning the maximum of the index matrix.

Special dispensation is given for zero-forms and zero tensors, which
return zero.

Vignette \code{dovs} provides more discussion.

}
\usage{dovs(K)}
\arguments{\item{K}{A \eqn{k}-form or \eqn{k}-tensor}}
\value{Returns a non-negative integer}
\author{Robin K. S. Hankin}
\examples{
dovs(rform())

table(replicate(20,dovs(rform(3))))  

}
