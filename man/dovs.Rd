\name{dovs}
\alias{dovs}
\title{Dimension of the underlying vector space}
\description{\loadmathjax

A \mjseqn{k}-form \mjeqn{\omega\in\Lambda^k(V)}{omitted} maps
\mjseqn{V^k} to the reals, where \mjeqn{V=\mathcal{R}^n}{omitted}.
Function \code{dovs()} returns \mjseqn{n}, the dimensionality of the
underlying vector space.  The function itself is almost trivial,
returning the maximum of the index matrix.

Special dispensation is given for zero-forms and zero tensors, which
return zero.

Vignette \code{dovs} provides more discussion.

}
\usage{dovs(K)}
\arguments{\item{K}{A \mjseqn{k}-form or \mjseqn{k}-tensor}}
\value{Returns a non-negative integer}
\author{Robin K. S. Hankin}
\examples{
dovs(rform())

table(replicate(20,dovs(rform(3))))  

}
