\name{keep}
\alias{keep}
\alias{discard}
\alias{retain}
\title{Keep or drop variables}
\description{Keep or drop variables}
\usage{
keep(K, yes)
discard(K, no)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{yes, no}{Specification of dimensions to either keep (yes) or
    discard (no)}
}
\details{

Given a \eqn{k}-form \eqn{\omega}{omitted}, we have
\eqn{\omega(v_1,\ldots,v_k)\in\mathbb{R}}{omitted}, where
\eqn{v_1,\ldots,v_k\in\mathbb{R}^n}{omitted}.  Now, \dfn{discarding}
dimension \eqn{i} is equivalent to asserting (or guaranteeing) that
\eqn{e_i\cdot v_j=0}{omitted} for \eqn{j=1,\ldots,k}{j=1,...,k}.
Alternatively, we may say that \eqn{\omega(v_1,\ldots,v_k)}{omitted} is
indeapendent of \eqn{e_i\cdot v_j}{omitted} for
\eqn{j=1,\ldots,k}{j=1,...,k}.  If this is the case, we may ignore any
row in which an \eqn{i} appears.

For \eqn{k}-forms, discarding (and its dual, keeping) is carried out in
functions \code{discard()} and \code{keep()}.  Function
\code{keep(omega, yes)} keeps the terms specified and
\code{discard(omega, no)} discards the terms specified.
}
\value{The functions documented here all return a \code{kform} object.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{lose}}}
\examples{

(o <- kform_general(7, 3, seq_len(choose(7, 3))))
keep(o, 1:4)     # keeps only terms with dimensions 1-4
discard(o, 1:2)  # loses any term with "1" or "2" in the index

}
