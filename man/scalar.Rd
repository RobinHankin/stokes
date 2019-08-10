\name{scalar}
\alias{scalar}
\alias{0form}
\alias{lose}
\alias{lose.kform}
\alias{lose.ktensor}
\alias{drop}
\title{Lose attributes}
\description{Scalars: \eqn{0}-forms and \eqn{0}-tensors}
\usage{
scalar(s)
`0form`(s)
\method{lose}{kform}(x)
\method{lose}{ktensor}(x)
}
\arguments{
  \item{s}{A scalar value; a number}
  \item{x}{Object of class \code{ktensor} or \code{kform}}
}
\details{

A \eqn{k}-tensor (including \eqn{k}-forms) maps \eqn{k} vectors to a
scalar.  If \eqn{k=0}, then a \eqn{0}-tensor maps no vectors to a
scalar, that is, mapping nothing at all to a scalar, or what normal
people would call a plain old scalar.  Such forms are created by a
couple of constructions in the package, specifically \code{scalar()},
\code{kform_general(1,0)} and \code{contract()}.  These functions take a
\code{lose} argument that behaves much like the \code{drop} argument in
base extraction.

Function \code{lose()} takes an object of class \code{ktensor} or
\code{kform} and, if of arity zero, returns the coefficient.

Note that function \code{kform()} \emph{always} returns a \code{kform}
object, it never loses attributes.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{zeroform}},\code{\link{scalar}}}
\examples{

o <- scalar(5)
o
lose(o)

kform_general(1,0)  
kform_general(1,0,lose=FALSE)

}