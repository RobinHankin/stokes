\name{scalar}
\alias{scalar}
\alias{is.scalar}
\alias{0form}
\alias{lose}
\alias{lose.kform}
\alias{lose.ktensor}
\alias{drop}
\title{Lose attributes}
\description{Scalars: \eqn{0}-forms and \eqn{0}-tensors}
\usage{
scalar(s,lose=FALSE)
is.scalar(M)
`0form`(s,lose=FALSE)
\method{lose}{kform}(M)
\method{lose}{ktensor}(M)
}
\arguments{
  \item{s}{A scalar value; a number}
  \item{M}{Object of class \code{ktensor} or \code{kform}}
  \item{lose}{In function \code{scalar()}, Boolean with \code{TRUE}
    meaning to return a normal scalar, and default \code{FALSE} meaning
    to return a formal \eqn{0}-form or \eqn{0}-tensor}
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

A \eqn{0}-form is not the same thing as a zero tensor.  A \eqn{0}-form
maps \eqn{V^0} to the reals; a scalar.  A zero tensor maps \eqn{V^k} to
zero.
}
\value{
  The functions documented here return an object of class
  \code{kform} or \code{ktensor}, except for \code{is.scalar()}, which
  returns a Boolean.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{zeroform}},\code{\link{lose}}}

\examples{

o <- scalar(5)
o
lose(o)

kform_general(1,0)  
kform_general(1,0,lose=FALSE)

}