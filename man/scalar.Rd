\name{scalar}
\alias{scalar}
\alias{is.scalar}
\alias{0form}
\alias{0tensor}
\alias{lose}
\alias{lose.kform}
\alias{lose.ktensor}
\alias{drop}
\title{Scalars and losing attributes}
\description{\loadmathjax
  Scalars: \mjseqn{0}-forms and \mjseqn{0}-tensors}
\usage{
scalar(s,kform=TRUE,lose=FALSE)
is.scalar(M)
`0form`(s=1,lose=FALSE)
`0tensor`(s=1,lose=FALSE)
\method{lose}{kform}(M)
\method{lose}{ktensor}(M)
}
\arguments{
  \item{s}{A scalar value; a number}
  \item{kform}{Boolean with default \code{TRUE} meaning to return a
    kform and \code{FALSE} meaning to return a ktensor}
  \item{M}{Object of class \code{ktensor} or \code{kform}}
  \item{lose}{In function \code{scalar()}, Boolean with \code{TRUE}
    meaning to return a normal scalar, and default \code{FALSE} meaning
    to return a formal \mjseqn{0}-form or \mjseqn{0}-tensor}
}
\details{

A \mjseqn{k}-tensor (including \mjseqn{k}-forms) maps \mjseqn{k} vectors
to a scalar.  If \mjseqn{k=0}, then a \mjseqn{0}-tensor maps no vectors
to a scalar, that is, mapping nothing at all to a scalar, or what normal
people would call a plain old scalar.  Such forms are created by a
couple of constructions in the package, specifically \code{scalar()},
\code{kform_general(1,0)} and \code{contract()}.  These functions take a
\code{lose} argument that behaves much like the \code{drop} argument in
base extraction.  Functions `0form()` and `0tensor()` are wrappers for
`scalar()`.

Function \code{lose()} takes an object of class \code{ktensor} or
\code{kform} and, if of arity zero, returns the coefficient.

Note that function \code{kform()} \emph{always} returns a \code{kform}
object, it never loses attributes.

There is a slight terminological problem.  A \mjseqn{k}-form maps
\mjseqn{k} vectors to the reals: so a \mjseqn{0}-form maps \mjseqn{0}
vectors to the reals.  This is what anyone on the planet would call a
scalar.  Similarly, a \mjseqn{0}-tensor maps \mjseqn{0} vectors to the
reals, and so is a scalar.  Mathematically, there is no difference
between \mjseqn{0}-forms and \mjseqn{0}-tensors, but the package makes a
distinction:

\preformatted{
> scalar(5,kform=TRUE)
An alternating linear map from V^0 to R with V=R^0:
     val
  =    5
> scalar(5,kform=FALSE)
A linear map from V^0 to R with V=R^0:
     val
  =    5
> 
}

Compare zero tensors and zero forms.  A zero tensor maps \mjseqn{V^k} to
the real number zero, and a zero form is an alternating tensor mapping
\mjseqn{V^k} to zero (so a zero tensor is necessarily alternating).  See
\code{zero.Rd}.

}
\value{
  The functions documented here return an object of class
  \code{kform} or \code{ktensor}, except for \code{is.scalar()}, which
  returns a Boolean.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{zeroform}}}

\examples{

o <- scalar(5)
o
lose(o)

kform_general(1,0)  
kform_general(1,0,lose=FALSE)

}