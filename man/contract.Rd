\name{contract}
\alias{contract}
\alias{contract_elementary}
\title{Contractions of \eqn{k}-forms}
\description{\loadmathjax
  A contraction is a natural linear map from \eqn{k}-forms to \eqn{k-1}-forms.
}
\usage{
contract(K,v,lose=TRUE)
contract_elementary(o,v)
}
\arguments{
  \item{K}{A \eqn{k}-form}
  \item{o}{Integer-valued vector corresponding to one row of an index
    matrix}
  \item{lose}{Boolean, with default \code{TRUE} meaning to coerce a
    \eqn{0}-form to a scalar and \code{FALSE} meaning to return the
    formal \eqn{0}-form}
  \item{v}{A vector; in function \code{contract()}, if a matrix,
    interpret each column as a vector to contract with}
}
\details{  
  Given a \eqn{k}-form \mjeqn{\phi}{phi} and a vector \mjeqn{\mathbf{v}}{v},
the \dfn{contraction} \eqn{\phi_\mathbf{v}}{phi_v} of \mjeqn{\phi}{phi}
and \mjeqn{\mathbf{v}}{v} is a \mjseqn{k-1}-form with

\mjdeqn{
  \phi_\mathbf{v}\left(\mathbf{v}^1,\ldots,\mathbf{v}^{k-1}\right) =
  \phi\left(\mathbf{v},\mathbf{v}^1,\ldots,\mathbf{v}^{k-1}\right)
  }{
ommitted; see PDF}

if \mjseqn{k>1}; we specify
\mjeqn{\phi_\mathbf{v}=\phi(\mathbf{v})}{phi_v=phi(v)} if \mjseqn{k=1}.

Function \code{contract_elementary()} is a low-level helper function
that translates elementary \eqn{k}-forms with coefficient 1 (in the form
of an integer vector corresponding to one row of an index matrix) into
its contraction with \eqn{\mathbf{v}}{v}.
}
\value{Returns an object of class \code{kform}.}
\references{

Steven H. Weintraub 2014. \dQuote{Differential forms: theory and
practice}, Elsevier (contractions defined in Definition 2.2.23 in
chapter 2, page 77).

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{wedge}},\code{\link{lose}}}
\examples{
contract(as.kform(1:5),1:8)
contract(as.kform(1),3)   # 0-form


## Now some verification:
o <- kform(spray(t(replicate(2, sample(9,5))), runif(2)))
V <- matrix(rnorm(45),ncol=5)
jj <- c(
   as.function(o)(V),
   as.function(contract(o,V[,1,drop=TRUE]))(V[,-1]), # scalar
   as.function(contract(o,V[,1:2]))(V[,-(1:2),drop=FALSE]),
   as.function(contract(o,V[,1:3]))(V[,-(1:3),drop=FALSE]),
   as.function(contract(o,V[,1:4]))(V[,-(1:4),drop=FALSE]),
   as.function(contract(o,V[,1:5],lose=FALSE))(V[,-(1:5),drop=FALSE])
)

max(jj) - min(jj) # zero to numerical precision
}