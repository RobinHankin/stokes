\name{Alt}
\alias{Alt}
\title{Alternating multilinear forms}
\description{Converts a \eqn{k}-tensor to alternating form}
\usage{Alt(S)}
\arguments{
  \item{S}{A multilinear form, an object of class \code{ktensor}}
}
\details{
  Given a \eqn{k}-tensor \eqn{T}, we have

  \deqn{\mathrm{Alt}(T)\left(v_1,\ldots,v_k\right)=
    \frac{1}{k!}\sum_{\sigma\in S_k}\mathrm{sgn}(\sigma)\cdot
    T\left(v_{\sigma(1)},\ldots,v_{\sigma(k)}\right)
  }{omitted; see PDF}

  Thus for example if \eqn{k=3}:

    \deqn{\mathrm{Alt}(T)\left(v_1,v_2,v_3\right)=
      \frac{1}{6}\left(\begin{array}{cc}
      +T\left(v_1,v_2,v_3\right)&
      -T\left(v_1,v_3,v_2\right)\cr
      -T\left(v_2,v_1,v_3\right)&
      +T\left(v_2,v_3,v_1\right)\cr
      +T\left(v_3,v_1,v_2\right)&
      -T\left(v_3,v_2,v_1\right)
      \end{array}
      \right)
    }{omitted; see PDF}

    and it is reasonably easy to see that \eqn{\mathrm{Alt}(T)}{Alt(T)}
    is alternating, in the sense that

  \deqn{\mathrm{Alt}(T)\left(v_1,\ldots,v_i,\ldots,v_j,\ldots,v_k\right)=
    -\mathrm{Alt}(T)\left(v_1,\ldots,v_j,\ldots,v_i,\ldots,v_k\right)
  }{omitted; see PDF}

  Function \code{Alt()} takes and returns an object of class
  \code{ktensor}.
}
\value{
  Returns an alternating \eqn{k}-tensor.  To coerce to a \eqn{k}-form,
  which is a much more efficient representation, 
  use \code{as.kform()}.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{kform}}}
\examples{

S <- as.ktensor(expand.grid(1:3,1:3),rnorm(9))
S
Alt(S)

issmall(Alt(S) - Alt(Alt(S)))  # should be TRUE

}
