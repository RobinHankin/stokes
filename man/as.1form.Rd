\name{as.1form}
\alias{as.1form}
\alias{grad}
\title{Coerce vectors to 1-forms}
\description{\loadmathjax
  Given a vector, return the corresponding 1-form; the exterior
  derivative of a 0-form (that is, a scalar function).  Function
  \code{grad()} is a synonym.
}
\usage{
as.1form(v)
grad(v)
}
\arguments{
  \item{v}{A vector with element \eqn{i} being \eqn{\partial f/\partial
      x_i}{df/dxi}}
}
\details{

The exterior derivative of a \eqn{k}-form \mjeqn{\phi}{phi} is a
\eqn{(k+1)}-form \mjeqn{\mathbf{d}\phi}{d phi} given by

\mjdeqn{
  \mathbf{d}\phi
  \left(
  P_\mathbf{x}\left(\mathbf{v}_i,\ldots,\mathbf{v}_{k+1}\right)
  \right)
  =
  \lim_{h\longrightarrow 0}\frac{1}{h^{k+1}}\int_{\partial
  P_\mathbf{x}\left(h\mathbf{v}_1,\ldots,h\mathbf{v}_{k+1}\right)}\phi
  }{omitted; see latex}

  We can use the facts that

  \mjdeqn{
    \mathbf{d}\left(f\,dx_{i_1}\wedge\cdots\wedge dx_{i_k}\right)=
    \mathbf{d}f\wedge dx_{i_1}\wedge\cdots\wedge dx_{i_k}
  }{omitted; see latex}

  and
  \mjdeqn{
    \mathbf{d}f=\sum_{j=1}^n\left(D_j f\right)\,dx_j
  }{omitted; see latex}
  
  to calculate differentials  of general \eqn{k}-forms. Specifically, if

  \mjdeqn{
    \phi=\sum_{1\leq i_i < \cdots < i_k\leq n} a_{i_1\ldots
      i_k}dx_{i_1}\wedge\cdots\wedge dx_{i_k}
  }{omitted; see latex}

  then
  \mjdeqn{
    \mathbf{d}\phi=
    \sum_{1\leq i_i < \cdots < i_k\leq n}
    [\sum_{j=1}^nD_ja_{i_1\ldots i_k}dx_j]\wedge dx_{i_1}\wedge\cdots\wedge dx_{i_k.}
  }{omitted; see latex}

The entry in square brackets is given by \code{grad()}.  See the
examples for appropriate R idiom.

}  
\value{A one-form}
\author{Robin K. S. Hankin}
\seealso{\code{\link{kform}}}
\examples{

as.1form(1:9)  # note ordering of terms


as.1form(rnorm(20))

grad(c(4,7)) \%^\% grad(1:4)




}