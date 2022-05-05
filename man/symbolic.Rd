\name{symbolic}
\alias{symbolic}
\alias{as.symbolic}
\title{Symbolic form}
\description{\loadmathjax

  Returns a character string representing \mjseqn{k}-tensor and
  \mjseqn{k}-form objects in symbolic form.  Used by the print method if
  either option \code{kform_symbolic_print} or
  \code{ktensor_symbolic_print} is non-null.
}
\usage{as.symbolic(M,symbols=letters,d="")}
\arguments{
  \item{M}{Object of class \code{kform} or \code{ktensor}; a map from
    \mjeqn{V^k}{V^k} to \mjseqn{R}, where \mjeqn{V=R^n}{V=R^n}}
  \item{symbols}{A character vector giving the names of the symbols}
  \item{d}{String specifying the appearance of the differential operator}
}
\details{

Spivak (p89), in archetypically terse writing, states:

A function \mjseqn{f} is considered to be a 0-form and
\mjeqn{f\cdot\omega}{omitted} is also written
\mjeqn{f\wedge\omega}{omitted}.  If
\mjeqn{f\colon\mathcal{R}^n\longrightarrow\mathcal{R}}{f: R^n -> R} is
differentiable, then
\mjeqn{Df(p)\in\Lambda^1\left(\mathcal{R}^n\right)}{omitted; see PDF}.
By a minor modification we therefore obtain a 1-form \mjseqn{df},
defined by

\mjdeqn{df(p)\left(v_p\right)=Df(p)(v)}{df(p)(v_p)=Df(p)(v).}

Let us consider in particular the 1-forms \mjeqn{d\pi^i}{omitted; see
  PDF}.  It is customary to let \mjseqn{x^i} denote the \emph{function}
  \mjeqn{\pi^i}{omitted; see PDF} (On \mjeqn{\mathcal{R}^3}{R^3} we
  often denote \mjseqn{x^1}, \mjseqn{x^2}, and \mjseqn{x^3} by
  \mjseqn{x}, \mjseqn{y}, and \mjseqn{z}).  This standard notation has
  obvious disadvantages but it allows many classical results to be
  expressed by formulas of equally classical appearance.  Since
  \mjeqn{dx^i(p)(v_p)=d\pi^i(p)(v_p)=D\pi^i(p)(v)=v^i}{(omitted; see
  PDF)}, we see that \mjeqn{dx^1(p),\ldots,dx^n(p)}{dx^1(p),...,dx^n(p)}
  is just the dual basis to
  \mjeqn{(e_1)_p,\ldots,(e_n)_p}{(e_1)_n,...,(e_n)_p}.  Thus every
  k-form \mjeqn{\omega}{omitted} can be written

  \mjdeqn{
    \omega=\sum_{i_1 < \cdots < i_k}\omega_{i_1,\ldots,i_k}
    dx^{i_1}\wedge\cdots\wedge dx^{i_k}.}{omitted.}

  Function \code{as.symbolic()} uses this format.  For completeness, we
  add (p77) that k-tensors may be expressed in the form

  \mjdeqn{
    \sum_{i_1,\ldots, i_k=1}^n a_{i_1,\ldots,i_k}\cdot
\phi_{i_1}\otimes\cdots\otimes\phi_{i_k}.}{omitted.}

and this form is used for k-tensors.

}
\value{Returns a noquote character string.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{print.stokes}},\code{\link{dx}}}
\examples{
(o <- kform_general(3,2,1:3))
as.symbolic(o,d="d",symbols=letters[23:26])
}
