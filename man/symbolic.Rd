\name{symbolic}
\alias{symbolic}
\alias{as.symbolic}
\title{Symbolic form}
\description{Prints \eqn{k}-tensor and \eqn{k}-form objects in symbolic
  form}
\usage{
as.symbolic(M,symbols=letters,d="")
}
\arguments{
  \item{M}{Object of class \code{kform} or \code{ktensor}; a map from
    \eqn{V^k} to \eqn{R}, where \eqn{V=R^n}}
  \item{symbols}{A character vector giving the names of the symbols}
  \item{d}{String specifying the appearance of the differential operator}
}
\value{Returns a noquote character string.}
\author{Robin K. S. Hankin}
\examples{

as.symbolic(rtensor())
as.symbolic(rform())

as.symbolic(kform_general(3,2,1:3),d="d",symbols=letters[23:26])
}
