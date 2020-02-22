\name{keep}
\alias{keep}
\alias{discard}
\alias{retain}
\alias{drop.free}
\title{Keep or drop variables}
\description{Keep or drop variables}
\usage{
keep(K, yes)
discard(K, no)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{yes,no}{Specification of dimensions to either keep (yes) or discard 
    (no), coerced to a free object}
}
\details{
  Function \code{keep(omega,yes)} keeps the terms specified and
  \code{discard(omega,no)} discards the terms specifed.  It is not clear
  to me what these functions mean from a mathematical perspective.
}
\value{The functions documented here all return a \code{kform} object.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{lose}}}
\examples{

keep(kform_general(7,3),1:4)   # keeps only terms with dimensions 1-4
discard(kform_general(7,3),1)  # loses any term with a "1" in the index

}
