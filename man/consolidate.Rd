\name{consolidate}
\alias{consolidate}
\alias{lose_repeats}
\alias{kill_trivial_rows}
\alias{include_perms}
\title{Various low-level helper functions}
\description{
  Various low-level helper functions used in \code{Alt()} and
  \code{kform()}
}
\usage{
consolidate(S)
kill_trivial_rows(S)
include_perms(S)
}
\arguments{
  \item{S}{Object of class \code{spray}}
}
\details{
  Low-level helper functions.

  \itemize{
    \item Function \code{consolidate()} takes a spray object, and
    combines any rows that are identical up to a permutation, respecting
    the sign of the permutation
    \item Function \code{kill_trivial_rows()} takes a spray object and
    deletes any rows with a repeated entry (which have \eqn{k}-forms
    identically zero)
    \item Function \code{include_perms()} replaces each row of a
    \code{spray} object with all its permutations, respecting the sign
    of the permutation
  }
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ktensor}},\code{\link{kform}}}
\examples{

S <- spray(matrix(c(1,1,2,2,1,3,3,1,3,5),ncol=2,byrow=TRUE),1:5)
kill_trivial_rows(S)
consolidate(S)

\dontrun{include_perms(S)} #  This will fail because of the repeated rows
include_perms(kill_trivial_rows(S)) # This should work

}