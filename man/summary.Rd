\name{summary.stokes}
\alias{summary.stokes}
\alias{summary.kform}
\alias{summary.ktensor}
\alias{print.summary.kform}
\alias{print.summary.ktensor}
\alias{summary}
\alias{print.summary.spray}
\title{Summaries of tensors and alternating forms}
\description{
A summary method for tensors and alternating forms, and a print method for summaries.
}
\usage{
\method{summary}{kform}(object, ...)
\method{summary}{ktensor}(object, ...)
\method{print}{summary.kform}(x, ...)
\method{print}{summary.ktensor}(x, ...)
}
\arguments{
  \item{object,x}{Object of class \code{ktensor} or \code{kform}}
  \item{...}{Further arguments, passed to \code{head()}}
}
\details{
Summary method for tensors and alternating forms.  Uses \code{spray::summary()}.
}
\author{Robin K. S. Hankin}
\examples{
a <- rform(100)
summary(a)
options(kform_symbolic_print = TRUE)
summary(a)
options(kform_symbolic_print = NULL)  # restore default


}
