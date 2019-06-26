\name{transform}
\alias{transform}
\alias{stretch}
\title{Linear transforms of \eqn{k}-forms}
\description{Given a \eqn{k}-form, express it in terms of linear
  combinations of the \eqn{dx^i}}
\usage{
transform(omega,M)
stretch(omega,d)
}
\arguments{
  \item{omega}{Object of class \code{kform}}
  \item{M}{Matrix of transformation}
  \item{d}{Numeric vector representing the diagonal elements of a
    diagonal matrix}
}
\details{
  Suppose we are given a two-form

  \deqn{
    \omega=\sum_{i<j}a_{ij}dx_i\wedge dx_j}{omitted: see PDF}

  and relationships

  \deqn{dx_i=\sum_rM_{ir}dy_r}{omitted: see PDF}

  then we would have

  \deqn{\omega =
    \sum_{i<j}
    a_{ij}\left(\sum_rM_{ir}dy_r\right)\wedge\left(\sum_rM_{jr}dy_r\right)
  }{omitted: see PDF}

  The general situation would be  a \eqn{k}-form where we would have
  \deqn{
    \omega=\sum_{i_1<\cdots<i_k}a_{i_1\ldots i_k}dx_{i_1}\wedge\cdots\wedge dx_{i_k}}{omitted: see PDF}

  giving

  \deqn{\omega =
    \sum_{i_1<\cdots <i_k}\left[
    a_{i_1<\cdots < i_k}\left(\sum_rM_{i_1r}dy_r\right)\wedge\cdots\wedge\left(\sum_rM_{i_kr}dy_r\right)\right]
  }{omitted: see PDF}

So \eqn{\omega} was given in terms of \eqn{dx_1,\ldots,dx_k} and we have
expressed it in terms of \eqn{dy_1,\ldots,dy_k}.  So for example if

\deqn{\omega=
  dx_1\wedge dx_2 + 5dx_1\wedge dx_3}{ommited: see PDF
}

and

\deqn{
  \left(
  \begin{array}{l}
  dx_1\\
  dx_2\\
  dx_3
  \end{array}
  \right)=
\left(
\begin{array}{ccc}
1 & 4 & 7\\
2 & 5 & 8\\
3 & 6 & 9\\
\end{array}
\right)  \left(
  \begin{array}{l}
  dy_1\\ dy_2\\   dy_3
  \end{array}
  \right)
}{omitted; see PDF}

then

\deqn{\begin{array}{ccl}
  \omega &=& 
\left(1dy_1+4dy_2+7dy_3\right)\wedge
\left(2dy_1+5dy_2+8dy_3\right)+
5\left(1dy_1+4dy_2+7dy_3\right)\wedge
\left(3dy_1+6dy_2+9dy_3\right)
\\
&=&2dy_1\wedge dy_1+5dy_1\wedge dy_2+\cdots+
5\cdot 7\cdot 6dx_3\wedge dx_2+
5\cdot 7\cdot 9dx_3\wedge dx_3+\\
&=& -33dy_1\wedge dy_2-66dy_1\wedge dy_3-33dy_2\wedge dy_3
\end{array}
}{omitted; see PDF}

The \code{transform()} function does all this but it is slow.  I am not
100\% sure that there isn't a much more efficient way to do such a
transformation.  There are a few tests in \code{tests/testthat}.

Function \code{stretch()} carries out the same operation but for a
matrix with zero off-diagonal elements.  It is much faster.

}
\value{Returns a \eqn{k}-form}
\author{Robin K. S. Hankin}
\seealso{\code{\link{wedge}}}
\examples{

# Example in the text:
omega <- as.kform(matrix(c(1,1,2,3),2,2),c(1,5))
M <- matrix(1:9,3,3)
transform(omega,M)

# Demonstrate that the result can be complicated:
M <- matrix(rnorm(25),5,5)
transform(as.kform(1:2),M)

# Numerical verification:
o <- rform(terms=2,n=5)

o2 <- transform(transform(o,M),solve(M))
max(abs(value(o-o2))) # zero to numerical precision

# Following should be zero:
transform(as.kform(1),M)-as.kform(matrix(1:5),c(crossprod(M,c(1,rep(0,4)))))

# Following should be TRUE:
issmall(transform(o,crossprod(matrix(rnorm(10),2,5))))

# Some stretch() use-cases:

p <- rform()
p
stretch(p,seq_len(5))
stretch(p,c(1,0,1,1,1))   # kills dimension 2

# Works nicely with pipes:
\dontrun{
max(abs(value(o-o \%>\% transform(M) \%>\% transform(solve(M)))))
}

}
