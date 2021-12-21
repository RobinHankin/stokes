The stokes package: exterior calculus in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/stokes.png" width = "150" align="right" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/stokes)](https://cran.r-project.org/package=stokes)
[![Codecov test
coverage](https://codecov.io/gh/RobinHankin/stokes/branch/master/graph/badge.svg)](https://codecov.io/gh/RobinHankin/stokes/branch/master)
<!-- badges: end -->

# Overview

The `stokes` package provides functionality for working with the
exterior calculus. It includes cross products and wedge products and a
variety of use-cases. The canonical reference would be Spivak (see
references). A detailed vignette is provided in the package.

The package deals with
![k](https://latex.codecogs.com/png.latex?k "k")-tensors and
![k](https://latex.codecogs.com/png.latex?k "k")-forms. A
![k](https://latex.codecogs.com/png.latex?k "k")-tensor is a multilinear
map
![S\\colon V^k\\longrightarrow\\mathbb{R}](https://latex.codecogs.com/png.latex?S%5Ccolon%20V%5Ek%5Clongrightarrow%5Cmathbb%7BR%7D "S\colon V^k\longrightarrow\mathbb{R}"),
where
![V=\\mathbb{R}^n](https://latex.codecogs.com/png.latex?V%3D%5Cmathbb%7BR%7D%5En "V=\mathbb{R}^n")
is considered as a vector space. Given two
![k](https://latex.codecogs.com/png.latex?k "k")-tensors
![S,T](https://latex.codecogs.com/png.latex?S%2CT "S,T") the package can
calculate their outer product
![S\\otimes T](https://latex.codecogs.com/png.latex?S%5Cotimes%20T "S\otimes T")
using natural R idiom (see below and the vignette for details).

A ![k](https://latex.codecogs.com/png.latex?k "k")-form is an
alternating ![k](https://latex.codecogs.com/png.latex?k "k")-tensor,
that is a ![k](https://latex.codecogs.com/png.latex?k "k")-tensor
![\\omega](https://latex.codecogs.com/png.latex?%5Comega "\omega") with
the property that linear dependence of
![x_1,\\ldots,x_n](https://latex.codecogs.com/png.latex?x_1%2C%5Cldots%2Cx_n "x_1,\ldots,x_n")
implies that
![\\omega\\left(x_1,\\ldots,x_n\\right)=0](https://latex.codecogs.com/png.latex?%5Comega%5Cleft%28x_1%2C%5Cldots%2Cx_n%5Cright%29%3D0 "\omega\left(x_1,\ldots,x_n\right)=0").
Given ![k](https://latex.codecogs.com/png.latex?k "k")-forms
![\\omega,\\eta](https://latex.codecogs.com/png.latex?%5Comega%2C%5Ceta "\omega,\eta"),
the package provides R idiom for calculating their wedge product
![\\omega\\wedge\\eta](https://latex.codecogs.com/png.latex?%5Comega%5Cwedge%5Ceta "\omega\wedge\eta").

# Installation

You can install the released version of stokes from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("stokes")  # uncomment this to install the package
library("stokes")
set.seed(0)
```

# The `stokes` package in use

The package has two main classes of objects, `kform` and `ktensor`. We
may define a ![k](https://latex.codecogs.com/png.latex?k "k")-tensor as
follows

``` r
KT <- as.ktensor(cbind(1:4,3:5),1:4)
#> Warning in cbind(1:4, 3:5): number of rows of result is not a multiple of vector
#> length (arg 2)
KT
#> A linear map from V^2 to R with V=R^5:
#>          val
#>  4 3  =    4
#>  3 5  =    3
#>  2 4  =    2
#>  1 3  =    1
```

We can coerce `KT` to a function and then evaluate it:

``` r
KT <- as.ktensor(cbind(1:4,2:5),1:4)
f <- as.function(KT)
E <- matrix(rnorm(10),5,2)
f(E)
#> [1] 11.23556
```

Cross products are implemented:

``` r
KT %X% KT
#> A linear map from V^4 to R with V=R^5:
#>              val
#>  1 2 1 2  =    1
#>  2 3 1 2  =    2
#>  3 4 3 4  =    9
#>  2 3 4 5  =    8
#>  1 2 2 3  =    2
#>  1 2 4 5  =    4
#>  4 5 4 5  =   16
#>  2 3 3 4  =    6
#>  4 5 3 4  =   12
#>  1 2 3 4  =    3
#>  3 4 4 5  =   12
#>  3 4 2 3  =    6
#>  4 5 2 3  =    8
#>  3 4 1 2  =    3
#>  2 3 2 3  =    4
#>  4 5 1 2  =    4
```

## Alternating forms

An alternating form (or
![k](https://latex.codecogs.com/png.latex?k "k")-form) is an
antisymmetric ![k](https://latex.codecogs.com/png.latex?k "k")-tensor;
the package can convert a general
![k](https://latex.codecogs.com/png.latex?k "k")-tensor to alternating
form using the `Alt()` function:

``` r
Alt(KT)
#> A linear map from V^2 to R with V=R^5:
#>           val
#>  5 4  =  -2.0
#>  4 5  =   2.0
#>  4 3  =  -1.5
#>  3 2  =  -1.0
#>  2 3  =   1.0
#>  3 4  =   1.5
#>  2 1  =  -0.5
#>  1 2  =   0.5
```

However, the package provides a bespoke and efficient representation for
![k](https://latex.codecogs.com/png.latex?k "k")-forms as objects with
class `kform`. Such objects may be created using the `as.kform()`
function:

``` r
M <- matrix(c(4,2,3,1,2,4),2,3,byrow=TRUE)
M
#>      [,1] [,2] [,3]
#> [1,]    4    2    3
#> [2,]    1    2    4
KF <- as.kform(M,c(1,5))
KF
#> An alternating linear map from V^3 to R with V=R^4:
#>            val
#>  1 2 4  =    5
#>  2 3 4  =    1
```

We may coerce `KF` to functional form:

``` r
f <- as.function(KF)
E <- matrix(rnorm(12),4,3)
f(E)
#> [1] -5.979544
```

# The wedge product

The wedge product of two
![k](https://latex.codecogs.com/png.latex?k "k")-forms is implemented as
`^` or `wedge()`:

``` r
KF2 <- kform_general(6:9,2,1:6)
KF2
#> An alternating linear map from V^2 to R with V=R^9:
#>          val
#>  8 9  =    6
#>  7 9  =    5
#>  6 9  =    4
#>  7 8  =    3
#>  6 8  =    2
#>  6 7  =    1
KF ^ KF2
#> An alternating linear map from V^5 to R with V=R^9:
#>                val
#>  1 2 4 6 7  =    5
#>  1 2 4 6 8  =   10
#>  2 3 4 6 8  =    2
#>  2 3 4 7 8  =    3
#>  2 3 4 6 9  =    4
#>  1 2 4 6 9  =   20
#>  2 3 4 6 7  =    1
#>  2 3 4 7 9  =    5
#>  1 2 4 7 8  =   15
#>  2 3 4 8 9  =    6
#>  1 2 4 7 9  =   25
#>  1 2 4 8 9  =   30
```

The package can accommodate a number of results from the exterior
calculus such as elementary forms:

``` r
dx <- as.kform(1)
dy <- as.kform(2)
dz <- as.kform(3)
dx ^ dy ^ dz  # element of volume 
#> An alternating linear map from V^3 to R with V=R^3:
#>            val
#>  1 2 3  =    1
```

A number of useful functions from the exterior calculus are provided,
such as the gradient of a scalar function:

``` r
grad(1:6)
#> An alternating linear map from V^1 to R with V=R^6:
#>        val
#>  6  =    6
#>  5  =    5
#>  4  =    4
#>  3  =    3
#>  2  =    2
#>  1  =    1
```

The package takes the leg-work out of the exterior calculus:

``` r
grad(1:4) ^ grad(1:6)
#> An alternating linear map from V^2 to R with V=R^6:
#>          val
#>  4 5  =   20
#>  1 5  =    5
#>  2 5  =   10
#>  3 5  =   15
#>  2 6  =   12
#>  4 6  =   24
#>  3 6  =   18
#>  1 6  =    6
```

# References

The most concise reference is

-   Spivak 1971. *Calculus on manifolds*, Addison-Wesley.

But an accessible book would be

-   Hubbard and Hubbard 2015. *Vector calculus, linear algebra, and
    differential forms: a unified approach*. Matrix Editions

# Further information

For more detail, see the package vignette

`vignette("stokes")`
