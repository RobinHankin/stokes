
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
========

The wedge package provides functionality for working with the exterior calculus. It includes cross products and wedge products and a variety of use-cases. A vignette is provided in the package.

Installation
============

You can install the released version of wedge from [CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("wedge")  # uncomment this to use the package
library("wedge")
#> Loading required package: spray
#> 
#> Attaching package: 'spray'
#> The following objects are masked from 'package:base':
#> 
#>     pmax, pmin
```

The `wedge` package in use
==========================

The package has two main classes of objects, `kform` and `ktensor`. We may define a *k*-tensor as follows

``` r
KT <- as.ktensor(cbind(1:4,3:5),1:4)
#> Warning in cbind(1:4, 3:5): number of rows of result is not a multiple of
#> vector length (arg 2)
KT
#>          val
#>  4 3  =    4
#>  3 5  =    3
#>  1 3  =    1
#>  2 4  =    2
```

We can coerce `KT` to a function and then evaluate it:

``` r
KT <- as.ktensor(cbind(1:4,2:5),1:4)
f <- as.function(KT)
E <- matrix(rnorm(10),5,2)
f(E)
#> [1] 3.577917
```

Cross products are implemented:

``` r
KT %X% KT
#>              val
#>  2 3 2 3  =    4
#>  1 2 2 3  =    2
#>  3 4 2 3  =    6
#>  1 2 1 2  =    1
#>  3 4 1 2  =    3
#>  4 5 1 2  =    4
#>  2 3 3 4  =    6
#>  2 3 1 2  =    2
#>  1 2 3 4  =    3
#>  3 4 4 5  =   12
#>  1 2 4 5  =    4
#>  4 5 2 3  =    8
#>  4 5 4 5  =   16
#>  2 3 4 5  =    8
#>  4 5 3 4  =   12
#>  3 4 3 4  =    9
```

Alternating forms
-----------------

An alternating form (or *k*-form) is an antisymmetric *k*-tensor; the package can convert a general *k*-tensor to alternating form using the `Alt()` function:

``` r
Alt(KT)
#>           val
#>  3 4  =   1.5
#>  4 3  =  -1.5
#>  5 4  =  -2.0
#>  4 5  =   2.0
#>  2 1  =  -0.5
#>  3 2  =  -1.0
#>  2 3  =   1.0
#>  1 2  =   0.5
```

However, the package provides a bespoke and efficient representation for *k*-forms as objects with class `kform`. Such objects may be created using the `as.kform()` function:

``` r

M <- matrix(c(4,2,3,1,2,4),2,3,byrow=TRUE)
M
#>      [,1] [,2] [,3]
#> [1,]    4    2    3
#> [2,]    1    2    4
KF <- as.kform(M,c(1,5))
KF
#>            val
#>  1 2 4  =    5
#>  2 3 4  =    1
```

We may coerce `KF` to functional form:

``` r
f <- as.function(KF)
E <- matrix(rnorm(12),4,3)
f(E)
#> [1] -6.425367
```

The wedge product
=================

The wedge product of two *k*-forms is implemented as `%^%` or `wedge()`:

``` r
KF2 <- kform_general(6:9,2,1:6)
KF2
#>          val
#>  7 9  =    5
#>  8 9  =    6
#>  6 9  =    4
#>  7 8  =    3
#>  6 8  =    2
#>  6 7  =    1
KF %^% KF2
#>                val
#>  1 2 4 7 8  =   15
#>  2 3 4 7 9  =    5
#>  2 3 4 6 8  =    2
#>  2 3 4 6 7  =    1
#>  2 3 4 7 8  =    3
#>  1 2 4 6 7  =    5
#>  1 2 4 6 9  =   20
#>  2 3 4 8 9  =    6
#>  2 3 4 6 9  =    4
#>  1 2 4 8 9  =   30
#>  1 2 4 6 8  =   10
#>  1 2 4 7 9  =   25
```

The package can accommodate a number of results from the exterior calculus such as elementary forms:

``` r
dx <- as.kform(1)
dy <- as.kform(2)
dz <- as.kform(3)
dx %^% dy %^% dz  # element of volume 
#>            val
#>  1 2 3  =    1
```

A number of useful functions from the exterior calculus are provided, such as the gradient of a scalar function:

``` r
grad(1:6)
#>        val
#>  5  =    5
#>  6  =    6
#>  4  =    4
#>  3  =    3
#>  2  =    2
#>  1  =    1
```

The package takes the leg-work out of the exterior calculus:

``` r
grad(1:4) %^% grad(1:6)
#>          val
#>  2 6  =   12
#>  4 5  =   20
#>  3 5  =   15
#>  1 5  =    5
#>  3 6  =   18
#>  1 6  =    6
#>  4 6  =   24
#>  2 5  =   10
```
