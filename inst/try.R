## Hubbard and Hubbard, 6.7.18 (pp629-630)

library(wedge)
library(Deriv)

f <- function(x){
    n <- length(x)
    as.kform(t(apply(diag(n)<1,2,which)))
}

df <- function(x){
    n <- length(x)
    S <- sum(x^2)
    grad(rep(c(1,-1),length=n)*
    (S^(n/2) - n*x^2*S^(n/2-1))/S^n
    )
}

x <- rnorm(9)
print(df(x) %^% f(x))  # should be zero

dw <- as.kform(1)
dx <- as.kform(2)
dy <- as.kform(3)
dz <- as.kform(4)

f1 <- function(w,x,y,z){w*x*y*z + sin(x) + cos(w)}
f2 <- function(w,x,y,z){w^2*x*y*z + sin(w) + w+z}
f3 <- function(w,x,y,z){w*y + x + (w*z)}


## I want a 2-form which is the sum of three elementary two-forms
phi <-
  (
    +f1(1,2,3,4) %^% dw %^% dx
    +f2(1,2,3,4) %^% dw %^% dy
    +f3(1,2,3,4) %^% dy %^% dz
  )

## We can use slightly slicker R idiom by defining elementary forms
## e1,e2,e3 and then defining phi to be a linear sum, weighted with
## 0-forms given by (scalar) functions f1(), f2() and f3():

e1 <- dw %^% dx
e2 <- dw %^% dy
e3 <- dy %^% dz

phi <-
  (
    +f1(1,2,3,4) %^% e1
    +f2(1,2,3,4) %^% e2
    +f3(1,2,3,4) %^% e3
  )


# Evaluate first derivatives of f1() etc at point (1,2,3,4), using Deriv():
Df1 <- Deriv(f1)(1,2,3,4)
Df2 <- Deriv(f2)(1,2,3,4)
Df3 <- Deriv(f3)(1,2,3,4)
# Df1 etc are numeric vectors of length 4


##Calculating dphi is easy:
dphi <-
  (
    +grad(Df1) %^% e1
    +grad(Df2) %^% e2
    +grad(Df3) %^% e3
  )


## Now work on the differential of the differential.  First evaluate
## the Hessians (4x4 numeric matrices) at the same point:

Hf1 <- matrix(Deriv(f1,nderiv=2)(1,2,3,4),4,4)
Hf2 <- matrix(Deriv(f2,nderiv=2)(1,2,3,4),4,4)
Hf3 <- matrix(Deriv(f3,nderiv=2)(1,2,3,4),4,4)



## ddphi is clearly zero as the Hessians are symmetrical:
ddphi <- # should be zero
  (  
    +as.kform(which(!is.na(Hf1),arr.ind=TRUE),c(Hf1))
    +as.kform(which(!is.na(Hf2),arr.ind=TRUE),c(Hf2))
    +as.kform(which(!is.na(Hf3),arr.ind=TRUE),c(Hf3))
  )
