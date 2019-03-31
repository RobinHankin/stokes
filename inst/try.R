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
f4 <- function(w,x,y,z){sin(y+z) + x + (w*z)}
f5 <- function(w,x,y,z){cos(w*z)}
f6 <- function(w,x,y,z){x*y*z}

phi <- (
    +f1(1,2,3,4) %^% dw %^% dx
    +f2(1,2,3,4) %^% dw %^% dy
    +f3(1,2,3,4) %^% dy %^% dz

)
# Df1 etc are numeric vectors corresponding to the first derivatives of f1() etc evaluated at point (1,2,3,4):
Df1 <-    Deriv(f1)(1,2,3,4)
Df2 <-    Deriv(f2)(1,2,3,4)
Df3 <-    Deriv(f3)(1,2,3,4)
Df4 <-    Deriv(f4)(1,2,3,4)  


##Calculating dphi is easy:
dphi <- (
    +grad(Df1) %^% dw %^% dx
    +grad(Df2) %^% dw %^% dy
    +grad(Df3) %^% dy %^% dz
)

## Now the Hessians at the same point:
Hf1 <- matrix(Deriv(f1,nderiv=2)(1,2,3,4),4,4)
Hf2 <- matrix(Deriv(f2,nderiv=2)(1,2,3,4),4,4)
Hf3 <- matrix(Deriv(f3,nderiv=2)(1,2,3,4),4,4)
Hf4 <- matrix(Deriv(f4,nderiv=2)(1,2,3,4),4,4)


## ddphi is clearly zero as the Hessians are symmetrical:
ddphi <- (  # should be zero
    +as.kform(which(Hf1 < Inf,arr.ind=TRUE),c(Hf1))
    +as.kform(which(Hf2 < Inf,arr.ind=TRUE),c(Hf2))
    +as.kform(which(Hf3 < Inf,arr.ind=TRUE),c(Hf3))
)
