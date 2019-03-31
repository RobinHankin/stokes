## Hubbard and Hubbard, 6.7.18 (pp629-630)

library(wedge)
as.kform(t(apply(blockparts(rep(1,5),4)>0,2,which)))

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


dw <- function(x){
    df(x) %^% f(x)
}


    
