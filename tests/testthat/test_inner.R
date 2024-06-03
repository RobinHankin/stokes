## Some tests of inner()

options(warn=999)
test_that("Function inner() behaves itself", {
    expect_true(TRUE)

    foo1 <- function(x){
        LHS <- as.function(inner(diag(length(x))))(cbind(x,x))
        RHS <- sum(x^2)
        expect_true(abs(LHS-RHS) < 1e-6)
    }
    
    foo2 <- function(M,x){  # positive-definiteness
        g <- as.function(inner(crossprod(M)))
        expect_true(g(kronecker(x,t(c(1,1))))>=0)
        }

    foo3 <- function(M,x,y){  # checks that inner(M) == function(x){x^T M x}
        LHS <- as.function(inner(M))(cbind(x,y))
        RHS <- drop(tcrossprod(y,crossprod(x,M)))
        ## RHS should be quadform::quad3.form(M,x,y)

        expect_true(abs(LHS-RHS) < 1e-6)
    } # foo() closes

    for(i in 1:3){
        M <- matrix(rnorm(25),5,5)
        x <- rnorm(5)
        y <- rnorm(5)
        foo1(x)
        foo2(M,x)
        foo3(M,x,y)
    }
})
