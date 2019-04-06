library("wedge")
library("testthat")

test_that("Cross product Ops behave", {

    foo <- function(terms,k,n){
        S  <- rtensor(terms,k,n,sample(terms))
        S1 <- rtensor(terms,k,n,sample(terms))

        expect_true(S == +S,info=S)
        expect_true(S == -(-S),info=S)

        expect_true(S - S == S*0,info=S)
        expect_true(S + S == 2*S,info=S)
        expect_true(S + S == S*2,info=S)
        expect_true(S + S1 - S1 == S,info=list(S,S1))

        expect_true(S + S1 == S1 + S,info=list(S,S1))
        expect_true(S/2 + S/2 == S,info=S)
        
    }

    foo(3,4,5)
    foo(3,4,5)
    foo(3,4,5)
    foo(3,4,5)

})

      

test_that("Wedge product Ops behave", {

    foo <- function(terms,k,n){
        S  <- rform(terms,k,n,sample(terms))
        S1 <- rform(terms,k,n,sample(terms))

        expect_true(S == +S,info=S)
        expect_true(S == -(-S),info=S)

        expect_true(S - S == S*0,info=S)
        expect_true(S + S == 2*S,info=S)
        expect_true(S + S == S*2,info=S)
        expect_true(S + S1 - S1 == S,info=list(S,S1))

        expect_true(S + S1 == S1 + S,info=list(S,S1))
        expect_true(S/2 + S/2 == S,info=S)
        
    }

    foo(3,4,5)
    foo(3,4,5)
    foo(3,4,5)
    foo(3,4,5)

})

      
