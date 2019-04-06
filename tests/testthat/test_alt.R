## Some tests of Alt()

library("wedge")
library("testthat")

test_that("Function Alt() behaves itself", {

    foo <- function(terms,k,n){
        S <- rtensor(terms,k,n,rnorm(terms))
        
        error <- value(Alt(Alt(S)) - Alt(S)) # should be small
        print(error)
        expect_true(max(abs(error)) < 1e-8, info=S)
     } # foo() closes


    foo(4,5,6)
    foo(4,2,3)
    foo(4,1,6)
    foo(8,5,6)
    foo(4,5,6)
})

