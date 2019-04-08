## Some tests of Alt()

library("wedge")
library("testthat")

test_that("Function Alt() behaves itself", {

    foo <- function(terms,k,n){
        S <- rtensor(terms,k,n,rnorm(terms))
        
        discrepancy <- Alt(Alt(S)) - Alt(S)
        ## 'discrepancy should be zero in theory, but numerical
        ## roundoff means discrepancy might not be the zero tensor.

        if(!is.zero(discrepancy)){
          error <- value(discrepancy)
          expect_true(max(abs(error))<1e-8,info=dput(S))
        } else {
          expect_true(TRUE) 
        }
    } # foo() closes

    foo(4,5,6)
    foo(4,2,3)
    foo(4,1,6)
    foo(8,5,6)
    foo(4,5,6)
})
