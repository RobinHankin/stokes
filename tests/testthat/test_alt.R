## Some tests of Alt()

test_that("Function Alt() behaves itself", {

    foo <- function(S){
        discrepancy <- Alt(Alt(S)) - Alt(S)
        ## discrepancy should be zero in theory, but numerical
        ## roundoff means discrepancy might not be the zero tensor.

        if(!is.zero(discrepancy)){
            error <- value(discrepancy)
            expect_true(max(abs(error))<1e-8,info=dput(S))
        } else {
            expect_true(TRUE) 
        }
    } # foo() closes
    
    
    for(i in 1:10){
        terms <- sample(2:4,1)
        k <- sample(3:4,1)
        n <- sample(2:4,1)
        S <- rtensor(terms,k,n,rnorm(terms))
        foo(S)
    }
})
