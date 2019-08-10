## Some tests of contract()

options(warn=999)
test_that("Function contract() behaves itself", {

    foo <- function(o,V){  
        jj <- c(
            as.function(o)(V),
            as.function(contract(o,V[,1,drop=TRUE]))(V[,-1]), # scalar
            as.function(contract(o,V[,1:2]))(V[,-(1:2),drop=FALSE]),
            as.function(contract(o,V[,1:3]))(V[,-(1:3),drop=FALSE]),
            as.function(contract(o,V[,1:4]))(V[,-(1:4),drop=FALSE]),
            as.function(contract(o,V[,1:4],lose=FALSE))(V[,-(1:4),drop=FALSE])
        )
        
        expect_true(max(jj)-min(jj) < 1e-6)
    } # foo() closes


    for(i in 1:2){
        o <- rform(2,k=5,n=9,coeffs=runif(2))
        V <- matrix(rnorm(45),ncol=5)
        foo(o,V)
    }
})
