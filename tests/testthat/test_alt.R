## Some tests of Alt()

test_that("Function Alt() behaves itself", {

    foo <- function(S){ # idempotence of Alt()
        discrepancy <- Alt(Alt(S)) - Alt(S)
        ## discrepancy should be zero in theory, but numerical
        ## roundoff means discrepancy might not be the zero tensor.

        if(!is.zero(discrepancy)){
            error <- coeffs(discrepancy)
            expect_true(max(abs(error))<1e-8,info=dput(S))
        } else {
            expect_true(TRUE) 
        }
    } # foo() closes

    bar <- function(S,V){   # Alt() is in fact alternating
      S <- Alt(S)
      if(ncol(V)>=2){
        V1 <- V[,c(2,1,seq(from=3,to=ncol(V)))]
        expect_true(abs(as.function(S)(V) + as.function(S)(V1)) < 1e-6)
      }
    }

    for(i in 1:10){
        terms <- sample(2:4,1)
        k <- sample(3:4,1)
        n <- sample(2:4,1)
        S <- rtensor(terms,k,n,rnorm(terms))
        foo(S)
        V <- matrix(rnorm(k*n),ncol=k)
        bar(S,V)
    }

    # test that issue  #48 has been resolved:
    foo(as.ktensor(matrix(c(4,2,4,3,4,2,2,3,3),3,3),1:3))
    expect_silent(Alt(spray(rbind(1:3,2:4),1:2),give_kform=TRUE))

})
