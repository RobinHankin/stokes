## Some tests of transform()

options(warn=999)
test_that("Function transform() behaves itself", {
    expect_true(TRUE)

    foo <- function(x,M){  # checks that transform(transform(x,M),solve(M)) == x
        xt <- transform(transform(x,M),solve(M))
        discrepancy <- x |> transform(M) |> transform(solve(M)) - x
        expect_true(issmall(discrepancy),info=list(x,M))
    } # foo() closes

    for(i in 1:3){
        o <-  rform(terms=3,k=2,n=5,coeffs=rnorm(3))
        M <- matrix(rnorm(25),5,5)
        foo(o,M)
        foo(o*0,M)
    }



})
