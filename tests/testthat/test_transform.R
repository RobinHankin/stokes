## Some tests of transform()

library("wedge")
library("testthat")
library("magrittr")

`issmall` <- function(discrepancy){  # tests for a kform being either zero or "small"
  if(!is.zero(discrepancy)){
    error <- value(discrepancy)
    expect_true(max(abs(error)) < 1e-8, info=list(x,discrepancy))
  } else {  # test successful
    expect_true(TRUE)
  }
}  # issmall() closes


options(warn=999)
test_that("Function transform() behaves itself", {
    expect_true(TRUE)

    foo <- function(x,M){  # checks that transform(transform(x,M),solve(M)) == x
        xt <- transform(transform(x,M),solve(M))
        discrepancy <- x %>% transform(M) %>% transform(solve(M)) - x
        issmall(discrepancy)
    } # foo() closes

    for(i in 1:3){
        o <-  rform(terms=3,k=2,n=5,coeffs=rnorm(3))
        M <- matrix(rnorm(25),5,5)
        foo(o,M)
    }
})
