## Some tests of hodge()

library("wedge")
library("testthat")
library("magrittr")

options(warn=999)
test_that("Function hodge() behaves itself", {
    expect_true(TRUE)

    foo1 <- function(x){

        n <- max(index(x))
        discrepancy <- x %>% hodge(n) %>% hodge(n) %>% hodge(n) %>% hodge(n) -x
        if(!is.zero(discrepancy)){
            error <- value(discrepancy)
            expect_true(max(abs(error))<1e-8, info=list(x,discrepancy))
        } else {  # test successful
            expect_true(TRUE) 
        }
        expect_true(value(x %^% hodge(x,n)) >= 0)
    } # foo1() closes


    foo2 <- function(x,y){
        n <- max(c(index(x),index(y)))
        discrepancy <- hodge(x,n) %^% y - hodge(y,n) %^% x
        if(!is.zero(discrepancy)){
            error <- value(discrepancy)
            expect_true(max(abs(error))<1e-8, info=list(x,discrepancy))
        } else {
            expect_true(TRUE) 
        }
    }  # foo2() closes


    for(i in 1:10){
      jj <- rform()
      foo1(jj)
    }

    for(i in 1:10){
      x <- rform()
      y <- rform()
      foo2(x,y)
      x <- rform(10,5,11)
      y <- rform(10,5,11)
      foo2(x,y)
    }

})
