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
        } else {
            expect_true(TRUE) 
        }
        expect_true(value(x %^% hodge(x,n)) >= 0)
    } # foo1() closes


    foo2 <- function(x,y){
        discrepancy <- hodge(x) %^% y - hodge(y) %^% x
        if(!is.zero(discrepancy)){
            error <- value(discrepancy)
            expect_true(max(abs(error))<1e-8, info=list(x,discrepancy))
        } else {
            expect_true(TRUE) 
        }
    }  # foo2() closes

    foo1(rform())
    foo1(rform(10,5,11))
    foo1(rform(10,5,11))
    foo1(rform(10,5,12))

    foo2(rform(),rform())
    foo2(rform(10,5,11),rform(10,5,11))
    foo2(rform(10,2,7),rform(5,3,8))
})
