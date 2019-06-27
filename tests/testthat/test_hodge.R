## Some tests of hodge()

options(warn=999)
test_that("Function hodge() behaves itself", {
    expect_true(TRUE)

    foo1 <- function(x){  # checks that ***x == x, also positivity

        n <- max(index(x))
        discrepancy <- x %>% hodge(n) %>% hodge(n) %>% hodge(n) %>% hodge(n) - x
        expect_true(issmall(discrepancy),info=x)
        expect_true(value(x %^% hodge(x,n)) >= 0)
    } # foo1() closes


    foo2 <- function(x,y){  # checks that *x^y == *y^x
        n <- max(c(index(x),index(y)))
        expect_true(issmall(hodge(x,n) %^% y - hodge(y,n) %^% x),info=list(x,y,n))
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
